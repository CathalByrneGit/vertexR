#' Extract a Subgraph Around Seed Nodes as a tbl_graph
#'
#' Runs a DuckPGQ k-hop expansion from seed nodes, then materializes the
#' result into an igraph/tidygraph object for algorithm-heavy work
#' (scenario analysis, risk propagation, optimization).
#'
#' This is the bridge between the DuckPGQ large-graph backend and the
#' in-memory igraph algorithms.
#'
#' @param connection DBI connection with DuckPGQ loaded.
#' @param bundle An ontologySpecR bundle.
#' @param graph_name Character; the property graph name.
#' @param seed_ids Character vector; starting node IDs.
#' @param seed_type Character; object type of seed nodes.
#' @param depth Integer; hops to expand. Default 2.
#' @param link_types Character vector or NULL; which link types to traverse.
#'   NULL means all.
#' @param direction One of `"forward"`, `"reverse"`, or `"both"`.
#'   Default `"both"`.
#'
#' @return A `tbl_graph` suitable for `vx_scenario()`, `vx_propagate_risk()`, etc.
#' @export
vx_pgq_subgraph <- function(connection, bundle, graph_name,
                            seed_ids, seed_type,
                            depth = 2L,
                            link_types = NULL,
                            direction = "both") {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  objects <- get_bundle_objects(bundle)
  links <- get_bundle_links(bundle)

  # Collect all reachable node IDs via DuckPGQ queries
  all_node_ids <- seed_ids

  # If no link_types specified, use all
  if (is.null(link_types)) {
    link_types <- vapply(links, function(l) l$id, character(1))
  }

  # For each link type, find neighbors
  for (ltype in link_types) {
    lnk <- find_link_type(bundle, ltype)
    if (is.null(lnk)) next

    # Try forward direction: seed_type -> target
    if (seed_type == lnk$from) {
      tryCatch({
        nbrs <- vx_pgq_neighbors(
          connection, graph_name,
          source_type = seed_type,
          source_ids = seed_ids,
          link_type = ltype,
          target_type = lnk$to,
          depth = depth,
          direction = "forward"
        )
        if (!is.null(nbrs) && nrow(nbrs) > 0L) {
          all_node_ids <- unique(c(all_node_ids, nbrs$target_id))
        }
      }, error = function(e) NULL)
    }

    # Try reverse direction: target -> seed_type
    if (seed_type == lnk$to) {
      tryCatch({
        nbrs <- vx_pgq_neighbors(
          connection, graph_name,
          source_type = seed_type,
          source_ids = seed_ids,
          link_type = ltype,
          target_type = lnk$from,
          depth = depth,
          direction = "reverse"
        )
        if (!is.null(nbrs) && nrow(nbrs) > 0L) {
          all_node_ids <- unique(c(all_node_ids, nbrs$target_id))
        }
      }, error = function(e) NULL)
    }
  }

  # Build filter_sql to only fetch these node IDs
  filter_sql <- list()
  for (obj in objects) {
    tbl_name <- get_source_table(obj)
    pk_col <- get_pk_columns(obj)[1]

    # Filter to only include node IDs in our subgraph
    ids_sql <- paste0("'", gsub("'", "''", all_node_ids), "'", collapse = ", ")
    filter_sql[[tbl_name]] <- paste0(pk_col, " IN (", ids_sql, ")")
  }

  # Use vertex_graph with filter_sql to materialize
  vertex_graph(
    bundle = bundle,
    connection = connection,
    link_types = link_types,
    filter_sql = filter_sql,
    max_nodes = Inf  # We already filtered, so no guard needed
  )
}


#' Find Weakly Connected Components Using DuckPGQ
#'
#' Uses recursive SQL/PGQ path expansion to find connected components
#' without pulling the full graph into igraph. Suitable for very large
#' datasets.
#'
#' @param connection DBI connection with DuckPGQ loaded.
#' @param graph_name Character; the property graph name.
#' @param vertex_type Character; object type label to compute components for.
#' @param link_type Character; link type to follow.
#'
#' @return A data.frame with columns: `node_id`, `component_id`, `component_size`.
#' @export
vx_pgq_components <- function(connection, graph_name, vertex_type, link_type) {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  # Use pointer-chasing approach: component ID = minimum reachable node_id
  sql <- paste0("
    WITH RECURSIVE reachable AS (
      -- Base case: each node can reach itself
      FROM GRAPH_TABLE (", graph_name, "
        MATCH (a:", vertex_type, ")
        COLUMNS (a.node_id AS from_id, a.node_id AS to_id)
      )
      UNION
      -- Recursive case: follow edges
      SELECT r.from_id, g.to_id
      FROM reachable r
      JOIN (
        FROM GRAPH_TABLE (", graph_name, "
          MATCH (a:", vertex_type, ")-[:", link_type, "]-(b:", vertex_type, ")
          COLUMNS (a.node_id AS from_id, b.node_id AS to_id)
        )
      ) g ON r.to_id = g.from_id
      WHERE r.to_id <> g.to_id
    ),
    components AS (
      SELECT from_id AS node_id, MIN(to_id) AS component_id
      FROM reachable
      GROUP BY from_id
    )
    SELECT
      c.node_id,
      c.component_id,
      COUNT(*) OVER (PARTITION BY c.component_id) AS component_size
    FROM components c
    ORDER BY component_size DESC, component_id, node_id
  ")

  tryCatch(
    DBI::dbGetQuery(connection, sql),
    error = function(e) {
      # Fallback to simpler non-recursive approach if recursive CTE fails
      warn(paste0("Recursive component query failed, using simple approach: ",
                  conditionMessage(e)))
      .pgq_components_simple(connection, graph_name, vertex_type)
    }
  )
}


#' Simple component detection fallback
#' @keywords internal
.pgq_components_simple <- function(connection, graph_name, vertex_type) {
  # Just return all nodes with unique component IDs (no connectivity analysis)
  sql <- paste0("
    FROM GRAPH_TABLE (", graph_name, "
      MATCH (a:", vertex_type, ")
      COLUMNS (a.node_id AS node_id)
    )
  ")
  nodes <- DBI::dbGetQuery(connection, sql)
  nodes$component_id <- seq_len(nrow(nodes))
  nodes$component_size <- 1L
  nodes
}
