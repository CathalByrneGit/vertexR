#' Build a System Graph from an Ontology Bundle
#'
#' Materializes object instances as nodes and link instances as edges from
#' an ontologySpecR bundle and a DBI database connection. Returns a
#' [tidygraph::tbl_graph] where each node has `.object_type`, `.node_id`,
#' and all property columns from the source table, and each edge has
#' `.link_type` plus the join key columns.
#'
#' @param bundle An ontologySpecR bundle object.
#' @param connection A DBI connection to the database containing instance data.
#' @param object_types Character vector of object type IDs to include,
#'   or `NULL` for all object types in the bundle.
#' @param link_types Character vector of link type IDs to include,
#'   or `NULL` for all link types in the bundle.
#' @param node_properties Logical; if `TRUE` (default), include all property
#'   columns from the source table on each node. If `FALSE`, only include
#'   `.object_type` and `.node_id`.
#'
#' @return A `tbl_graph` object.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ontologySpecR)
#' b <- read_bundle("aviation-demo.json")
#' con <- DBI::dbConnect(duckdb::duckdb())
#' g <- vertex_graph(b, con)
#' vx_nodes(g)
#' vx_edges(g)
#' }
vertex_graph <- function(bundle, connection,
                         object_types = NULL,
                         link_types = NULL,
                         node_properties = TRUE) {

  # Validate inputs
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  # Get object and link type definitions from bundle
  all_objects <- get_bundle_objects(bundle)
  all_links   <- get_bundle_links(bundle)

  # Filter object types if specified
  if (!is.null(object_types)) {
    all_objects <- Filter(function(o) o$id %in% object_types, all_objects)
    if (length(all_objects) == 0L) {
      abort("None of the specified object_types were found in the bundle.")
    }
  }

  # Filter link types if specified
  if (!is.null(link_types)) {
    all_links <- Filter(function(l) l$id %in% link_types, all_links)
  }

  # Also filter links to only those whose from/to reference included objects

  included_obj_ids <- vapply(all_objects, function(o) o$id, character(1))
  all_links <- Filter(function(l) {
    l$from %in% included_obj_ids && l$to %in% included_obj_ids
  }, all_links)

  # --- Build node table ---
  node_frames <- list()
  for (obj in all_objects) {
    tbl_name <- get_source_table(obj)
    df <- read_table(connection, tbl_name)
    if (is.null(df) || nrow(df) == 0L) next

    pk_cols <- get_pk_columns(obj)
    node_id <- make_node_ids(df, pk_cols)

    if (node_properties) {
      node_df <- df
    } else {
      node_df <- data.frame(row.names = seq_len(nrow(df)))
    }
    node_df$.object_type <- obj$id
    node_df$.node_id     <- node_id
    node_frames[[length(node_frames) + 1L]] <- node_df
  }

  if (length(node_frames) == 0L) {
    abort("No nodes could be materialized. Check that source tables exist.")
  }

  # Combine node frames, filling missing columns with NA
  nodes <- dplyr::bind_rows(node_frames)

  # Build a lookup: .node_id -> row index
  node_index <- stats::setNames(seq_len(nrow(nodes)), nodes$.node_id)

  # --- Build edge table ---
  edge_frames <- list()
  for (lnk in all_links) {
    join_info <- lnk$join
    if (is.null(join_info)) next

    from_keys <- as.character(join_info$fromKeys)
    to_keys   <- as.character(join_info$toKeys)
    if (length(from_keys) == 0L || length(to_keys) == 0L) next

    # Find the source object types
    from_obj <- find_object_type(bundle, lnk$from)
    to_obj   <- find_object_type(bundle, lnk$to)
    if (is.null(from_obj) || is.null(to_obj)) next

    from_table <- get_source_table(from_obj)
    to_table   <- get_source_table(to_obj)

    from_df <- read_table(connection, from_table)
    to_df   <- read_table(connection, to_table)
    if (is.null(from_df) || is.null(to_df)) next

    from_pk <- get_pk_columns(from_obj)
    to_pk   <- get_pk_columns(to_obj)

    # For each row in the "from" table, the from_keys columns contain
    # the values that match the to_keys columns in the "to" table.
    # We need to find the node IDs for both sides and create edges.
    from_node_ids <- make_node_ids(from_df, from_pk)

    # Build edge rows by matching from_keys in from_df to to_keys in to_df
    # Create a lookup from to_keys -> to_node_id
    to_node_ids <- make_node_ids(to_df, to_pk)

    # Create composite key for the "to" side
    if (length(to_keys) == 1L) {
      to_key_vals <- as.character(to_df[[to_keys]])
    } else {
      to_key_vals <- do.call(paste, c(
        lapply(to_keys, function(col) as.character(to_df[[col]])),
        list(sep = ":")
      ))
    }
    to_lookup <- stats::setNames(to_node_ids, to_key_vals)

    # Create composite key for the "from" side join columns
    if (length(from_keys) == 1L) {
      from_join_vals <- as.character(from_df[[from_keys]])
    } else {
      from_join_vals <- do.call(paste, c(
        lapply(from_keys, function(col) as.character(from_df[[col]])),
        list(sep = ":")
      ))
    }

    # Match
    matched_to_node_ids <- to_lookup[from_join_vals]
    valid <- !is.na(matched_to_node_ids)

    if (sum(valid) == 0L) next

    edge_df <- data.frame(
      from = unname(node_index[from_node_ids[valid]]),
      to   = unname(node_index[as.character(matched_to_node_ids[valid])]),
      .link_type = lnk$id,
      stringsAsFactors = FALSE
    )

    # Add join key columns for reference
    for (fk in from_keys) {
      if (fk %in% names(from_df)) {
        edge_df[[fk]] <- from_df[[fk]][valid]
      }
    }

    # Filter out edges where from or to index is NA (node not in graph)
    edge_df <- edge_df[!is.na(edge_df$from) & !is.na(edge_df$to), ]

    if (nrow(edge_df) > 0L) {
      edge_frames[[length(edge_frames) + 1L]] <- edge_df
    }
  }

  if (length(edge_frames) > 0L) {
    edges <- dplyr::bind_rows(edge_frames)
  } else {
    edges <- data.frame(from = integer(0), to = integer(0),
                        .link_type = character(0),
                        stringsAsFactors = FALSE)
  }

  # Build tbl_graph
  g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  g
}


#' Get Node Data from a Vertex Graph
#'
#' Returns the node data as a tibble.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @return A tibble of node data.
#' @export
vx_nodes <- function(g) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  g_activated <- tidygraph::activate(g, nodes)
  dplyr::as_tibble(g_activated)
}


#' Get Edge Data from a Vertex Graph
#'
#' Returns the edge data as a tibble.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @return A tibble of edge data.
#' @export
vx_edges <- function(g) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  g_activated <- tidygraph::activate(g, edges)
  dplyr::as_tibble(g_activated)
}
