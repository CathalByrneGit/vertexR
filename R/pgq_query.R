#' Execute a SQL/PGQ MATCH Pattern Query
#'
#' The lowest-level DuckPGQ query function. Executes a raw MATCH pattern
#' and returns results as a data frame.
#'
#' @param connection A DBI connection with DuckPGQ loaded.
#' @param graph_name Character; the property graph name.
#' @param pattern Character; the MATCH clause body, e.g.
#'   `"(o:Officer)-[:OfficerOf]->(e:Entity)"`.
#' @param columns Character vector; the COLUMNS clause body, e.g.
#'   `c("o.node_id AS officer_id", "e.name AS entity_name")`.
#' @param where Character or NULL; additional WHERE predicate.
#' @param limit Integer or NULL; row limit.
#'
#' @return A data.frame.
#' @export
vx_pgq_match <- function(connection, graph_name, pattern, columns,
                         where = NULL, limit = NULL) {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  cols_sql <- paste(columns, collapse = ",\n      ")
  where_sql <- if (!is.null(where)) paste("WHERE", where) else ""
  limit_sql <- if (!is.null(limit)) paste("LIMIT", as.integer(limit)) else ""

  sql <- paste0(
    "FROM GRAPH_TABLE (", graph_name, "\n",
    "  MATCH ", pattern, "\n",
    "  ", where_sql, "\n",
    "  COLUMNS (\n",
    "      ", cols_sql, "\n",
    "  )\n",
    ")\n",
    limit_sql
  )

  tryCatch(
    DBI::dbGetQuery(connection, sql),
    error = function(e) {
      abort(paste0("DuckPGQ query failed: ", conditionMessage(e),
                   "\nSQL: ", sql))
    }
  )
}


#' Find k-Hop Neighbors of Specified Nodes
#'
#' Uses DuckPGQ to find all nodes reachable from a set of source nodes
#' within k hops. This executes inside DuckDB without materializing the
#' entire graph into R memory.
#'
#' @param connection DBI connection with DuckPGQ loaded.
#' @param graph_name Character; the property graph name.
#' @param source_type Character; object type label of starting nodes.
#' @param source_ids Character vector; primary key values to start from.
#' @param link_type Character; link type label to traverse.
#' @param target_type Character; object type label of destination nodes.
#' @param depth Integer or integer vector of length 2; number of hops.
#'   Use `depth = 2` for exactly 2 hops, or `depth = c(1, 3)` for
#'   between 1 and 3 hops.
#' @param direction One of `"forward"`, `"reverse"`, or `"both"`.
#'   Default `"forward"`.
#' @param source_pk Character; PK column name on source type.
#'   Default `"node_id"`.
#' @param target_pk Character; PK column name on target type.
#'   Default `"node_id"`.
#'
#' @return A data.frame with columns: `source_id`, `target_id`, `target_name`.
#' @export
vx_pgq_neighbors <- function(connection, graph_name,
                             source_type, source_ids, link_type, target_type,
                             depth = 1L,
                             direction = c("forward", "reverse", "both"),
                             source_pk = "node_id", target_pk = "node_id") {
  direction <- match.arg(direction)

  # Build depth quantifier
  depth_q <- if (length(depth) == 1L) {
    if (depth == 1L) "" else paste0("{", depth, "}")
  } else {
    paste0("{", depth[1], ",", depth[2], "}")
  }

  # Build edge pattern based on direction
  edge_pattern <- switch(direction,
    forward = paste0("-[:", link_type, "]", depth_q, "->"),
    reverse = paste0("<-[:", link_type, "]", depth_q, "-"),
    both = paste0("-[:", link_type, "]", depth_q, "-")
  )

  # Escape and format source IDs
  ids_sql <- paste0("'", gsub("'", "''", source_ids), "'", collapse = ", ")

  vx_pgq_match(
    connection, graph_name,
    pattern = paste0("(s:", source_type, ")", edge_pattern, "(t:", target_type, ")"),
    columns = c(
      paste0("s.", source_pk, " AS source_id"),
      paste0("t.", target_pk, " AS target_id"),
      "t.name AS target_name"
    ),
    where = paste0("s.", source_pk, " IN (", ids_sql, ")")
  )
}


#' Find the Shortest Path Between Two Nodes
#'
#' Uses DuckPGQ's ANY SHORTEST path syntax to find the shortest path
#' between two nodes.
#'
#' @param connection DBI connection with DuckPGQ loaded.
#' @param graph_name Character; the property graph name.
#' @param from_type Character; source object type label.
#' @param from_id Character; source node primary key value.
#' @param to_type Character; target object type label.
#' @param to_id Character; target node primary key value.
#' @param link_types Character vector or NULL; link types to traverse.
#'   NULL means all link types.
#' @param max_hops Integer; maximum path length. Default 10.
#' @param from_pk Character; PK column on source type. Default `"node_id"`.
#' @param to_pk Character; PK column on target type. Default `"node_id"`.
#'
#' @return A data.frame with path information, or NULL if no path exists.
#' @export
vx_pgq_shortest_path <- function(connection, graph_name,
                                 from_type, from_id,
                                 to_type, to_id,
                                 link_types = NULL,
                                 max_hops = 10L,
                                 from_pk = "node_id",
                                 to_pk = "node_id") {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  # Build edge pattern
  if (is.null(link_types)) {
    edge_pattern <- paste0("-[e]-{1,", max_hops, "}>")
  } else {
    types <- paste(link_types, collapse = "|")
    edge_pattern <- paste0("-[e:", types, "]-{1,", max_hops, "}>")
  }

  # Escape IDs
  from_id_esc <- gsub("'", "''", from_id)
  to_id_esc <- gsub("'", "''", to_id)

  sql <- paste0(
    "FROM GRAPH_TABLE (", graph_name, "\n",
    "  MATCH ANY SHORTEST (s:", from_type, ")", edge_pattern, "(t:", to_type, ")\n",
    "  WHERE s.", from_pk, " = '", from_id_esc, "' AND t.", to_pk, " = '", to_id_esc, "'\n",
    "  COLUMNS (\n",
    "    s.", from_pk, " AS start_id,\n",
    "    t.", to_pk, " AS end_id,\n",
    "    path_length(e) AS hops\n",
    "  )\n",
    ")"
  )

  result <- tryCatch(
    DBI::dbGetQuery(connection, sql),
    error = function(e) {
      warn(paste0("Path query failed: ", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0L) NULL else result
}
