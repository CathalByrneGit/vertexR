#' Check if DuckPGQ is Available
#'
#' Tests whether the DuckPGQ extension can be loaded in a DuckDB connection.
#'
#' @param connection A DBI connection to DuckDB.
#' @return Logical; `TRUE` if DuckPGQ is available, `FALSE` otherwise.
#' @export
vx_pgq_available <- function(connection) {
  if (!inherits(connection, "DBIConnection")) {
    return(FALSE)
  }

  tryCatch({
    DBI::dbExecute(connection, "LOAD duckpgq")
    TRUE
  }, error = function(e) FALSE)
}


#' Get the DuckPGQ Graph Name from a Bundle
#'
#' Converts a bundle ID to a valid DuckPGQ graph name by replacing
#' hyphens with underscores.
#'
#' @param bundle An ontologySpecR bundle.
#' @return Character; the graph name.
#' @export
vx_pgq_graph_name <- function(bundle) {
 bundle_id <- bundle$bundleId %||% bundle$bundle_id %||% "graph"
  gsub("-", "_", bundle_id)
}


#' Set Up DuckPGQ Property Graph from a Bundle
#'
#' Installs and loads the DuckPGQ extension, then creates a property graph
#' from an ontologySpecR bundle. The graph can then be queried using
#' `vx_pgq_match()`, `vx_pgq_neighbors()`, etc.
#'
#' @param bundle An ontologySpecR bundle with `source_table` set on object
#'   and link types.
#' @param connection A DuckDB DBI connection.
#' @param graph_name Character or NULL. Defaults to bundle ID.
#' @param vertex_tables Character vector or NULL. Subset of object type IDs
#'   to include as vertex tables. NULL means all.
#' @param edge_tables Character vector or NULL. Subset of link type IDs
#'   to include as edge tables. NULL means all.
#' @return The connection invisibly (for piping).
#' @export
vx_pgq_setup <- function(bundle, connection,
                         graph_name = NULL,
                         vertex_tables = NULL,
                         edge_tables = NULL) {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
 }

  # Install + load DuckPGQ
  tryCatch(
    DBI::dbExecute(connection, "INSTALL duckpgq FROM community"),
    error = function(e) {
      if (!grepl("already installed", conditionMessage(e), ignore.case = TRUE)) {
        inform("DuckPGQ extension already installed or install skipped.")
      }
    }
  )
  DBI::dbExecute(connection, "LOAD duckpgq")

  # Determine graph name
  gname <- graph_name %||% vx_pgq_graph_name(bundle)

  # Generate DDL from bundle
  ddl <- .build_pgq_ddl(bundle, gname, vertex_tables, edge_tables)

  # Execute DDL
  DBI::dbExecute(connection, ddl)

  inform(paste0("Property graph '", gname, "' created."))
  invisible(connection)
}


#' Build DuckPGQ CREATE PROPERTY GRAPH DDL
#'
#' @param bundle An ontologySpecR bundle.
#' @param graph_name Character.
#' @param vertex_tables Character vector or NULL.
#' @param edge_tables Character vector or NULL.
#' @return Character; the DDL statement.
#' @keywords internal
.build_pgq_ddl <- function(bundle, graph_name, vertex_tables, edge_tables) {
  objects <- get_bundle_objects(bundle)
  links <- get_bundle_links(bundle)

  # Filter vertex tables if specified
  if (!is.null(vertex_tables)) {
    objects <- Filter(function(o) o$id %in% vertex_tables, objects)
  }

  # Filter edge tables if specified
  if (!is.null(edge_tables)) {
    links <- Filter(function(l) l$id %in% edge_tables, links)
  }

  # Build VERTEX TABLE clauses
  vertex_clauses <- vapply(objects, function(obj) {
    tbl <- get_source_table(obj)
    pk <- get_pk_columns(obj)
    pk_col <- pk[1]  # Use first PK column
    label <- obj$id
    paste0("  ", tbl, " PROPERTIES (", pk_col, " AS node_id) LABEL ", label)
  }, character(1))

  # Build EDGE TABLE clauses
  edge_clauses <- character(0)
  for (lnk in links) {
    from_obj <- find_object_type(bundle, lnk$from)
    to_obj <- find_object_type(bundle, lnk$to)
    if (is.null(from_obj) || is.null(to_obj)) next

    join_info <- lnk$join
    if (is.null(join_info)) next

    from_keys <- as.character(join_info$fromKeys)
    to_keys <- as.character(join_info$toKeys)
    if (length(from_keys) == 0L || length(to_keys) == 0L) next

    from_tbl <- get_source_table(from_obj)
    to_tbl <- get_source_table(to_obj)
    from_pk <- get_pk_columns(from_obj)[1]
    to_pk <- get_pk_columns(to_obj)[1]

    # Edge comes FROM the "from" object type TO the "to" object type
    # The join keys in the "from" table reference the "to" table's PK
    edge_clause <- paste0(
      "  ", from_tbl,
      " SOURCE KEY (", from_pk, ") REFERENCES ", from_tbl, " (", from_pk, ")",
      " DESTINATION KEY (", from_keys[1], ") REFERENCES ", to_tbl, " (", to_pk, ")",
      " LABEL ", lnk$id
    )
    edge_clauses <- c(edge_clauses, edge_clause)
  }

  # Combine into CREATE PROPERTY GRAPH statement
  paste0(
    "CREATE OR REPLACE PROPERTY GRAPH ", graph_name, " (\n",
    "VERTEX TABLES (\n",
    paste(vertex_clauses, collapse = ",\n"),
    "\n),\n",
    "EDGE TABLES (\n",
    paste(edge_clauses, collapse = ",\n"),
    "\n)\n",
    ")"
  )
}
