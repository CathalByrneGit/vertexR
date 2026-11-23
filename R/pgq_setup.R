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
  bundle <- as_vx_bundle(bundle)
  bundle_id <- ontologySpecR::bundle_id(bundle) %||% "graph"
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
#' @details
#' Object types backed by a query (`source$kind = "query"`) are exposed to
#' DuckPGQ through views named `<graph_name>__<type_id>`, created with
#' `CREATE OR REPLACE VIEW` before the property graph. Setup therefore
#' creates (or replaces) these views in the connected database.
#'
#' Composite primary keys are not supported by the PGQ backend and abort.
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
  bundle <- as_vx_bundle(bundle)
  gname <- graph_name %||% vx_pgq_graph_name(bundle)

  # Query-backed object types need a view: CREATE PROPERTY GRAPH only
  # accepts tables and views.
  for (view_sql in .pgq_view_sql(bundle, gname)) {
    DBI::dbExecute(connection, view_sql)
  }

  # Generate DDL from bundle
  ddl <- .build_pgq_ddl(bundle, gname, vertex_tables, edge_tables)

  # Execute DDL
  DBI::dbExecute(connection, ddl)

  inform(paste0("Property graph '", gname, "' created."))
  invisible(connection)
}


#' Relation name DuckPGQ should use for an object type
#'
#' The source table (schema-qualified when the type has a schema), or `<graph_name>__<id>` (a view) for query-backed types.
#'
#' @keywords internal
.pgq_relation <- function(obj, graph_name) {
  if (is_query_object(obj)) {
    paste0(graph_name, "__", obj$id)
  } else {
    id <- ontologySpecR::object_table_id(obj)
    if (is.null(id$schema)) id$table else paste0(id$schema, ".", id$table)
  }
}

#' CREATE VIEW statements for query-backed object types
#'
#' @return Character vector (possibly empty) of SQL statements
#' @keywords internal
.pgq_view_sql <- function(bundle, graph_name) {
  objs <- Filter(is_query_object, get_bundle_objects(as_vx_bundle(bundle)))
  vapply(objs, function(o) {
    paste0("CREATE OR REPLACE VIEW ", .pgq_relation(o, graph_name), " AS ",
           ontologySpecR::object_source_sql(o))
  }, character(1))
}

#' Primary key column for PGQ, aborting on composite keys
#'
#' @keywords internal
.pgq_pk <- function(obj) {
  pk <- get_pk_columns(obj)
  if (length(pk) != 1L) {
    abort(paste0("Object type '", obj$id, "' has a composite primary key (",
                 paste(pk, collapse = ", "),
                 "); the DuckPGQ backend supports single-column keys only."))
  }
  pk
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
  bundle <- as_vx_bundle(bundle)
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
    tbl <- .pgq_relation(obj, graph_name)
    pk_col <- .pgq_pk(obj)
    label <- obj$id
    paste0("  ", tbl, " PROPERTIES (", pk_col, " AS node_id) LABEL ", label)
  }, character(1))

  # Build EDGE TABLE clauses
  edge_clauses <- character(0)
  no_join <- character(0)
  for (lnk in links) {
    ep <- ontologySpecR::link_endpoints(lnk)
    from_obj <- find_object_type(bundle, ep$from)
    to_obj <- find_object_type(bundle, ep$to)
    if (is.null(from_obj) || is.null(to_obj)) next

    keys <- .link_keys(lnk)
    if (is.null(keys)) {
      no_join <- c(no_join, lnk$id)
      next
    }
    from_keys <- keys$from

    from_tbl <- .pgq_relation(from_obj, graph_name)
    to_tbl <- .pgq_relation(to_obj, graph_name)
    from_pk <- .pgq_pk(from_obj)
    to_pk <- .pgq_pk(to_obj)

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

  .warn_no_join(no_join)

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
