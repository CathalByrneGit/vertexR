#' @importFrom rlang %||% abort warn inform
NULL

#' vertexR: Graph Exploration, Simulation, and Optimization
#'
#' Turns ontology specifications into interconnected system graphs
#' for exploration, alerting, what-if simulation, and optimization.
#'
#' @docType package
#' @name vertexR-package
"_PACKAGE"

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Extract primary key column names from an object type
#'
#' @param obj An ontologySpecR object_type
#' @return Character vector of primary key column names
#' @keywords internal
get_pk_columns <- function(obj) {
  as.character(ontologySpecR::object_pk(obj))
}

#' Get source table name from an object type
#'
#' @param obj An ontologySpecR object_type
#' @return Character string table name
#' @keywords internal
get_source_table <- function(obj) {
  ontologySpecR::object_table(obj)
}

#' Read selected columns from a table via DBI lazily
#'
#' Uses dplyr::tbl() for lazy construction and only collects the requested
#' columns to reduce memory usage for large tables.
#'
#' @param con A DBI connection
#' @param tbl_nm Character table name
#' @param columns Character vector of columns to select, or NULL for all
#' @return A data.frame or NULL if table doesn't exist
#' @keywords internal
read_table <- function(con, tbl_nm, columns = NULL) {

  if (!DBI::dbExistsTable(con, tbl_nm)) {
    warn(paste0("Table '", tbl_nm, "' does not exist in the database. ",
                "Skipping."))
    return(NULL)
  }
  tbl_lazy <- dplyr::tbl(con, tbl_nm)
  if (!is.null(columns)) {
    existing_cols <- colnames(tbl_lazy)
    cols_to_select <- intersect(columns, existing_cols)
    if (length(cols_to_select) > 0L) {
      tbl_lazy <- dplyr::select(tbl_lazy, dplyr::all_of(cols_to_select))
    }
  }
  dplyr::collect(tbl_lazy)
}

#' Count rows in a table lazily
#'
#' @param con A DBI connection
#' @param tbl_nm Character table name
#' @return Integer row count, or 0 if table doesn't exist
#' @keywords internal
count_table_rows <- function(con, tbl_nm) {
  if (!DBI::dbExistsTable(con, tbl_nm)) {
    return(0L)
  }
  result <- DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ",
                                         DBI::dbQuoteIdentifier(con, tbl_nm)))
  as.integer(result$n[1])
}

#' Build a composite node ID from primary key columns
#'
#' When a primary key has multiple columns, paste them together with a null
#' separator (\\x00) which cannot appear in standard identifiers, avoiding
#' collisions when PK values contain colons.
#'
#' @param df A data.frame of rows
#' @param pk_cols Character vector of column names
#' @return Character vector of node IDs
#' @keywords internal
make_node_ids <- function(df, pk_cols) {
  if (length(pk_cols) == 1L) {
    as.character(df[[pk_cols]])
  } else {
    do.call(paste, c(lapply(pk_cols, function(col) as.character(df[[col]])),
                     list(sep = "\x00")))
  }
}

#' Get object types from a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @return List of object types
#' @keywords internal
get_bundle_objects <- function(bundle) {
  ontologySpecR::bundle_object_types(bundle)
}

#' Get link types from a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @return List of link types
#' @keywords internal
get_bundle_links <- function(bundle) {
  ontologySpecR::bundle_link_types(bundle)
}

#' Find an object type by ID in a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @param id Character object type ID
#' @return An ontologySpecR object_type or NULL if the id is unknown
#' @keywords internal
find_object_type <- function(bundle, id) {
  tryCatch(ontologySpecR::bundle_object_type(bundle, id),
           error = function(e) NULL)
}

#' Find a link type by ID in a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @param id Character link type ID
#' @return An ontologySpecR link_type or NULL if the id is unknown
#' @keywords internal
find_link_type <- function(bundle, id) {
  for (lnk in get_bundle_links(bundle)) {
    if (identical(lnk$id, id)) return(lnk)
  }
  NULL
}

#' Coerce input to a bundle, warning on legacy spellings
#'
#' Single entry point used by every function that takes a bundle.
#'
#' @param bundle A bundle, or something `ontologySpecR::as_bundle()` accepts
#' @return A validated ontologySpecR bundle
#' @keywords internal
as_vx_bundle <- function(bundle) {
  ontologySpecR::as_bundle(bundle)
}

#' Read table with optional WHERE filter
#'
#' @param con A DBI connection
#' @param tbl_nm Character table name
#' @param columns Character vector of columns to select, or NULL for all
#' @param where Character WHERE clause (without WHERE keyword), or NULL
#' @return A data.frame or NULL if table doesn't exist
#' @keywords internal
read_table_filtered <- function(con, tbl_nm, columns = NULL, where = NULL) {
  if (!DBI::dbExistsTable(con, tbl_nm)) {
    warn(paste0("Table '", tbl_nm, "' does not exist in the database. ",
                "Skipping."))
    return(NULL)
  }

  if (is.null(where)) {
    return(read_table(con, tbl_nm, columns))
  }

  # Build SQL with WHERE clause
  tbl_quoted <- DBI::dbQuoteIdentifier(con, tbl_nm)
  if (is.null(columns)) {
    cols_sql <- "*"
  } else {
    cols_sql <- paste(DBI::dbQuoteIdentifier(con, columns), collapse = ", ")
  }

  sql <- paste0("SELECT ", cols_sql, " FROM ", tbl_quoted, " WHERE ", where)
  tryCatch(
    DBI::dbGetQuery(con, sql),
    error = function(e) {
      warn(paste0("Query failed for table '", tbl_nm, "': ",
                  conditionMessage(e)))
      NULL
    }
  )
}


#' Count rows in a table with optional WHERE filter
#'
#' @param con A DBI connection
#' @param tbl_nm Character table name
#' @param where Character WHERE clause (without WHERE keyword)
#' @return Integer row count, or 0 if table doesn't exist or query fails
#' @keywords internal
count_table_rows_filtered <- function(con, tbl_nm, where) {
  if (!DBI::dbExistsTable(con, tbl_nm)) {
    return(0L)
  }
  tbl_quoted <- DBI::dbQuoteIdentifier(con, tbl_nm)
  sql <- paste0("SELECT COUNT(*) AS n FROM ", tbl_quoted, " WHERE ", where)
  tryCatch({
    result <- DBI::dbGetQuery(con, sql)
    as.integer(result$n[1])
  }, error = function(e) {
    0L
  })
}
