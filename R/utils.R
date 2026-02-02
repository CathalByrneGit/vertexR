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

#' Extract primary key column names from an ontologySpecR object type
#'
#' Handles the structured primaryKey (with $properties and $strategy).
#'
#' @param obj An ontologySpecR object_type
#' @return Character vector of primary key column names
#' @keywords internal
get_pk_columns <- function(obj) {

  pk <- obj$primaryKey
  if (is.null(pk)) {
    abort(paste0("Object type '", obj$id, "' has no primaryKey defined."))
  }
  # primaryKey is an ontology_primary_key_def with $properties (char vector)
  # and $strategy ("natural" or "surrogate")
  props <- pk$properties
  if (is.null(props)) {
    abort(paste0("Object type '", obj$id,
                 "' has primaryKey but no $properties field."))
  }
  as.character(props)
}

#' Get source table name from an ontologySpecR object type
#'
#' @param obj An ontologySpecR object_type
#' @return Character string table name
#' @keywords internal
get_source_table <- function(obj) {
  src <- obj$source
  if (is.null(src) || is.null(src$table)) {
    abort(paste0("Object type '", obj$id,
                 "' has no source table defined. ",
                 "vertexR requires source$table for each object type."))
  }
  src$table
}

#' Read all rows from a table via DBI
#'
#' @param con A DBI connection
#' @param table_name Character table name
#' @return A data.frame
#' @keywords internal
read_table <- function(con, table_name) {
  if (!DBI::dbExistsTable(con, table_name)) {
    warn(paste0("Table '", table_name, "' does not exist in the database. ",
                "Skipping."))
    return(NULL)
  }
  DBI::dbReadTable(con, table_name)
}

#' Build a composite node ID from primary key columns
#'
#' When a primary key has multiple columns, paste them together with ":".
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
                     list(sep = ":")))
  }
}

#' Get objects from a bundle, handling both field naming conventions
#'
#' ontologySpecR uses `objects` and `links` (not objectTypes/linkTypes).
#' We handle both forms for robustness.
#'
#' @param bundle An ontologySpecR bundle
#' @return List of object types
#' @keywords internal
get_bundle_objects <- function(bundle) {

  objs <- bundle$objects
  if (is.null(objs)) {
    objs <- bundle$objectTypes  # fallback for alternative naming
  }
  objs %||% list()
}

#' Get links from a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @return List of link types
#' @keywords internal
get_bundle_links <- function(bundle) {
  lnks <- bundle$links
  if (is.null(lnks)) {
    lnks <- bundle$linkTypes  # fallback for alternative naming
  }
  lnks %||% list()
}

#' Find an object type by ID in a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @param id Character object type ID
#' @return An ontologySpecR object_type or NULL
#' @keywords internal
find_object_type <- function(bundle, id) {
  objs <- get_bundle_objects(bundle)
  for (obj in objs) {
    if (identical(obj$id, id)) return(obj)
  }
  NULL
}

#' Find a link type by ID in a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @param id Character link type ID
#' @return An ontologySpecR link_type or NULL
#' @keywords internal
find_link_type <- function(bundle, id) {
  lnks <- get_bundle_links(bundle)
  for (lnk in lnks) {
    if (identical(lnk$id, id)) return(lnk)
  }
  NULL
}
