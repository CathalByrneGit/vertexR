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

#' Is an object type backed by a SQL query rather than a table?
#'
#' @param obj An ontologySpecR object_type
#' @return Logical
#' @keywords internal
is_query_object <- function(obj) {
  identical(ontologySpecR::object_source(obj)$kind, "query")
}

#' Lazy table for an object type
#'
#' Query-backed types (`source$kind = "query"`) become a lazy `tbl` over the
#' SQL expression; table-backed types use their source table.
#'
#' @param con A DBI connection
#' @param ot An ontologySpecR object_type
#' @return A lazy `tbl`
#' @keywords internal
.object_tbl <- function(con, ot) {
  if (is_query_object(ot)) {
    dplyr::tbl(con, dplyr::sql(ontologySpecR::object_source_sql(ot)))
  } else {
    id <- ontologySpecR::object_table_id(ot)
    dplyr::tbl(con, if (is.null(id$schema)) id$table
                    else dbplyr::in_schema(id$schema, id$table))
  }
}

#' Does the backing relation of an object type exist?
#'
#' Query-backed types are assumed to exist (the query is validated when run).
#'
#' @keywords internal
.object_exists <- function(con, ot) {
  if (is_query_object(ot)) return(TRUE)
  id <- ontologySpecR::object_table_id(ot)
  DBI::dbExistsTable(con, if (is.null(id$schema)) id$table
                          else DBI::Id(schema = id$schema, table = id$table))
}

#' Key under which `filter_sql` is looked up for an object type
#'
#' The object type id always works; table-backed types also accept their
#' table name (the original convention).
#'
#' @keywords internal
.filter_for <- function(filter_sql, ot) {
  f <- filter_sql[[ot$id]]
  if (is.null(f) && !is_query_object(ot)) {
    f <- filter_sql[[ontologySpecR::object_table(ot)]]
  }
  f
}

#' Read (optionally filtered, column-selected) rows of an object type
#'
#' @param con A DBI connection
#' @param ot An ontologySpecR object_type
#' @param columns Character vector of columns, or NULL for all
#' @param where Character WHERE clause (no `WHERE` keyword), or NULL
#' @return A data.frame, or NULL if the backing table is missing or the
#'   query fails
#' @keywords internal
read_object <- function(con, ot, columns = NULL, where = NULL) {
  if (!.object_exists(con, ot)) {
    warn(paste0("Table '", ontologySpecR::object_table(ot),
                "' does not exist in the database. Skipping."))
    return(NULL)
  }
  tryCatch({
    x <- .object_tbl(con, ot)
    if (!is.null(where)) x <- dplyr::filter(x, dplyr::sql(where))
    if (!is.null(columns)) {
      keep <- intersect(columns, colnames(x))
      if (length(keep) > 0L) x <- dplyr::select(x, dplyr::all_of(keep))
    }
    as.data.frame(dplyr::collect(x), stringsAsFactors = FALSE)
  }, error = function(e) {
    warn(paste0("Query failed for object type '", ot$id, "': ",
                conditionMessage(e)))
    NULL
  })
}

#' Count rows of an object type, optionally filtered
#'
#' @inheritParams read_object
#' @return Integer count (0 if missing or the query fails)
#' @keywords internal
count_object <- function(con, ot, where = NULL) {
  if (!.object_exists(con, ot)) return(0L)
  tryCatch({
    x <- .object_tbl(con, ot)
    if (!is.null(where)) x <- dplyr::filter(x, dplyr::sql(where))
    as.integer(dplyr::pull(dplyr::count(x), "n")[1])
  }, error = function(e) 0L)
}

#' Join keys of a link type, or NULL if it has none
#'
#' @param lnk An ontologySpecR link_type
#' @return `list(from =, to =)` of character vectors, or NULL
#' @keywords internal
.link_keys <- function(lnk) {
  # link_join_keys() stops on a missing join (-> skip, caller warns) and on
  # mismatched key lengths (a real spec error, which must propagate).
  k <- tryCatch(
    ontologySpecR::link_join_keys(lnk),
    error = function(e) {
      if (grepl("missing join keys", conditionMessage(e), fixed = TRUE)) {
        NULL
      } else {
        stop(e)
      }
    }
  )
  if (is.null(k)) return(NULL)
  from <- as.character(k$from)
  to <- as.character(k$to)
  if (length(from) == 0L || length(to) == 0L) return(NULL)
  list(from = from, to = to)
}

#' Warn once about links skipped for lack of join keys
#'
#' @param ids Character vector of link ids
#' @keywords internal
.warn_no_join <- function(ids) {
  if (length(ids) > 0L) {
    warn(paste0("Skipping link type(s) with no join keys: ",
                paste(ids, collapse = ", "), "."))
  }
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
#' @return Unnamed list of object types (ontologySpecR names them by id;
#'   internal code iterates by position and must not leak names into JSON)
#' @keywords internal
get_bundle_objects <- function(bundle) {
  unname(ontologySpecR::bundle_object_types(bundle))
}

#' Get link types from a bundle
#'
#' @param bundle An ontologySpecR bundle
#' @return List of link types
#' @keywords internal
get_bundle_links <- function(bundle) {
  unname(ontologySpecR::bundle_link_types(bundle))
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

