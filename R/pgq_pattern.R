#' Named PGQ Investigation Patterns
#'
#' A list of named graph patterns designed for investigation use cases
#' (e.g., ICIJ Panama/Paradise Papers style analysis).
#'
#' @keywords internal
PGQ_PATTERNS <- list(

  common_officer = list(
    description = "Entities that share at least one officer - may indicate common control",
    sql = "
      FROM GRAPH_TABLE ({graph_name}
        MATCH (o:Officer)-[:OfficerOf]->(e1:Entity),
              (o)-[:OfficerOf]->(e2:Entity)
        WHERE e1.node_id <> e2.node_id
        {where_clause}
        COLUMNS (
          o.node_id  AS officer_id,
          o.name     AS officer_name,
          e1.node_id AS entity1_id,
          e1.name    AS entity1_name,
          e2.node_id AS entity2_id,
          e2.name    AS entity2_name
        )
      )
      {limit_clause}
    "
  ),

  circular_ownership = list(
    description = "Circular ownership chains - entity controls entity that controls it",
    sql = "
      FROM GRAPH_TABLE ({graph_name}
        MATCH (e1:Entity)-[:RelatedEntity]->{2,{max_depth}}(e1)
        {where_clause}
        COLUMNS (
          e1.node_id AS entity_id,
          e1.name AS entity_name,
          e1.jurisdiction AS jurisdiction
        )
      )
      {limit_clause}
    "
  ),

  multi_jurisdiction_officer = list(
    description = "Officers appearing in entities across N or more jurisdictions",
    sql = "
      SELECT officer_id, officer_name,
             COUNT(DISTINCT jurisdiction) AS jurisdiction_count,
             LIST(DISTINCT jurisdiction) AS jurisdictions
      FROM (
        FROM GRAPH_TABLE ({graph_name}
          MATCH (o:Officer)-[:OfficerOf]->(e:Entity)
          {where_clause}
          COLUMNS (
            o.node_id AS officer_id,
            o.name AS officer_name,
            e.jurisdiction AS jurisdiction
          )
        )
      )
      GROUP BY officer_id, officer_name
      HAVING jurisdiction_count >= {min_jurisdictions}
      ORDER BY jurisdiction_count DESC
      {limit_clause}
    "
  ),

  deep_ownership = list(
    description = "Officers connected to entities via multi-hop corporate chains",
    sql = "
      FROM GRAPH_TABLE ({graph_name}
        MATCH (o:Officer)-[:OfficerOf]->(e1:Entity)-[:RelatedEntity]->{1,{max_depth}}(eN:Entity)
        {where_clause}
        COLUMNS (
          o.node_id   AS officer_id,
          o.name      AS officer_name,
          e1.node_id  AS direct_entity_id,
          e1.name     AS direct_entity_name,
          eN.node_id  AS final_entity_id,
          eN.name     AS final_entity_name,
          eN.jurisdiction AS jurisdiction
        )
      )
      {limit_clause}
    "
  ),

  intermediary_hub = list(
    description = "Intermediaries who incorporated the most entities",
    sql = "
      SELECT intermediary_id, intermediary_name, COUNT(*) AS entity_count
      FROM (
        FROM GRAPH_TABLE ({graph_name}
          MATCH (i:Intermediary)-[:IntermediaryOf]->(e:Entity)
          {where_clause}
          COLUMNS (
            i.node_id AS intermediary_id,
            i.name AS intermediary_name,
            e.node_id AS entity_id
          )
        )
      )
      GROUP BY intermediary_id, intermediary_name
      ORDER BY entity_count DESC
      {limit_clause}
    "
  )
)


#' Run a Named Investigative Graph Pattern
#'
#' Executes one of the built-in named patterns against a DuckPGQ property graph.
#' These patterns are designed for investigation use cases like analyzing
#' corporate ownership structures.
#'
#' @param connection DBI connection with DuckPGQ loaded.
#' @param graph_name Character; the property graph name.
#' @param pattern_name Character; one of the named patterns:
#'   \itemize{
#'     \item `"common_officer"` - Entities sharing an officer
#'     \item `"circular_ownership"` - Circular ownership chains
#'     \item `"multi_jurisdiction_officer"` - Officers in multiple jurisdictions
#'     \item `"deep_ownership"` - Multi-hop corporate chains
#'     \item `"intermediary_hub"` - High-volume intermediaries
#'   }
#' @param params Named list of pattern parameters:
#'   \itemize
#'     \item `limit` - Row limit (default NULL for no limit)
#'     \item `where` - Additional WHERE clause
#'     \item `min_jurisdictions` - For multi_jurisdiction_officer (default 2)
#'     \item `max_depth` - For circular/deep ownership (default 6)
#'   }
#'
#' @return A data.frame with pattern results.
#' @export
vx_pgq_pattern <- function(connection, graph_name, pattern_name,
                           params = list()) {
  if (!inherits(connection, "DBIConnection")) {
    abort("`connection` must be a DBI connection object.")
  }

  if (!pattern_name %in% names(PGQ_PATTERNS)) {
    available <- paste(names(PGQ_PATTERNS), collapse = ", ")
    abort(paste0("Unknown pattern '", pattern_name, "'. ",
                 "Available patterns: ", available))
  }

  pattern <- PGQ_PATTERNS[[pattern_name]]

  # Build substitution values
  limit_clause <- if (!is.null(params$limit)) {
    paste("LIMIT", as.integer(params$limit))
  } else {
    ""
  }

  where_clause <- if (!is.null(params$where)) {
    paste("AND", params$where)
  } else {
    ""
  }

  min_jurisdictions <- params$min_jurisdictions %||% 2L
  max_depth <- params$max_depth %||% 6L

  # Substitute parameters into SQL template
  sql <- pattern$sql
  sql <- gsub("\\{graph_name\\}", graph_name, sql)
  sql <- gsub("\\{limit_clause\\}", limit_clause, sql)
  sql <- gsub("\\{where_clause\\}", where_clause, sql)
  sql <- gsub("\\{min_jurisdictions\\}", as.integer(min_jurisdictions), sql)
  sql <- gsub("\\{max_depth\\}", as.integer(max_depth), sql)

  tryCatch(
    DBI::dbGetQuery(connection, sql),
    error = function(e) {
      abort(paste0("Pattern query '", pattern_name, "' failed: ",
                   conditionMessage(e)))
    }
  )
}


#' List Available PGQ Patterns
#'
#' Returns information about the built-in named patterns.
#'
#' @return A data.frame with columns: `pattern_name`, `description`.
#' @export
vx_pgq_list_patterns <- function() {
  data.frame(
    pattern_name = names(PGQ_PATTERNS),
    description = vapply(PGQ_PATTERNS, function(p) p$description, character(1)),
    stringsAsFactors = FALSE
  )
}
