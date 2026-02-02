#' Define an Alert Rule
#'
#' Creates an alert rule that can be evaluated against a graph.
#' The condition function receives a `tbl_graph` and returns a character
#' vector of `.node_id` values that are flagged.
#'
#' @param id Character; unique rule identifier.
#' @param description Character; human-readable description.
#' @param condition A function taking a single argument (a `tbl_graph`)
#'   and returning a character vector of flagged `.node_id` values.
#' @param severity Character; one of `"low"`, `"medium"`, `"high"`,
#'   `"critical"`.
#'
#' @return An S3 object of class `"vx_alert_rule"`.
#' @export
vx_alert_rule <- function(id, description, condition, severity = "medium") {
  if (!is.character(id) || length(id) != 1L) {
    abort("`id` must be a single character string.")
  }
  if (!is.function(condition)) {
    abort("`condition` must be a function.")
  }
  valid_severities <- c("low", "medium", "high", "critical")
  if (!severity %in% valid_severities) {
    abort(paste0("`severity` must be one of: ",
                 paste(valid_severities, collapse = ", ")))
  }

  structure(
    list(
      id = id,
      description = description,
      condition = condition,
      severity = severity
    ),
    class = "vx_alert_rule"
  )
}

#' @export
print.vx_alert_rule <- function(x, ...) {
  cat("<vx_alert_rule>\n")
  cat("  ID:          ", x$id, "\n")
  cat("  Description: ", x$description, "\n")

  cat("  Severity:    ", x$severity, "\n")
  invisible(x)
}


#' Evaluate Alert Rules Against a Graph
#'
#' Runs one or more alert rules against a graph and returns a tibble
#' of all flagged nodes.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param rules A list of `vx_alert_rule` objects.
#'
#' @return A tibble with columns: `rule_id`, `severity`, `node_id`,
#'   `evaluated_at`.
#' @export
vx_evaluate_alerts <- function(g, rules) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  if (!is.list(rules)) {
    abort("`rules` must be a list of vx_alert_rule objects.")
  }

  now <- Sys.time()
  results <- list()

  for (rule in rules) {
    if (!inherits(rule, "vx_alert_rule")) {
      warn("Skipping non-vx_alert_rule object in rules list.")
      next
    }
    flagged <- tryCatch(
      rule$condition(g),
      error = function(e) {
        warn(paste0("Alert rule '", rule$id, "' failed: ", e$message))
        character(0)
      }
    )
    flagged <- as.character(flagged)
    if (length(flagged) > 0L) {
      results[[length(results) + 1L]] <- dplyr::tibble(
        rule_id      = rule$id,
        severity     = rule$severity,
        node_id      = flagged,
        evaluated_at = now
      )
    }
  }

  if (length(results) == 0L) {
    return(dplyr::tibble(
      rule_id      = character(0),
      severity     = character(0),
      node_id      = character(0),
      evaluated_at = as.POSIXct(character(0))
    ))
  }

  dplyr::bind_rows(results)
}


#' Propagate Risk Through the Graph
#'
#' Starting from flagged nodes, walks edges up to a given depth to find
#' downstream (reachable) nodes that may be affected.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param flagged_nodes Character vector of `.node_id` values to start from.
#' @param depth Integer; how many hops to propagate (default 2).
#'
#' @return A tibble with columns: `node_id`, `.object_type`, `depth`
#'   (distance from nearest flagged node), `source_node` (which flagged
#'   node reached it).
#' @export
vx_propagate_risk <- function(g, flagged_nodes, depth = 2L) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  nodes <- vx_nodes(g)
  ig <- igraph::as.igraph(g)

  flagged_indices <- which(nodes$.node_id %in% flagged_nodes)
  if (length(flagged_indices) == 0L) {
    return(dplyr::tibble(
      node_id      = character(0),
      .object_type = character(0),
      depth        = integer(0),
      source_node  = character(0)
    ))
  }

  results <- list()
  for (fi in flagged_indices) {
    source_id <- nodes$.node_id[fi]
    # Walk outward from the flagged node
    ego_verts <- igraph::ego(ig, order = depth, nodes = fi,
                             mode = "all")[[1]]
    ego_indices <- as.integer(ego_verts)
    # Compute distances
    dists <- igraph::distances(ig, v = fi, to = ego_indices,
                               mode = "all")[1, ]

    for (j in seq_along(ego_indices)) {
      idx <- ego_indices[j]
      d <- as.integer(dists[j])
      # Skip the flagged node itself and nodes beyond depth
      if (d == 0L || d > depth) next
      results[[length(results) + 1L]] <- list(
        node_id      = nodes$.node_id[idx],
        .object_type = nodes$.object_type[idx],
        depth        = d,
        source_node  = source_id
      )
    }
  }

  if (length(results) == 0L) {
    return(dplyr::tibble(
      node_id      = character(0),
      .object_type = character(0),
      depth        = integer(0),
      source_node  = character(0)
    ))
  }

  out <- dplyr::bind_rows(lapply(results, dplyr::as_tibble))
  # De-duplicate: keep the minimum depth for each node_id
  out <- out[order(out$depth), ]
  out <- out[!duplicated(out$node_id), ]
  out
}
