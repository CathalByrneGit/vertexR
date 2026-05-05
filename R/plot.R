#' Plot a Vertex Graph
#'
#' Creates an interactive graph visualization using visNetwork (if available)
#' or a static plot using ggraph.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param color_by Character; node column to map to fill color
#'   (default `.object_type`).
#' @param size_by Character; node column to map to size, or `NULL`.
#' @param edge_color_by Character; edge column to map to color
#'   (default `.link_type`).
#' @param label_by Character; node column for labels (default `.node_id`).
#' @param highlight Character vector of `.node_id` values to highlight.
#' @param layout Character; igraph layout algorithm (default `"fr"` for
#'   Fruchterman-Reingold).
#'
#' @return A `visNetwork` htmlwidget or a `ggplot` object.
#' @export
vx_plot <- function(g,
                    color_by = ".object_type",
                    size_by = NULL,
                    edge_color_by = ".link_type",
                    label_by = ".node_id",
                    highlight = NULL,
                    layout = "fr") {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }

  nodes <- vx_nodes(g)
  edges <- vx_edges(g)

  # Try visNetwork first
  if (requireNamespace("visNetwork", quietly = TRUE)) {
    return(.vx_plot_visnetwork(nodes, edges, color_by, size_by,
                                edge_color_by, label_by, highlight))
  }

  # Fall back to ggraph
  if (requireNamespace("ggraph", quietly = TRUE) &&
      requireNamespace("ggplot2", quietly = TRUE)) {
    return(.vx_plot_ggraph(g, color_by, size_by, edge_color_by,
                            label_by, highlight, layout))
  }

  abort(paste0(
    "Neither 'visNetwork' nor 'ggraph'+'ggplot2' are installed. ",
    "Install one of them for visualization:\n",
    "  install.packages('visNetwork')  # interactive\n",
    "  install.packages(c('ggraph', 'ggplot2'))  # static"
  ))
}


#' @keywords internal
.vx_plot_visnetwork <- function(nodes, edges, color_by, size_by,
                                 edge_color_by, label_by, highlight) {
  # Build visNetwork node data
  vis_nodes <- data.frame(
    id = seq_len(nrow(nodes)),
    label = if (label_by %in% names(nodes)) as.character(nodes[[label_by]])
            else nodes$.node_id,
    title = paste0(nodes$.object_type, ": ", nodes$.node_id),
    stringsAsFactors = FALSE
  )

  # Color by group

if (color_by %in% names(nodes)) {
    vis_nodes$group <- as.character(nodes[[color_by]])
  }

  # Size
  if (!is.null(size_by) && size_by %in% names(nodes)) {
    vals <- as.numeric(nodes[[size_by]])
    # Scale to reasonable visNetwork sizes (10-50)
    if (!all(is.na(vals))) {
      rng <- range(vals, na.rm = TRUE)
      if (rng[2] > rng[1]) {
        vis_nodes$value <- 10 + 40 * (vals - rng[1]) / (rng[2] - rng[1])
      } else {
        vis_nodes$value <- 25
      }
    }
  }

  # Highlight
  if (!is.null(highlight)) {
    highlight_idx <- which(nodes$.node_id %in% highlight)
    vis_nodes$borderWidth <- 1
    vis_nodes$borderWidth[highlight_idx] <- 4
    vis_nodes$color.border <- "gray"
    vis_nodes$color.border[highlight_idx] <- "red"
  }

  # Build visNetwork edge data
  vis_edges <- data.frame(
    from = edges$from,
    to = edges$to,
    stringsAsFactors = FALSE
  )
  if (edge_color_by %in% names(edges)) {
    vis_edges$title <- as.character(edges[[edge_color_by]])
    # Assign colors by link type
    link_types <- unique(edges[[edge_color_by]])
    colors <- grDevices::palette.colors(max(3, length(link_types)),
                                         palette = "Okabe-Ito")
    color_map <- stats::setNames(colors[seq_along(link_types)], link_types)
    vis_edges$color <- unname(color_map[edges[[edge_color_by]]])
  }
  vis_edges$arrows <- "to"

  visNetwork::visNetwork(vis_nodes, vis_edges) |>
    visNetwork::visLayout(randomSeed = 42) |>
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
}


#' @keywords internal
.vx_plot_ggraph <- function(g, color_by, size_by, edge_color_by,
                             label_by, highlight, layout) {
  p <- ggraph::ggraph(g, layout = layout)

  # Edges
  if (edge_color_by %in% names(vx_edges(g))) {
    p <- p + ggraph::geom_edge_link(
      ggplot2::aes(colour = .data[[edge_color_by]]),
      arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
      alpha = 0.6
    )
  } else {
    p <- p + ggraph::geom_edge_link(
      arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
      alpha = 0.6
    )
  }

  # Nodes
  node_aes <- list()
  if (color_by %in% names(vx_nodes(g))) {
    node_aes$fill <- color_by
  }
  if (!is.null(size_by) && size_by %in% names(vx_nodes(g))) {
    node_aes$size <- size_by
  }

  if (length(node_aes) > 0) {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(!!!rlang::syms(node_aes)),
      shape = 21, colour = "black"
    )
  } else {
    p <- p + ggraph::geom_node_point(shape = 21, fill = "steelblue",
                                      colour = "black", size = 4)
  }

  # Labels
  if (label_by %in% names(vx_nodes(g))) {
    p <- p + ggraph::geom_node_text(
      ggplot2::aes(label = .data[[label_by]]),
      repel = TRUE, size = 3
    )
  }

  p <- p + ggraph::theme_graph()
  p
}


#' Plot a Scenario Diff
#'
#' Visualizes the differences between the original graph and a scenario.
#' Removed nodes appear in red, modified nodes in yellow, and added edges
#' in green.
#'
#' @param g_original A `tbl_graph` (the baseline).
#' @param scenario A `vx_scenario` object.
#' @param layout Character; igraph layout algorithm (default `"fr"`).
#'
#' @return A `visNetwork` htmlwidget or a `ggplot` object.
#' @export
vx_plot_scenario <- function(g_original, scenario, layout = "fr") {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }

  g_modified <- vx_apply_scenario(scenario)
  diff <- vx_compare(g_original, g_modified)

  nodes <- vx_nodes(g_original)
  # Annotate nodes with status
  nodes$.diff_status <- "unchanged"
  removed_ids <- diff$id[diff$element == "node" & diff$change == "removed"]
  modified_ids <- diff$id[diff$element == "node" & diff$change == "modified"]
  nodes$.diff_status[nodes$.node_id %in% removed_ids] <- "removed"
  nodes$.diff_status[nodes$.node_id %in% modified_ids] <- "modified"

  vx_plot(g_original, color_by = ".diff_status",
          highlight = c(removed_ids, modified_ids),
          layout = layout)
}


#' Add Concept Results to Graph Nodes
#'
#' Joins concept evaluation results onto graph nodes, enabling visualization
#' of concept truth values via `vx_plot(g, color_by = "<concept_name>")`.
#'
#' This is the integration point with `conceptR` — concept evaluations can be
#' visualized as graph node colours.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param concept_results A data frame with columns `.node_id` and one or more
#'   concept columns (logical/boolean values indicating concept membership).
#'
#' @return A modified `tbl_graph` with concept columns added to nodes.
#' @export
vx_color_by_concept <- function(g, concept_results) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  if (!is.data.frame(concept_results)) {
    abort("`concept_results` must be a data frame.")
  }
  if (!".node_id" %in% names(concept_results)) {
    abort("`concept_results` must have a `.node_id` column.")
  }

  nodes <- vx_nodes(g)
  edges <- vx_edges(g)

  # Join concept results onto nodes
  concept_cols <- setdiff(names(concept_results), ".node_id")
  if (length(concept_cols) == 0L) {
    warn("No concept columns found in concept_results (only .node_id present).")
    return(g)
  }

  nodes <- dplyr::left_join(nodes, concept_results, by = ".node_id")

  tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
}
