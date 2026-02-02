#' Extract a Subgraph by Object Type
#'
#' Filters the graph to only include nodes of the specified object type
#' and edges between those nodes.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param object_type_id Character; the object type to filter on.
#' @return A `tbl_graph` containing only nodes of the given type.
#' @export
vx_subgraph <- function(g, object_type_id) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  nodes <- vx_nodes(g)
  keep_indices <- which(nodes$.object_type == object_type_id)
  if (length(keep_indices) == 0L) {
    warn(paste0("No nodes found with object type '", object_type_id, "'."))
  }
  ig <- igraph::as.igraph(g)
  sub_ig <- igraph::induced_subgraph(ig, keep_indices)
  tidygraph::as_tbl_graph(sub_ig)
}


#' Get Neighborhood (Ego Graph)
#'
#' Extracts the neighborhood of a node up to a given depth. This is the
#' "search-around" operation.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param node_id Character; the `.node_id` of the center node.
#' @param depth Integer; how many hops to include (default 1).
#' @return A `tbl_graph` with the neighborhood subgraph.
#' @export
vx_neighbors <- function(g, node_id, depth = 1L) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  nodes <- vx_nodes(g)
  center_idx <- which(nodes$.node_id == node_id)
  if (length(center_idx) == 0L) {
    abort(paste0("Node '", node_id, "' not found in the graph."))
  }

  ig <- igraph::as.igraph(g)
  # Use ego() to find all nodes within depth hops (undirected for search-around)
  ego_verts <- igraph::ego(ig, order = depth, nodes = center_idx,
                           mode = "all")[[1]]
  sub_ig <- igraph::induced_subgraph(ig, ego_verts)
  tidygraph::as_tbl_graph(sub_ig)
}


#' Find Shortest Path Between Two Nodes
#'
#' Computes the shortest path between two nodes identified by `.node_id`.
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @param from_id Character; the `.node_id` of the source node.
#' @param to_id Character; the `.node_id` of the target node.
#' @return A tibble of nodes along the shortest path (in order), or an
#'   empty tibble if no path exists.
#' @export
vx_shortest_path <- function(g, from_id, to_id) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  nodes <- vx_nodes(g)
  from_idx <- which(nodes$.node_id == from_id)
  to_idx   <- which(nodes$.node_id == to_id)

  if (length(from_idx) == 0L) {
    abort(paste0("Node '", from_id, "' not found in the graph."))
  }
  if (length(to_idx) == 0L) {
    abort(paste0("Node '", to_id, "' not found in the graph."))
  }

  ig <- igraph::as.igraph(g)
  sp <- igraph::shortest_paths(ig, from = from_idx, to = to_idx,
                               mode = "all", output = "vpath")
  path_verts <- sp$vpath[[1]]

  if (length(path_verts) == 0L) {
    return(nodes[integer(0), , drop = FALSE])
  }

  path_indices <- as.integer(path_verts)
  nodes[path_indices, , drop = FALSE]
}
