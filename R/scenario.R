#' Create a What-If Scenario
#'
#' Wraps a graph in a Scenario object that tracks modifications.
#' Apply modifications with [vx_modify_node()], [vx_remove_node()],
#' [vx_remove_edge()], and [vx_add_edge()], then materialize with
#' [vx_apply_scenario()].
#'
#' @param g A `tbl_graph` produced by [vertex_graph()].
#' @return An S3 object of class `"vx_scenario"`.
#' @export
vx_scenario <- function(g) {
  if (!inherits(g, "tbl_graph")) {
    abort("`g` must be a tbl_graph object.")
  }
  structure(
    list(
      original    = g,
      modifications = list(),  # list of operation records
      node_changes  = list(),  # node_id -> list of property changes
      remove_nodes  = character(0),
      remove_edges  = integer(0),  # edge row indices
      add_edges     = list()   # list of edge data frames to append
    ),
    class = "vx_scenario"
  )
}

#' @export
print.vx_scenario <- function(x, ...) {
  n_mod  <- length(x$node_changes)
  n_rm_n <- length(x$remove_nodes)
  n_rm_e <- length(x$remove_edges)
  n_add_e <- length(x$add_edges)
  cat("<vx_scenario>\n")
  cat("  Node modifications: ", n_mod, "\n")
  cat("  Nodes removed:      ", n_rm_n, "\n")
  cat("  Edges removed:      ", n_rm_e, "\n")
  cat("  Edges added:        ", n_add_e, "\n")
  invisible(x)
}


#' Modify a Node's Properties in a Scenario
#'
#' Records property changes for a specific node. Changes are applied
#' when [vx_apply_scenario()] is called.
#'
#' @param scenario A `vx_scenario` object.
#' @param node_id Character; the `.node_id` of the node to modify.
#' @param ... Named property values to change (e.g., `status = "closed"`).
#'
#' @return The modified `vx_scenario` (for piping).
#' @export
vx_modify_node <- function(scenario, node_id, ...) {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }
  changes <- list(...)
  if (length(changes) == 0L) {
    warn("No property changes specified in vx_modify_node().")
    return(scenario)
  }

  # Merge with any existing changes for this node
  existing <- scenario$node_changes[[node_id]] %||% list()
  scenario$node_changes[[node_id]] <- c(existing, changes)

  scenario$modifications[[length(scenario$modifications) + 1L]] <- list(
    op = "modify_node", node_id = node_id, changes = changes
  )
  scenario
}


#' Remove a Node from a Scenario
#'
#' Marks a node (and all its incident edges) for removal.
#'
#' @param scenario A `vx_scenario` object.
#' @param node_id Character; the `.node_id` of the node to remove.
#'
#' @return The modified `vx_scenario` (for piping).
#' @export
vx_remove_node <- function(scenario, node_id) {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }
  scenario$remove_nodes <- unique(c(scenario$remove_nodes, node_id))
  scenario$modifications[[length(scenario$modifications) + 1L]] <- list(
    op = "remove_node", node_id = node_id
  )
  scenario
}


#' Remove an Edge from a Scenario
#'
#' Marks a specific edge for removal by its row index in the edge table.
#'
#' @param scenario A `vx_scenario` object.
#' @param edge_id Integer; the row index of the edge to remove.
#'
#' @return The modified `vx_scenario` (for piping).
#' @export
vx_remove_edge <- function(scenario, edge_id) {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }
  scenario$remove_edges <- unique(c(scenario$remove_edges, as.integer(edge_id)))
  scenario$modifications[[length(scenario$modifications) + 1L]] <- list(
    op = "remove_edge", edge_id = edge_id
  )
  scenario
}


#' Add an Edge in a Scenario
#'
#' Records a new edge to be added when the scenario is applied.
#'
#' @param scenario A `vx_scenario` object.
#' @param edge_id Character; identifier for the new edge.
#' @param from Character; `.node_id` of the source node.
#' @param to Character; `.node_id` of the target node.
#' @param .link_type Character; the link type for the new edge.
#' @param ... Additional edge properties.
#'
#' @return The modified `vx_scenario` (for piping).
#' @export
vx_add_edge <- function(scenario, edge_id, from, to, .link_type, ...) {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }
  extra <- list(...)
  edge_data <- c(list(.edge_id = edge_id, .from_id = from, .to_id = to,
                       .link_type = .link_type), extra)
  scenario$add_edges[[length(scenario$add_edges) + 1L]] <- edge_data
  scenario$modifications[[length(scenario$modifications) + 1L]] <- list(
    op = "add_edge", edge_id = edge_id, from = from, to = to,
    .link_type = .link_type
  )
  scenario
}


#' Apply a Scenario to Produce a Modified Graph
#'
#' Takes a scenario with recorded modifications and produces a new
#' `tbl_graph` reflecting all changes.
#'
#' @param scenario A `vx_scenario` object.
#' @return A new `tbl_graph` with the scenario applied.
#' @export
vx_apply_scenario <- function(scenario) {
  if (!inherits(scenario, "vx_scenario")) {
    abort("`scenario` must be a vx_scenario object.")
  }

  g <- scenario$original
  nodes <- vx_nodes(g)
  edges <- vx_edges(g)

  # 1. Apply node property modifications
  for (nid in names(scenario$node_changes)) {
    row_idx <- which(nodes$.node_id == nid)
    if (length(row_idx) == 0L) {
      warn(paste0("Node '", nid, "' not found; skipping modification."))
      next
    }
    changes <- scenario$node_changes[[nid]]
    for (prop in names(changes)) {
      if (prop %in% names(nodes)) {
        nodes[[prop]][row_idx] <- changes[[prop]]
      } else {
        # Add new column
        nodes[[prop]] <- NA
        nodes[[prop]][row_idx] <- changes[[prop]]
      }
    }
  }

  # 2. Remove nodes (and their incident edges)
  if (length(scenario$remove_nodes) > 0L) {
    remove_idx <- which(nodes$.node_id %in% scenario$remove_nodes)
    if (length(remove_idx) > 0L) {
      # Remove edges incident to removed nodes
      edges <- edges[!(edges$from %in% remove_idx | edges$to %in% remove_idx), ,
                     drop = FALSE]
      # Remove nodes
      nodes <- nodes[-remove_idx, , drop = FALSE]
      # Reindex edges: build a mapping from old index to new index
      old_to_new <- rep(NA_integer_, max(c(remove_idx, nrow(nodes) + length(remove_idx))))
      new_indices <- seq_len(nrow(nodes))
      old_indices <- setdiff(seq_len(nrow(nodes) + length(remove_idx)), remove_idx)
      old_to_new[old_indices] <- new_indices
      if (nrow(edges) > 0L) {
        edges$from <- old_to_new[edges$from]
        edges$to   <- old_to_new[edges$to]
        # Remove edges that now have NA from/to
        edges <- edges[!is.na(edges$from) & !is.na(edges$to), , drop = FALSE]
      }
    }
  }

  # 3. Remove specific edges
  if (length(scenario$remove_edges) > 0L) {
    valid_remove <- scenario$remove_edges[scenario$remove_edges <= nrow(edges)]
    if (length(valid_remove) > 0L) {
      edges <- edges[-valid_remove, , drop = FALSE]
    }
  }

  # 4. Add new edges
  if (length(scenario$add_edges) > 0L) {
    for (new_edge in scenario$add_edges) {
      from_idx <- which(nodes$.node_id == new_edge$.from_id)
      to_idx   <- which(nodes$.node_id == new_edge$.to_id)
      if (length(from_idx) == 0L || length(to_idx) == 0L) {
        warn(paste0("Cannot add edge '", new_edge$.edge_id,
                     "': source or target node not found."))
        next
      }
      edge_row <- data.frame(
        from = from_idx[1],
        to = to_idx[1],
        .link_type = new_edge$.link_type,
        stringsAsFactors = FALSE
      )
      # Add extra properties
      extras <- new_edge[!names(new_edge) %in%
                          c(".edge_id", ".from_id", ".to_id", ".link_type")]
      for (nm in names(extras)) {
        edge_row[[nm]] <- extras[[nm]]
      }
      edges <- dplyr::bind_rows(edges, edge_row)
    }
  }

  # Reset row names
  rownames(nodes) <- NULL
  rownames(edges) <- NULL

  tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
}


#' Compare Two Graphs
#'
#' Compares an original graph with a modified graph and reports differences
#' in nodes and edges.
#'
#' @param g_original A `tbl_graph` (the baseline).
#' @param g_modified A `tbl_graph` (the scenario result).
#'
#' @return A tibble with columns: `element` ("node" or "edge"),
#'   `id`, `change` ("added", "removed", or "modified"), and `details`.
#' @export
vx_compare <- function(g_original, g_modified) {
  if (!inherits(g_original, "tbl_graph") || !inherits(g_modified, "tbl_graph")) {
    abort("Both `g_original` and `g_modified` must be tbl_graph objects.")
  }

  orig_nodes <- vx_nodes(g_original)
  mod_nodes  <- vx_nodes(g_modified)
  orig_edges <- vx_edges(g_original)
  mod_edges  <- vx_edges(g_modified)

  results <- list()

  # --- Node comparison ---
  orig_ids <- orig_nodes$.node_id
  mod_ids  <- mod_nodes$.node_id

  # Removed nodes
  removed <- setdiff(orig_ids, mod_ids)
  for (nid in removed) {
    results[[length(results) + 1L]] <- dplyr::tibble(
      element = "node", id = nid, change = "removed", details = ""
    )
  }

  # Added nodes
  added <- setdiff(mod_ids, orig_ids)
  for (nid in added) {
    results[[length(results) + 1L]] <- dplyr::tibble(
      element = "node", id = nid, change = "added", details = ""
    )
  }

  # Modified nodes (present in both)
  common <- intersect(orig_ids, mod_ids)
  # Compare property columns (excluding .object_type and .node_id)
  prop_cols <- setdiff(
    intersect(names(orig_nodes), names(mod_nodes)),
    c(".object_type", ".node_id")
  )
  for (nid in common) {
    orig_row <- orig_nodes[orig_nodes$.node_id == nid, , drop = FALSE]
    mod_row  <- mod_nodes[mod_nodes$.node_id == nid, , drop = FALSE]
    diffs <- character(0)
    for (col in prop_cols) {
      v_orig <- orig_row[[col]]
      v_mod  <- mod_row[[col]]
      if (!identical(v_orig, v_mod)) {
        diffs <- c(diffs, paste0(col, ": ", v_orig, " -> ", v_mod))
      }
    }
    if (length(diffs) > 0L) {
      results[[length(results) + 1L]] <- dplyr::tibble(
        element = "node", id = nid, change = "modified",
        details = paste(diffs, collapse = "; ")
      )
    }
  }

  # --- Edge comparison ---
  # Create edge signatures for comparison
  make_edge_sig <- function(edges, nodes_df) {
    if (nrow(edges) == 0L) return(character(0))
    from_ids <- nodes_df$.node_id[edges$from]
    to_ids   <- nodes_df$.node_id[edges$to]
    paste(from_ids, edges$.link_type, to_ids, sep = "->")
  }

  orig_sigs <- make_edge_sig(orig_edges, orig_nodes)
  mod_sigs  <- make_edge_sig(mod_edges, mod_nodes)

  removed_edges <- setdiff(orig_sigs, mod_sigs)
  for (sig in removed_edges) {
    results[[length(results) + 1L]] <- dplyr::tibble(
      element = "edge", id = sig, change = "removed", details = ""
    )
  }

  added_edges <- setdiff(mod_sigs, orig_sigs)
  for (sig in added_edges) {
    results[[length(results) + 1L]] <- dplyr::tibble(
      element = "edge", id = sig, change = "added", details = ""
    )
  }

  if (length(results) == 0L) {
    return(dplyr::tibble(
      element = character(0), id = character(0),
      change = character(0), details = character(0)
    ))
  }

  dplyr::bind_rows(results)
}
