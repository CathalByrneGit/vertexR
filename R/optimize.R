#' Define an Edge Variable for Optimization
#'
#' Declares a variable on edges for use in [vx_optimize()].
#'
#' @param name Character; the column name of the variable.
#' @param type Character; `"binary"`, `"integer"`, or `"continuous"`.
#' @param lower Numeric; lower bound (default 0).
#' @param upper Numeric; upper bound (default 1 for binary, Inf otherwise).
#'
#' @return An S3 object of class `"vx_edge_var"`.
#' @export
vx_edge_var <- function(name, type = "binary", lower = 0,
                        upper = if (type == "binary") 1 else Inf) {
  valid_types <- c("binary", "integer", "continuous")
  if (!type %in% valid_types) {
    abort(paste0("`type` must be one of: ",
                 paste(valid_types, collapse = ", ")))
  }
  structure(
    list(name = name, type = type, lower = lower, upper = upper),
    class = "vx_edge_var"
  )
}


#' Define a Node Constraint for Optimization
#'
#' Declares a constraint on nodes of a specific type.
#'
#' @param object_type Character; the object type this constraint applies to.
#' @param constraint_fn A function taking `(node, edges)` and returning
#'   `TRUE` if the constraint is satisfied.
#'
#' @return An S3 object of class `"vx_node_constraint"`.
#' @export
vx_node_constraint <- function(object_type, constraint_fn) {
  if (!is.function(constraint_fn)) {
    abort("`constraint_fn` must be a function.")
  }
  structure(
    list(object_type = object_type, constraint_fn = constraint_fn),
    class = "vx_node_constraint"
  )
}


#' Optimize Over a Graph (Experimental)
#'
#' Finds an assignment of edge variables that minimizes (or maximizes)
#' an objective function subject to node constraints. This is an
#' experimental module that uses a greedy/brute-force approach for
#' small graphs.
#'
#' @param graph A `tbl_graph` produced by [vertex_graph()].
#' @param objective A function taking a `tbl_graph` and returning a
#'   numeric scalar to minimize.
#' @param variables A list of `vx_edge_var` objects.
#' @param constraints A list of `vx_node_constraint` objects.
#' @param solver Character; solver backend. Currently only `"greedy"`
#'   is fully supported. `"glpk"` and `"highs"` are placeholders for
#'   future ROI/ompr integration.
#' @param maximize Logical; if `TRUE`, maximize instead of minimize
#'   (default `FALSE`).
#'
#' @return A list with:
#'   - `graph`: the optimized `tbl_graph`
#'   - `objective_value`: numeric objective at the solution
#'   - `iterations`: number of iterations performed
#'   - `solver`: the solver used
#'   - `status`: `"optimal"`, `"feasible"`, or `"infeasible"`
#'
#' @section Warning:
#' This module is experimental. The greedy solver does not guarantee
#' global optimality. For large graphs, consider using dedicated
#' optimization packages (ROI, ompr) directly.
#'
#' @export
vx_optimize <- function(graph, objective, variables = list(),
                        constraints = list(), solver = "greedy",
                        maximize = FALSE) {
  if (!inherits(graph, "tbl_graph")) {
    abort("`graph` must be a tbl_graph object.")
  }
  if (!is.function(objective)) {
    abort("`objective` must be a function.")
  }

  edges <- vx_edges(graph)
  nodes <- vx_nodes(graph)
  n_edges <- nrow(edges)

  if (n_edges == 0L) {
    return(list(
      graph = graph,
      objective_value = objective(graph),
      iterations = 0L,
      solver = solver,
      status = "optimal"
    ))
  }

  # Initialize edge variables
  var_names <- vapply(variables, function(v) v$name, character(1))
  for (v in variables) {
    if (!v$name %in% names(edges)) {
      # Initialize: for binary, start with all 1s
      if (v$type == "binary") {
        edges[[v$name]] <- rep(1L, n_edges)
      } else {
        edges[[v$name]] <- rep(v$upper, n_edges)
      }
    }
  }

  # Helper to check constraints
  check_constraints <- function(nodes_df, edges_df) {
    for (con in constraints) {
      con_nodes <- nodes_df[nodes_df$.object_type == con$object_type, ,
                            drop = FALSE]
      for (i in seq_len(nrow(con_nodes))) {
        node <- as.list(con_nodes[i, ])
        node_idx <- which(nodes_df$.node_id == node$.node_id)
        # Get incident edges
        incident <- edges_df[edges_df$from == node_idx |
                              edges_df$to == node_idx, , drop = FALSE]
        if (!isTRUE(con$constraint_fn(node, incident))) {
          return(FALSE)
        }
      }
    }
    TRUE
  }

  # Greedy solver: try toggling each binary variable
  if (solver == "greedy") {
    best_obj <- NULL
    best_edges <- edges
    iterations <- 0L

    # Build the graph with current edges to evaluate
    build_graph <- function(edges_df) {
      tidygraph::tbl_graph(nodes = nodes, edges = edges_df, directed = TRUE)
    }

    current_g <- build_graph(edges)
    best_obj <- objective(current_g)

    improved <- TRUE
    while (improved) {
      improved <- FALSE
      for (v in variables) {
        if (v$type != "binary") next
        for (e_idx in seq_len(n_edges)) {
          iterations <- iterations + 1L
          old_val <- edges[[v$name]][e_idx]
          new_val <- 1L - old_val
          edges[[v$name]][e_idx] <- new_val

          # Check constraints
          if (check_constraints(nodes, edges)) {
            trial_g <- build_graph(edges)
            trial_obj <- objective(trial_g)
            is_better <- if (maximize) trial_obj > best_obj else trial_obj < best_obj
            if (is_better) {
              best_obj <- trial_obj
              best_edges <- edges
              improved <- TRUE
            } else {
              edges[[v$name]][e_idx] <- old_val
            }
          } else {
            edges[[v$name]][e_idx] <- old_val
          }
        }
      }
    }

    final_g <- build_graph(best_edges)
    feasible <- check_constraints(nodes, best_edges)

    return(list(
      graph = final_g,
      objective_value = best_obj,
      iterations = iterations,
      solver = "greedy",
      status = if (feasible) "feasible" else "infeasible"
    ))
  }

  # Fallback for unsupported solvers
  warn(paste0("Solver '", solver, "' is not yet supported. ",
              "Falling back to greedy solver."))
  vx_optimize(graph, objective, variables, constraints,
              solver = "greedy", maximize = maximize)
}
