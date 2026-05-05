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


#' Define a Node Variable for Optimization
#'
#' Declares a variable on nodes for use in [vx_optimize()].
#'
#' @param name Character; the column name of the variable.
#' @param type Character; `"binary"`, `"integer"`, or `"continuous"`.
#' @param lower Numeric; lower bound (default 0).
#' @param upper Numeric; upper bound (default 1 for binary, Inf otherwise).
#'
#' @return An S3 object of class `"vx_node_var"`.
#' @export
vx_node_var <- function(name, type = "binary", lower = 0,
                        upper = if (type == "binary") 1 else Inf) {
  valid_types <- c("binary", "integer", "continuous")
  if (!type %in% valid_types) {
    abort(paste0("`type` must be one of: ",
                 paste(valid_types, collapse = ", ")))
  }
  structure(
    list(name = name, type = type, lower = lower, upper = upper),
    class = "vx_node_var"
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
#' Finds an assignment of edge/node variables that minimizes (or maximizes)
#' an objective function subject to node constraints. This is an
#' experimental module that uses a greedy local search approach.
#'
#' The greedy solver iteratively tries toggling each variable and accepts
#' changes that improve the objective while satisfying constraints.
#'
#' @param graph A `tbl_graph` produced by [vertex_graph()].
#' @param objective A function taking a `tbl_graph` and returning a
#'   numeric scalar to minimize (or maximize if `maximize = TRUE`).
#' @param variables A list of `vx_edge_var` or `vx_node_var` objects.
#' @param constraints A list of `vx_node_constraint` objects.
#' @param solver Character; solver backend. Currently only `"greedy"`
#'   is supported. `"glpk"` and `"highs"` are placeholders for
#'   future ROI/ompr integration.
#' @param max_iter Integer; maximum iterations for the greedy solver
#'   (default 100).
#' @param verbose Logical; if `TRUE`, print progress messages.
#' @param maximize Logical; if `TRUE`, maximize instead of minimize
#'   (default `FALSE`).
#'
#' @return A list of class `"vx_optimize_result"` with:
#'   \describe{
#'     \item{best_graph}{The graph at the best objective value found.}
#'     \item{best_objective}{Numeric; the best objective value.}
#'     \item{iterations}{Integer; number of iterations performed.}
#'     \item{history}{Data frame of (iteration, objective_value).}
#'     \item{variables}{Final variable assignments (named list).}
#'     \item{status}{Character; `"optimal"` if converged, `"feasible"` if
#'       max_iter reached, `"infeasible"` if constraints cannot be satisfied.}
#'     \item{solver}{Character; the solver used.}
#'   }
#'
#' @section Warning:
#' This module is experimental. The greedy solver does not guarantee
#' global optimality. For large graphs, consider using dedicated
#' optimization packages (ROI, ompr) directly.
#'
#' @export
vx_optimize <- function(graph, objective, variables = list(),
                        constraints = list(), solver = "greedy",
                        max_iter = 100L, verbose = FALSE,
                        maximize = FALSE) {
  if (!inherits(graph, "tbl_graph")) {
    abort("`graph` must be a tbl_graph object.")
  }
  if (!is.function(objective)) {
    abort("`objective` must be a function.")
  }

  nodes <- vx_nodes(graph)
  edges <- vx_edges(graph)
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)

  # Handle empty graph

  if (n_nodes == 0L) {
    result <- list(
      best_graph = graph,
      best_objective = objective(graph),
      iterations = 0L,
      history = data.frame(iteration = integer(0), objective_value = numeric(0)),
      variables = list(),
      status = "optimal",
      solver = solver
    )
    class(result) <- "vx_optimize_result"
    return(result)
  }

  # Separate edge and node variables
  edge_vars <- Filter(function(v) inherits(v, "vx_edge_var"), variables)
  node_vars <- Filter(function(v) inherits(v, "vx_node_var"), variables)

  # Initialize variables on edges
  for (v in edge_vars) {
    if (!v$name %in% names(edges)) {
      if (v$type == "binary") {
        edges[[v$name]] <- rep(1L, n_edges)
      } else {
        edges[[v$name]] <- rep(v$upper, n_edges)
      }
    }
  }

  # Initialize variables on nodes
  for (v in node_vars) {
    if (!v$name %in% names(nodes)) {
      if (v$type == "binary") {
        nodes[[v$name]] <- rep(1L, n_nodes)
      } else {
        nodes[[v$name]] <- rep(v$upper, n_nodes)
      }
    }
  }

  # Helper to build graph from current state
  build_graph <- function(nodes_df, edges_df) {
    tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df, directed = TRUE)
  }

  # Helper to check constraints
  check_constraints <- function(nodes_df, edges_df) {
    for (con in constraints) {
      con_nodes <- nodes_df[nodes_df$.object_type == con$object_type, ,
                            drop = FALSE]
      for (i in seq_len(nrow(con_nodes))) {
        node <- as.list(con_nodes[i, ])
        node_idx <- which(nodes_df$.node_id == node$.node_id)
        incident <- edges_df[edges_df$from == node_idx |
                              edges_df$to == node_idx, , drop = FALSE]
        if (!isTRUE(con$constraint_fn(node, incident))) {
          return(FALSE)
        }
      }
    }
    TRUE
  }

  # Check initial feasibility
  if (!check_constraints(nodes, edges)) {
    result <- list(
      best_graph = build_graph(nodes, edges),
      best_objective = NA_real_,
      iterations = 0L,
      history = data.frame(iteration = integer(0), objective_value = numeric(0)),
      variables = list(),
      status = "infeasible",
      solver = solver
    )
    class(result) <- "vx_optimize_result"
    return(result)
  }

  # Greedy solver
  if (solver == "greedy" || solver %in% c("glpk", "highs")) {
    if (solver != "greedy") {
      warn(paste0("Solver '", solver, "' is not yet supported. ",
                  "Falling back to greedy solver."))
    }

    current_g <- build_graph(nodes, edges)
    best_obj <- objective(current_g)
    best_nodes <- nodes
    best_edges <- edges
    history <- data.frame(iteration = 0L, objective_value = best_obj)
    iterations <- 0L

    for (iter in seq_len(max_iter)) {
      improved <- FALSE
      iterations <- iter

      # Try toggling each binary edge variable
      for (v in edge_vars) {
        if (v$type != "binary" || n_edges == 0L) next
        for (e_idx in seq_len(n_edges)) {
          old_val <- edges[[v$name]][e_idx]
          new_val <- 1L - old_val
          edges[[v$name]][e_idx] <- new_val

          if (check_constraints(nodes, edges)) {
            trial_g <- build_graph(nodes, edges)
            trial_obj <- objective(trial_g)
            is_better <- if (maximize) {
              trial_obj > best_obj
            } else {
              trial_obj < best_obj
            }
            if (is_better) {
              best_obj <- trial_obj
              best_nodes <- nodes
              best_edges <- edges
              improved <- TRUE
              if (verbose) {
                message(sprintf("Iter %d: improved to %.4f (edge %d, var %s)",
                                iter, best_obj, e_idx, v$name))
              }
            } else {
              edges[[v$name]][e_idx] <- old_val
            }
          } else {
            edges[[v$name]][e_idx] <- old_val
          }
        }
      }

      # Try toggling each binary node variable
      for (v in node_vars) {
        if (v$type != "binary") next
        for (n_idx in seq_len(n_nodes)) {
          old_val <- nodes[[v$name]][n_idx]
          new_val <- 1L - old_val
          nodes[[v$name]][n_idx] <- new_val

          if (check_constraints(nodes, edges)) {
            trial_g <- build_graph(nodes, edges)
            trial_obj <- objective(trial_g)
            is_better <- if (maximize) {
              trial_obj > best_obj
            } else {
              trial_obj < best_obj
            }
            if (is_better) {
              best_obj <- trial_obj
              best_nodes <- nodes
              best_edges <- edges
              improved <- TRUE
              if (verbose) {
                message(sprintf("Iter %d: improved to %.4f (node %d, var %s)",
                                iter, best_obj, n_idx, v$name))
              }
            } else {
              nodes[[v$name]][n_idx] <- old_val
            }
          } else {
            nodes[[v$name]][n_idx] <- old_val
          }
        }
      }

      history <- rbind(history, data.frame(iteration = iter,
                                            objective_value = best_obj))

      if (!improved) {
        if (verbose) message("Converged at iteration ", iter)
        break
      }
    }

    # Collect final variable values
    final_vars <- list()
    for (v in edge_vars) {
      final_vars[[paste0("edge_", v$name)]] <- best_edges[[v$name]]
    }
    for (v in node_vars) {
      final_vars[[paste0("node_", v$name)]] <- best_nodes[[v$name]]
    }

    status <- if (iterations < max_iter) "optimal" else "feasible"

    result <- list(
      best_graph = build_graph(best_nodes, best_edges),
      best_objective = best_obj,
      iterations = iterations,
      history = history,
      variables = final_vars,
      status = status,
      solver = "greedy"
    )
    class(result) <- "vx_optimize_result"
    return(result)
  }

  abort(paste0("Unknown solver: '", solver, "'"))
}


#' @export
print.vx_optimize_result <- function(x, ...) {
  cat("<vx_optimize_result>\n")
  cat("  Status:          ", x$status, "\n")
  cat("  Best objective:  ", x$best_objective, "\n")
  cat("  Iterations:      ", x$iterations, "\n")
  cat("  Solver:          ", x$solver, "\n")
  cat("  Variables:       ", length(x$variables), "\n")
  invisible(x)
}
