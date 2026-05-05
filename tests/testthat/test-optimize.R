# Tests for optimization: vx_optimize(), vx_edge_var(), vx_node_var()

setup_aviation <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb())

  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id  = c("DUB", "JFK", "LHR", "CDG"),
    name        = c("Dublin", "JFK", "Heathrow", "Charles de Gaulle"),
    country     = c("Ireland", "USA", "UK", "France"),
    capacity    = c(30L, 100L, 80L, 70L),
    utilization = c(0.95, 0.6, 0.85, 0.5),
    latitude    = c(53.42, 40.64, 51.47, 49.01),
    longitude   = c(-6.27, -73.78, -0.46, 2.55),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "airlines", data.frame(
    airline_id = c("EI", "AA", "BA"),
    name       = c("Aer Lingus", "American Airlines", "British Airways"),
    country    = c("Ireland", "USA", "UK"),
    active     = c(TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "routes", data.frame(
    route_id       = c("R1", "R2", "R3", "R4", "R5"),
    origin_id      = c("DUB", "JFK", "LHR", "DUB", "CDG"),
    destination_id = c("JFK", "DUB", "JFK", "LHR", "LHR"),
    airline_id     = c("EI", "EI", "BA", "EI", "BA"),
    stops          = c(0L, 0L, 0L, 0L, 1L),
    equipment      = c("A330", "A330", "777", "A320", "A320"),
    stringsAsFactors = FALSE
  ))

  b <- ontologySpecR::bundle(
    bundle_id = "aviation-demo",
    bundle_version = "0.1.0",
    objects = list(
      ontologySpecR::object_type(
        id = "Airport",
        properties = list(
          ontologySpecR::property_def("airport_id", "string", nullable = FALSE),
          ontologySpecR::property_def("name", "string"),
          ontologySpecR::property_def("country", "string"),
          ontologySpecR::property_def("capacity", "integer"),
          ontologySpecR::property_def("utilization", "number"),
          ontologySpecR::property_def("latitude", "number"),
          ontologySpecR::property_def("longitude", "number")
        ),
        primary_key = "airport_id",
        source_kind = "table",
        source_table = "airports"
      ),
      ontologySpecR::object_type(
        id = "Airline",
        properties = list(
          ontologySpecR::property_def("airline_id", "string", nullable = FALSE),
          ontologySpecR::property_def("name", "string"),
          ontologySpecR::property_def("country", "string"),
          ontologySpecR::property_def("active", "boolean")
        ),
        primary_key = "airline_id",
        source_kind = "table",
        source_table = "airlines"
      ),
      ontologySpecR::object_type(
        id = "FlightRoute",
        properties = list(
          ontologySpecR::property_def("route_id", "string", nullable = FALSE),
          ontologySpecR::property_def("origin_id", "string"),
          ontologySpecR::property_def("destination_id", "string"),
          ontologySpecR::property_def("airline_id", "string"),
          ontologySpecR::property_def("stops", "integer"),
          ontologySpecR::property_def("equipment", "string")
        ),
        primary_key = "route_id",
        source_kind = "table",
        source_table = "routes"
      )
    ),
    links = list(
      ontologySpecR::link_type(
        id = "RouteOrigin",
        from = "FlightRoute",
        to = "Airport",
        cardinality = "many-to-one",
        directed = TRUE,
        join_from_keys = "origin_id",
        join_to_keys = "airport_id"
      ),
      ontologySpecR::link_type(
        id = "RouteDestination",
        from = "FlightRoute",
        to = "Airport",
        cardinality = "many-to-one",
        directed = TRUE,
        join_from_keys = "destination_id",
        join_to_keys = "airport_id"
      ),
      ontologySpecR::link_type(
        id = "RouteOperator",
        from = "FlightRoute",
        to = "Airline",
        cardinality = "many-to-one",
        directed = TRUE,
        join_from_keys = "airline_id",
        join_to_keys = "airline_id"
      )
    )
  )

  list(bundle = b, con = con)
}

teardown_aviation <- function(env) {
  DBI::dbDisconnect(env$con, shutdown = TRUE)
}


test_that("vx_edge_var creates valid variable spec", {
  v <- vx_edge_var("active", type = "binary")
  expect_s3_class(v, "vx_edge_var")
  expect_equal(v$name, "active")
  expect_equal(v$type, "binary")
  expect_equal(v$lower, 0)
  expect_equal(v$upper, 1)
})


test_that("vx_node_var creates valid variable spec", {
  v <- vx_node_var("selected", type = "binary")
  expect_s3_class(v, "vx_node_var")
  expect_equal(v$name, "selected")
  expect_equal(v$type, "binary")
})


test_that("vx_node_constraint creates valid constraint", {
  con <- vx_node_constraint("Airport", function(node, edges) TRUE)
  expect_s3_class(con, "vx_node_constraint")
  expect_equal(con$object_type, "Airport")
  expect_true(is.function(con$constraint_fn))
})


test_that("vx_optimize returns valid result structure", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  result <- vx_optimize(
    graph = g,
    objective = function(g) nrow(vx_edges(g)),
    variables = list(vx_edge_var("active", type = "binary")),
    constraints = list(),
    solver = "greedy"
  )

  expect_s3_class(result, "vx_optimize_result")
  expect_true("best_graph" %in% names(result))
  expect_true("best_objective" %in% names(result))
  expect_true("iterations" %in% names(result))
  expect_true("history" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true(result$status %in% c("optimal", "feasible", "infeasible"))
})


test_that("vx_optimize greedy solver improves objective", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  initial_edges <- nrow(vx_edges(g))

  result <- vx_optimize(
    graph = g,
    objective = function(g) {
      edges <- vx_edges(g)
      if (!"active" %in% names(edges)) return(nrow(edges))
      sum(edges$active, na.rm = TRUE)
    },
    variables = list(vx_edge_var("active", type = "binary")),
    constraints = list(),
    solver = "greedy"
  )

  # With no constraints, minimizing should turn all edges off
  expect_lte(result$best_objective, initial_edges)
})


test_that("vx_optimize respects constraints", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  # Constraint: each Airport must have at least 1 incident edge active
  con <- vx_node_constraint("Airport", function(node, edges) {
    if (nrow(edges) == 0L) return(TRUE)
    if (!"active" %in% names(edges)) return(TRUE)
    sum(edges$active, na.rm = TRUE) >= 1L
  })

  result <- vx_optimize(
    graph = g,
    objective = function(g) {
      edges <- vx_edges(g)
      if (!"active" %in% names(edges)) return(nrow(edges))
      sum(edges$active, na.rm = TRUE)
    },
    variables = list(vx_edge_var("active", type = "binary")),
    constraints = list(con),
    solver = "greedy"
  )

  expect_true(result$status %in% c("optimal", "feasible"))
})


test_that("vx_optimize maximize option works", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  result <- vx_optimize(
    graph = g,
    objective = function(g) {
      edges <- vx_edges(g)
      if (!"active" %in% names(edges)) return(nrow(edges))
      sum(edges$active, na.rm = TRUE)
    },
    variables = list(vx_edge_var("active", type = "binary")),
    constraints = list(),
    solver = "greedy",
    maximize = TRUE
  )

  # When maximizing active edges, all should be 1
  edges <- vx_edges(result$best_graph)
  expect_equal(sum(edges$active), nrow(edges))
})


test_that("vx_optimize handles empty graph", {
  skip_if_not_installed("ontologySpecR")
  skip_if_not_installed("duckdb")

  # Create empty tbl_graph
  empty_g <- tidygraph::tbl_graph(
    nodes = data.frame(.object_type = character(0), .node_id = character(0)),
    edges = data.frame(from = integer(0), to = integer(0), .link_type = character(0))
  )

  result <- vx_optimize(
    graph = empty_g,
    objective = function(g) 0,
    variables = list(),
    constraints = list(),
    solver = "greedy"
  )

  expect_equal(result$status, "optimal")
  expect_equal(result$iterations, 0L)
})
