# Tests for subgraph operations: vx_subgraph(), vx_neighbors(), vx_shortest_path()

# Reuse the aviation setup helper from test-graph.R
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


test_that("vx_subgraph filters to a single object type", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  sub <- vx_subgraph(g, "Airport")
  sub_nodes <- vx_nodes(sub)

  expect_equal(nrow(sub_nodes), 4L)
  expect_true(all(sub_nodes$.object_type == "Airport"))
})


test_that("vx_subgraph with no matching type gives warning", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  expect_warning(
    sub <- vx_subgraph(g, "NonExistent"),
    "No nodes found"
  )
})


test_that("vx_neighbors returns nodes within depth 1", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  nbrs <- vx_neighbors(g, "DUB", depth = 1)
  nbr_nodes <- vx_nodes(nbrs)

  # DUB itself + direct neighbors (routes R1, R2, R4 connect to DUB)
  expect_true("DUB" %in% nbr_nodes$.node_id)
  # Routes that have DUB as origin or destination
  expect_true(nrow(nbr_nodes) > 1L)
})


test_that("vx_neighbors at depth 2 includes more nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  nbrs1 <- vx_neighbors(g, "DUB", depth = 1)
  nbrs2 <- vx_neighbors(g, "DUB", depth = 2)

  expect_gte(nrow(vx_nodes(nbrs2)), nrow(vx_nodes(nbrs1)))
})


test_that("vx_neighbors errors on unknown node", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  expect_error(
    vx_neighbors(g, "UNKNOWN"),
    "not found"
  )
})


test_that("vx_shortest_path finds a path between connected nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  path <- vx_shortest_path(g, "DUB", "JFK")

  expect_true(nrow(path) >= 2L)
  expect_equal(path$.node_id[1], "DUB")
  expect_equal(path$.node_id[nrow(path)], "JFK")
})


test_that("vx_shortest_path returns empty for disconnected nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  # Build a graph with only airports (no edges between them)
  g <- vertex_graph(env$bundle, env$con,
                    object_types = "Airport")
  path <- vx_shortest_path(g, "DUB", "CDG")|>
    expect_warning(regexp = "At vendor/cigraph/src/paths/unweighted.c:444 : Couldn't reach some vertices.")

  
})


test_that("vx_shortest_path errors on unknown nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  expect_error(vx_shortest_path(g, "UNKNOWN", "JFK"), "not found")
  expect_error(vx_shortest_path(g, "DUB", "UNKNOWN"), "not found")
})
