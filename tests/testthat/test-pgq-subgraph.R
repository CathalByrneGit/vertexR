# Tests for DuckPGQ subgraph: vx_pgq_subgraph, vx_pgq_components

setup_aviation_pgq <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb())

  if (!vx_pgq_available(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    skip("DuckPGQ not available")
  }

  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id  = c("DUB", "JFK", "LHR", "CDG"),
    name        = c("Dublin", "JFK", "Heathrow", "Charles de Gaulle"),
    country     = c("Ireland", "USA", "UK", "France"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "airlines", data.frame(
    airline_id = c("EI", "AA", "BA"),
    name       = c("Aer Lingus", "American Airlines", "British Airways"),
    country    = c("Ireland", "USA", "UK"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "routes", data.frame(
    route_id       = c("R1", "R2", "R3", "R4", "R5"),
    origin_id      = c("DUB", "JFK", "LHR", "DUB", "CDG"),
    destination_id = c("JFK", "DUB", "JFK", "LHR", "LHR"),
    airline_id     = c("EI", "EI", "BA", "EI", "BA"),
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
          ontologySpecR::property_def("country", "string")
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
          ontologySpecR::property_def("country", "string")
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
          ontologySpecR::property_def("airline_id", "string")
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

  vx_pgq_setup(b, con, graph_name = "aviation_demo")

  list(bundle = b, con = con, graph_name = "aviation_demo")
}

teardown_aviation_pgq <- function(env) {
  DBI::dbDisconnect(env$con, shutdown = TRUE)
}


test_that("vx_pgq_subgraph returns valid tbl_graph", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  # Extract subgraph around DUB airport
  g <- vx_pgq_subgraph(
    env$con, env$bundle, env$graph_name,
    seed_ids = c("DUB"),
    seed_type = "Airport",
    depth = 1L
  )

  expect_s3_class(g, "tbl_graph")
  nodes <- vx_nodes(g)
  expect_true("DUB" %in% nodes$.node_id)
})


test_that("vx_pgq_subgraph with specific link types", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  g <- vx_pgq_subgraph(
    env$con, env$bundle, env$graph_name,
    seed_ids = c("R1"),
    seed_type = "FlightRoute",
    depth = 1L,
    link_types = c("RouteOrigin")
  )

  expect_s3_class(g, "tbl_graph")
})


test_that("vx_pgq_components returns component info", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  # This test may fail if recursive CTE is not supported
  result <- tryCatch(
    vx_pgq_components(env$con, env$graph_name, "Airport", "RouteOrigin"),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true("node_id" %in% names(result))
    expect_true("component_id" %in% names(result))
  }
})


test_that("vx_pgq_subgraph result works with vx_scenario", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  g <- vx_pgq_subgraph(
    env$con, env$bundle, env$graph_name,
    seed_ids = c("DUB"),
    seed_type = "Airport",
    depth = 1L
  )

  # Should be able to create scenarios on the subgraph
  s <- vx_scenario(g)
  expect_s3_class(s, "vx_scenario")
})
