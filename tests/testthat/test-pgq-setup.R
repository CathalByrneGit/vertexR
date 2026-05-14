# Tests for DuckPGQ setup: vx_pgq_available, vx_pgq_setup, vx_pgq_graph_name

setup_aviation_pgq <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")

  con <- DBI::dbConnect(duckdb::duckdb())

  # Check if DuckPGQ is available
  if (!vx_pgq_available(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    skip("DuckPGQ not available")
  }

  DBI::dbWriteTable(con, "airports", data.frame(
    airport_id  = c("DUB", "JFK", "LHR", "CDG"),
    name        = c("Dublin", "JFK", "Heathrow", "Charles de Gaulle"),
    country     = c("Ireland", "USA", "UK", "France"),
    capacity    = c(30L, 100L, 80L, 70L),
    utilization = c(0.95, 0.6, 0.85, 0.5),
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
          ontologySpecR::property_def("country", "string"),
          ontologySpecR::property_def("capacity", "integer"),
          ontologySpecR::property_def("utilization", "number")
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

  list(bundle = b, con = con)
}

teardown_aviation_pgq <- function(env) {
  DBI::dbDisconnect(env$con, shutdown = TRUE)
}


test_that("vx_pgq_available returns logical", {
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- vx_pgq_available(con)
  expect_type(result, "logical")
})


test_that("vx_pgq_available returns FALSE for non-DBI object", {
  expect_false(vx_pgq_available("not a connection"))
  expect_false(vx_pgq_available(NULL))
})


test_that("vx_pgq_graph_name converts bundle ID correctly", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::bundle(
    bundle_id = "my-test-bundle",
    bundle_version = "0.1.0"
  )
  expect_equal(vx_pgq_graph_name(b), "my_test_bundle")
})


test_that("vx_pgq_setup creates property graph", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  # Setup should not error
  expect_no_error(
    vx_pgq_setup(env$bundle, env$con)
  )
})


test_that("vx_pgq_setup with custom graph name works", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  expect_no_error(
    vx_pgq_setup(env$bundle, env$con, graph_name = "custom_graph")
  )
})


test_that("vx_pgq_setup with subset of vertex/edge tables works", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  expect_no_error(
    vx_pgq_setup(env$bundle, env$con,
                 graph_name = "subset_graph",
                 vertex_tables = c("Airport", "FlightRoute"),
                 edge_tables = c("RouteOrigin"))
  )
})
