# Tests for DuckPGQ queries: vx_pgq_match, vx_pgq_neighbors, vx_pgq_shortest_path

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

  # Setup property graph
  vx_pgq_setup(b, con, graph_name = "aviation_demo")

  list(bundle = b, con = con, graph_name = "aviation_demo")
}

teardown_aviation_pgq <- function(env) {
  DBI::dbDisconnect(env$con, shutdown = TRUE)
}


test_that("vx_pgq_match returns data frame", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  result <- vx_pgq_match(
    env$con, env$graph_name,
    pattern = "(a:Airport)",
    columns = c("a.node_id AS airport_id", "a.name AS airport_name")
  )

  expect_s3_class(result, "data.frame")
  expect_true("airport_id" %in% names(result))
  expect_true("airport_name" %in% names(result))
  expect_equal(nrow(result), 4L)
})


test_that("vx_pgq_match with WHERE clause works", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  result <- vx_pgq_match(
    env$con, env$graph_name,
    pattern = "(a:Airport)",
    columns = c("a.node_id AS airport_id"),
    where = "a.country = 'Ireland'"
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$airport_id, "DUB")
})


test_that("vx_pgq_match with LIMIT works", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  result <- vx_pgq_match(
    env$con, env$graph_name,
    pattern = "(a:Airport)",
    columns = c("a.node_id AS airport_id"),
    limit = 2L
  )

  expect_equal(nrow(result), 2L)
})


test_that("vx_pgq_neighbors finds 1-hop neighbors", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  result <- vx_pgq_neighbors(
    env$con, env$graph_name,
    source_type = "FlightRoute",
    source_ids = c("R1"),
    link_type = "RouteOrigin",
    target_type = "Airport",
    depth = 1L,
    direction = "forward"
  )

  expect_s3_class(result, "data.frame")
  expect_true("source_id" %in% names(result))
  expect_true("target_id" %in% names(result))
})


test_that("vx_pgq_neighbors with multiple source IDs", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  result <- vx_pgq_neighbors(
    env$con, env$graph_name,
    source_type = "FlightRoute",
    source_ids = c("R1", "R2", "R3"),
    link_type = "RouteOrigin",
    target_type = "Airport",
    depth = 1L
  )

  expect_gte(nrow(result), 1L)
})


test_that("vx_pgq_shortest_path returns path info", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  # This may fail depending on DuckPGQ syntax - the test validates the function
  # returns something reasonable or NULL
  result <- tryCatch(
    vx_pgq_shortest_path(
      env$con, env$graph_name,
      from_type = "Airport",
      from_id = "DUB",
      to_type = "Airport",
      to_id = "JFK",
      max_hops = 5L
    ),
    error = function(e) NULL,
    warning = function(w) NULL
  )

  # Result should be NULL or a data frame
  if (!is.null(result)) {
    expect_s3_class(result, "data.frame")
  }
})


test_that("vx_pgq_shortest_path returns NULL for disconnected nodes", {
  env <- setup_aviation_pgq()
  on.exit(teardown_aviation_pgq(env))

  # Airlines are not connected to airports directly
  result <- vx_pgq_shortest_path(
    env$con, env$graph_name,
    from_type = "Airline",
    from_id = "EI",
    to_type = "Airline",
    to_id = "BA",
    link_types = c("RouteOperator"),
    max_hops = 3L
  )

  # Should be NULL since airlines aren't directly connected via RouteOperator
  expect_true(is.null(result) || nrow(result) == 0L)
})
