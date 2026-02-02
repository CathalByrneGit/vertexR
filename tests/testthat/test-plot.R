# Tests for visualization: vx_plot(), vx_plot_scenario()

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


test_that("vx_plot returns a visNetwork widget when available", {
  skip_if_not_installed("visNetwork")
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  p <- vx_plot(g, color_by = ".object_type", highlight = "DUB")

  expect_s3_class(p, "visNetwork")
})


test_that("vx_plot returns a ggplot when visNetwork is not available", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("ggplot2")
  # Only run if visNetwork is NOT installed (otherwise it takes precedence)
  skip_if(requireNamespace("visNetwork", quietly = TRUE),
          "visNetwork is installed; skipping ggraph fallback test")

  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  p <- vx_plot(g, color_by = ".object_type")

  expect_s3_class(p, "ggplot")
})


test_that("vx_plot with size_by works", {
  skip_if_not_installed("visNetwork")
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  p <- vx_plot(g, size_by = "capacity")

  expect_s3_class(p, "visNetwork")
})


test_that("vx_plot_scenario returns a visualization", {
  skip_if_not_installed("visNetwork")
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_remove_node("DUB")

  p <- vx_plot_scenario(g, s)
  expect_s3_class(p, "visNetwork")
})
