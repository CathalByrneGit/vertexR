# Tests for graph construction: vertex_graph(), vx_nodes(), vx_edges()

# Helper: build the aviation bundle + DuckDB for tests
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

  # Build bundle using ontologySpecR
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


test_that("vertex_graph builds a graph with all object types", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  expect_s3_class(g, "tbl_graph")

  nodes <- vx_nodes(g)
  # 4 airports + 3 airlines + 5 routes = 12 nodes
  expect_equal(nrow(nodes), 12L)

  # Check .object_type column
  expect_true(".object_type" %in% names(nodes))
  expect_true(".node_id" %in% names(nodes))

  types <- unique(nodes$.object_type)
  expect_true("Airport" %in% types)
  expect_true("Airline" %in% types)
  expect_true("FlightRoute" %in% types)

  # Check node counts per type
  expect_equal(sum(nodes$.object_type == "Airport"), 4L)
  expect_equal(sum(nodes$.object_type == "Airline"), 3L)
  expect_equal(sum(nodes$.object_type == "FlightRoute"), 5L)
})


test_that("vertex_graph includes property columns on nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con, node_properties = TRUE)
  nodes <- vx_nodes(g)

  # Airport-specific columns should be present (as NA for non-airports)
  expect_true("capacity" %in% names(nodes))
  expect_true("utilization" %in% names(nodes))

  airports <- nodes[nodes$.object_type == "Airport", ]
  expect_equal(airports$.node_id[airports$name == "Dublin"], "DUB")
})


test_that("vertex_graph without properties only has .object_type and .node_id", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con, node_properties = FALSE)
  nodes <- vx_nodes(g)

  expect_true(".object_type" %in% names(nodes))
  expect_true(".node_id" %in% names(nodes))
  # Should not have domain columns
  expect_false("capacity" %in% names(nodes))
})


test_that("vertex_graph builds edges with .link_type", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  edges <- vx_edges(g)

  expect_true(".link_type" %in% names(edges))
  expect_true("from" %in% names(edges))
  expect_true("to" %in% names(edges))

  # 5 routes * 3 link types = 15 edges
  # (each route has origin, destination, operator)
  expect_equal(nrow(edges), 15L)

  link_types <- unique(edges$.link_type)
  expect_true("RouteOrigin" %in% link_types)
  expect_true("RouteDestination" %in% link_types)
  expect_true("RouteOperator" %in% link_types)
})


test_that("vertex_graph can subset object types", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con,
                    object_types = c("Airport", "FlightRoute"))
  nodes <- vx_nodes(g)

  types <- unique(nodes$.object_type)
  expect_true("Airport" %in% types)
  expect_true("FlightRoute" %in% types)
  expect_false("Airline" %in% types)

  # RouteOperator edges should be excluded (goes to Airline)
  edges <- vx_edges(g)
  expect_false("RouteOperator" %in% edges$.link_type)
})


test_that("vertex_graph can subset link types", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con,
                    link_types = c("RouteOrigin"))
  edges <- vx_edges(g)

  expect_equal(unique(edges$.link_type), "RouteOrigin")
  expect_equal(nrow(edges), 5L)
})


test_that("vx_nodes and vx_edges return tibbles", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  nodes <- vx_nodes(g)
  edges <- vx_edges(g)

  expect_s3_class(nodes, "tbl_df")
  expect_s3_class(edges, "tbl_df")
})


test_that("vertex_graph errors on invalid connection", {
  skip_if_not_installed("ontologySpecR")

  b <- ontologySpecR::bundle(
    bundle_id = "test",
    bundle_version = "0.1.0"
  )
  expect_error(vertex_graph(b, "not_a_connection"),
               "must be a DBI connection")
})


test_that("vertex_graph enforces max_nodes limit", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  # Our test data has 12 nodes total (4 airports + 3 airlines + 5 routes)
  # Setting max_nodes = 10 should trigger the guard
  expect_error(
    vertex_graph(env$bundle, env$con, max_nodes = 10L),
    "exceeds max_nodes"
  )
})


test_that("vertex_graph succeeds when within max_nodes limit", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  # max_nodes = 100 should be plenty
  g <- vertex_graph(env$bundle, env$con, max_nodes = 100L)
  expect_s3_class(g, "tbl_graph")
  expect_equal(nrow(vx_nodes(g)), 12L)
})


test_that("vertex_graph max_nodes can be disabled with Inf", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con, max_nodes = Inf)
  expect_s3_class(g, "tbl_graph")
})
