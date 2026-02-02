# Tests for scenarios: vx_scenario(), vx_modify_node(), vx_remove_node(),
#   vx_add_edge(), vx_apply_scenario(), vx_compare()

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


test_that("vx_scenario creates a scenario object", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g)

  expect_s3_class(s, "vx_scenario")
  expect_identical(s$original, g)
  expect_equal(length(s$modifications), 0L)
})


test_that("vx_modify_node records property changes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_modify_node("DUB", capacity = 0L, utilization = 0.0)

  expect_equal(length(s$node_changes), 1L)
  expect_equal(s$node_changes[["DUB"]]$capacity, 0L)
  expect_equal(s$node_changes[["DUB"]]$utilization, 0.0)
})


test_that("vx_modify_node applies changes correctly", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_modify_node("DUB", capacity = 0L, utilization = 0.0)
  g2 <- vx_apply_scenario(s)

  nodes <- vx_nodes(g2)
  dub <- nodes[nodes$.node_id == "DUB", ]
  expect_equal(dub$capacity, 0L)
  expect_equal(dub$utilization, 0.0)
})


test_that("vx_remove_node removes node and incident edges", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  orig_nodes <- vx_nodes(g)
  orig_edges <- vx_edges(g)

  s <- vx_scenario(g) |>
    vx_remove_node("DUB")
  g2 <- vx_apply_scenario(s)

  mod_nodes <- vx_nodes(g2)
  mod_edges <- vx_edges(g2)

  # DUB should be gone
  expect_false("DUB" %in% mod_nodes$.node_id)
  expect_equal(nrow(mod_nodes), nrow(orig_nodes) - 1L)

  # Edges to/from DUB should be removed
  expect_lt(nrow(mod_edges), nrow(orig_edges))
})


test_that("vx_add_edge adds a new edge", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  orig_edges <- vx_edges(g)

  s <- vx_scenario(g) |>
    vx_add_edge("R99", from = "JFK", to = "LHR", .link_type = "RouteOrigin")
  g2 <- vx_apply_scenario(s)
  mod_edges <- vx_edges(g2)

  expect_equal(nrow(mod_edges), nrow(orig_edges) + 1L)
})


test_that("vx_apply_scenario returns a valid tbl_graph", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_modify_node("DUB", capacity = 0L) |>
    vx_remove_node("CDG")
  g2 <- vx_apply_scenario(s)

  expect_s3_class(g2, "tbl_graph")
  nodes <- vx_nodes(g2)
  expect_false("CDG" %in% nodes$.node_id)
  expect_true("DUB" %in% nodes$.node_id)
  dub <- nodes[nodes$.node_id == "DUB", ]
  expect_equal(dub$capacity, 0L)
})


test_that("vx_compare detects removed nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_remove_node("DUB")
  g2 <- vx_apply_scenario(s)

  diff <- vx_compare(g, g2)

  expect_s3_class(diff, "tbl_df")
  expect_true("element" %in% names(diff))
  expect_true("change" %in% names(diff))

  removed_nodes <- diff[diff$element == "node" & diff$change == "removed", ]
  expect_true("DUB" %in% removed_nodes$id)

  # Edges should also be removed
  removed_edges <- diff[diff$element == "edge" & diff$change == "removed", ]
  expect_true(nrow(removed_edges) > 0L)
})


test_that("vx_compare detects modified nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_modify_node("DUB", capacity = 999L)
  g2 <- vx_apply_scenario(s)

  diff <- vx_compare(g, g2)
  modified <- diff[diff$element == "node" & diff$change == "modified", ]
  expect_true("DUB" %in% modified$id)
  expect_true(grepl("capacity", modified$details[modified$id == "DUB"]))
})


test_that("vx_compare detects added edges", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_add_edge("R99", from = "JFK", to = "CDG", .link_type = "RouteOrigin")
  g2 <- vx_apply_scenario(s)

  diff <- vx_compare(g, g2)
  added_edges <- diff[diff$element == "edge" & diff$change == "added", ]
  expect_true(nrow(added_edges) > 0L)
})


test_that("vx_compare returns empty tibble for identical graphs", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g)
  g2 <- vx_apply_scenario(s)

  diff <- vx_compare(g, g2)
  expect_equal(nrow(diff), 0L)
})


test_that("scenarios are composable via piping", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  s <- vx_scenario(g) |>
    vx_modify_node("DUB", status = "closed", capacity = 0L) |>
    vx_remove_node("CDG") |>
    vx_add_edge("R99", from = "JFK", to = "LHR", .link_type = "RouteOrigin")

  expect_equal(length(s$modifications), 3L)

  g2 <- vx_apply_scenario(s)
  expect_s3_class(g2, "tbl_graph")

  nodes <- vx_nodes(g2)
  expect_false("CDG" %in% nodes$.node_id)
  expect_true("DUB" %in% nodes$.node_id)
})
