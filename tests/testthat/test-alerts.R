# Tests for alert rules: vx_alert_rule(), vx_evaluate_alerts(), vx_propagate_risk()

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


test_that("vx_alert_rule creates a valid rule object", {
  rule <- vx_alert_rule(
    id = "test_rule",
    description = "A test rule",
    condition = function(g) character(0),
    severity = "high"
  )

  expect_s3_class(rule, "vx_alert_rule")
  expect_equal(rule$id, "test_rule")
  expect_equal(rule$severity, "high")
  expect_true(is.function(rule$condition))
})


test_that("vx_alert_rule validates severity", {
  expect_error(
    vx_alert_rule("x", "desc", function(g) character(0), severity = "banana"),
    "severity"
  )
})


test_that("vx_alert_rule validates condition is a function", {
  expect_error(
    vx_alert_rule("x", "desc", "not a function"),
    "must be a function"
  )
})


test_that("vx_evaluate_alerts returns correct flagged nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  rule <- vx_alert_rule(
    id = "over_capacity",
    description = "Airports over 90% utilization",
    condition = function(g) {
      nodes <- vx_nodes(g)
      airports <- nodes[nodes$.object_type == "Airport", ]
      airports[airports$utilization > 0.9, ]$.node_id
    },
    severity = "critical"
  )

  alerts <- vx_evaluate_alerts(g, list(rule))

  expect_s3_class(alerts, "tbl_df")
  expect_true("rule_id" %in% names(alerts))
  expect_true("severity" %in% names(alerts))
  expect_true("node_id" %in% names(alerts))
  expect_true("evaluated_at" %in% names(alerts))

  # Only DUB has utilization > 0.9
  expect_equal(nrow(alerts), 1L)
  expect_equal(alerts$node_id, "DUB")
  expect_equal(alerts$rule_id, "over_capacity")
  expect_equal(alerts$severity, "critical")
})


test_that("vx_evaluate_alerts returns empty tibble when no flags", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  rule <- vx_alert_rule(
    id = "impossible",
    description = "Never triggers",
    condition = function(g) character(0),
    severity = "low"
  )

  alerts <- vx_evaluate_alerts(g, list(rule))
  expect_equal(nrow(alerts), 0L)
  expect_true(all(c("rule_id", "severity", "node_id", "evaluated_at") %in%
                    names(alerts)))
})


test_that("vx_evaluate_alerts handles multiple rules", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)

  rule1 <- vx_alert_rule(
    id = "over_90",
    description = "Over 90%",
    condition = function(g) {
      nodes <- vx_nodes(g)
      a <- nodes[nodes$.object_type == "Airport", ]
      a[a$utilization > 0.9, ]$.node_id
    },
    severity = "critical"
  )
  rule2 <- vx_alert_rule(
    id = "over_80",
    description = "Over 80%",
    condition = function(g) {
      nodes <- vx_nodes(g)
      a <- nodes[nodes$.object_type == "Airport", ]
      a[a$utilization > 0.8, ]$.node_id
    },
    severity = "high"
  )

  alerts <- vx_evaluate_alerts(g, list(rule1, rule2))
  # rule1: DUB, rule2: DUB + LHR
  expect_equal(nrow(alerts), 3L)
  expect_true("DUB" %in% alerts$node_id)
  expect_true("LHR" %in% alerts$node_id)
})


test_that("vx_propagate_risk finds downstream nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  propagated <- vx_propagate_risk(g, "DUB", depth = 2)

  expect_s3_class(propagated, "tbl_df")
  expect_true("node_id" %in% names(propagated))
  expect_true("depth" %in% names(propagated))
  expect_true("source_node" %in% names(propagated))

  # DUB is connected to routes R1, R2, R4 (at depth 1)
  # Those routes connect to JFK, LHR, EI (at depth 2)
  expect_true(nrow(propagated) > 0L)
  expect_true(all(propagated$depth > 0L))
  expect_true(all(propagated$depth <= 2L))
})


test_that("vx_propagate_risk returns empty for isolated nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  # Build graph with only airports (no edges)
  g <- vertex_graph(env$bundle, env$con, object_types = "Airport")
  propagated <- vx_propagate_risk(g, "DUB", depth = 2)

  expect_equal(nrow(propagated), 0L)
})


test_that("vx_propagate_risk returns empty for unknown nodes", {
  env <- setup_aviation()
  on.exit(teardown_aviation(env))

  g <- vertex_graph(env$bundle, env$con)
  propagated <- vx_propagate_risk(g, "UNKNOWN_NODE", depth = 2)

  expect_equal(nrow(propagated), 0L)
})
