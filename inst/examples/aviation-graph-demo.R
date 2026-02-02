#!/usr/bin/env Rscript
#
# aviation-graph-demo.R
#
# Demonstrates all four vertexR modules using the aviation domain:
#   1. Graph Construction
#   2. Alert Rules
#   3. What-If Simulation
#   4. Optimization (experimental)
#
# Requirements: ontologySpecR, vertexR, duckdb

library(ontologySpecR)
library(vertexR)
library(dplyr, warn.conflicts = FALSE)

cat("=== vertexR Aviation Demo ===\n\n")

# --------------------------------------------------------------------------
# Setup: define the ontology bundle and populate a DuckDB
# --------------------------------------------------------------------------

b <- bundle(
  bundle_id = "aviation-demo",
  bundle_version = "0.1.0",
  objects = list(
    object_type(
      id = "Airport",
      properties = list(
        property_def("airport_id", "string", nullable = FALSE),
        property_def("name", "string"),
        property_def("country", "string"),
        property_def("capacity", "integer"),
        property_def("utilization", "number"),
        property_def("latitude", "number"),
        property_def("longitude", "number")
      ),
      primary_key = "airport_id",
      source_kind = "table",
      source_table = "airports"
    ),
    object_type(
      id = "Airline",
      properties = list(
        property_def("airline_id", "string", nullable = FALSE),
        property_def("name", "string"),
        property_def("country", "string"),
        property_def("active", "boolean")
      ),
      primary_key = "airline_id",
      source_kind = "table",
      source_table = "airlines"
    ),
    object_type(
      id = "FlightRoute",
      properties = list(
        property_def("route_id", "string", nullable = FALSE),
        property_def("origin_id", "string"),
        property_def("destination_id", "string"),
        property_def("airline_id", "string"),
        property_def("stops", "integer"),
        property_def("equipment", "string")
      ),
      primary_key = "route_id",
      source_kind = "table",
      source_table = "routes"
    )
  ),
  links = list(
    link_type(
      id = "RouteOrigin",
      from = "FlightRoute", to = "Airport",
      cardinality = "many-to-one", directed = TRUE,
      join_from_keys = "origin_id", join_to_keys = "airport_id"
    ),
    link_type(
      id = "RouteDestination",
      from = "FlightRoute", to = "Airport",
      cardinality = "many-to-one", directed = TRUE,
      join_from_keys = "destination_id", join_to_keys = "airport_id"
    ),
    link_type(
      id = "RouteOperator",
      from = "FlightRoute", to = "Airline",
      cardinality = "many-to-one", directed = TRUE,
      join_from_keys = "airline_id", join_to_keys = "airline_id"
    )
  )
)

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

# --------------------------------------------------------------------------
# Module 1: Graph Construction
# --------------------------------------------------------------------------

cat("--- Module 1: Graph Construction ---\n")
g <- vertex_graph(b, con)
cat("Built graph:\n")
print(g)

cat("\nNodes:\n")
print(vx_nodes(g))

cat("\nEdges:\n")
print(vx_edges(g))

# Subgraph: just airports
cat("\nAirport subgraph:\n")
airport_g <- vx_subgraph(g, "Airport")
print(vx_nodes(airport_g))

# Neighbors of DUB
cat("\nNeighbors of DUB (depth 1):\n")
dub_nbrs <- vx_neighbors(g, "DUB", depth = 1)
print(vx_nodes(dub_nbrs))

# Shortest path
cat("\nShortest path DUB -> JFK:\n")
print(vx_shortest_path(g, "DUB", "JFK"))

# --------------------------------------------------------------------------
# Module 2: Alert Rules
# --------------------------------------------------------------------------

cat("\n--- Module 2: Alert Rules ---\n")

rule_capacity <- vx_alert_rule(
  id = "over_capacity",
  description = "Airports over 90% utilization",
  condition = function(g) {
    nodes <- vx_nodes(g)
    airports <- nodes[nodes$.object_type == "Airport", ]
    airports[airports$utilization > 0.9, ]$.node_id
  },
  severity = "critical"
)

rule_high_use <- vx_alert_rule(
  id = "high_use",
  description = "Airports over 80% utilization",
  condition = function(g) {
    nodes <- vx_nodes(g)
    airports <- nodes[nodes$.object_type == "Airport", ]
    airports[airports$utilization > 0.8, ]$.node_id
  },
  severity = "high"
)

alerts <- vx_evaluate_alerts(g, list(rule_capacity, rule_high_use))
cat("Alerts:\n")
print(alerts)

cat("\nRisk propagation from DUB (depth 2):\n")
propagated <- vx_propagate_risk(g, "DUB", depth = 2)
print(propagated)

# --------------------------------------------------------------------------
# Module 3: What-If Simulation
# --------------------------------------------------------------------------

cat("\n--- Module 3: What-If Simulation ---\n")

# Scenario: Dublin airport closes
cat("Scenario: Dublin airport closes\n")
scenario_close <- vx_scenario(g) |>
  vx_remove_node("DUB")
g_closed <- vx_apply_scenario(scenario_close)

cat("Modified graph:\n")
print(g_closed)

cat("\nDifferences:\n")
diff <- vx_compare(g, g_closed)
print(diff)

# Scenario: Increase Dublin capacity
cat("\nScenario: Double Dublin's capacity\n")
scenario_expand <- vx_scenario(g) |>
  vx_modify_node("DUB", capacity = 60L, utilization = 0.5)
g_expanded <- vx_apply_scenario(scenario_expand)

cat("Re-evaluate alerts on expanded scenario:\n")
alerts_after <- vx_evaluate_alerts(g_expanded, list(rule_capacity))
if (nrow(alerts_after) == 0) {
  cat("  No critical alerts -- expansion resolved the issue.\n")
} else {
  print(alerts_after)
}

# --------------------------------------------------------------------------
# Module 4: Optimization (Experimental)
# --------------------------------------------------------------------------

cat("\n--- Module 4: Optimization (Experimental) ---\n")

# Simple optimization: minimize number of active edges
result <- vx_optimize(
  graph = g,
  objective = function(g) {
    # Minimize total number of edges
    nrow(vx_edges(g))
  },
  variables = list(
    vx_edge_var("active", type = "binary")
  ),
  constraints = list(),
  solver = "greedy"
)

cat("Optimization result:\n")
cat("  Status:          ", result$status, "\n")
cat("  Objective value: ", result$objective_value, "\n")
cat("  Iterations:      ", result$iterations, "\n")

# --------------------------------------------------------------------------
# Visualization
# --------------------------------------------------------------------------

cat("\n--- Visualization ---\n")
if (requireNamespace("visNetwork", quietly = TRUE)) {
  cat("Creating interactive plot...\n")
  p <- vx_plot(g,
    color_by = ".object_type",
    size_by = "capacity",
    highlight = "DUB",
    label_by = ".node_id"
  )
  cat("  visNetwork widget created. View in RStudio Viewer or browser.\n")
} else {
  cat("  visNetwork not installed. Install it for interactive plots:\n")
  cat("  install.packages('visNetwork')\n")
}

# --------------------------------------------------------------------------
# Cleanup
# --------------------------------------------------------------------------

DBI::dbDisconnect(con, shutdown = TRUE)
cat("\nDone.\n")
