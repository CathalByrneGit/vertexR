# vertexR

Graph exploration, simulation, alerting, and optimization for
ontology-backed system graphs.

vertexR treats the ontology not as isolated tables but as an
**interconnected system** -- a graph where nodes are objects and edges are
links. Every operation (subgraph extraction, impact propagation, scenario
simulation, optimization) works on this graph.

## Two Backends

vertexR provides two complementary backends:

| Backend | Use Case | Memory |
|---------|----------|--------|
| `vertex_graph()` + igraph | Small-to-medium graphs, algorithm-heavy work | In-memory |
| `vx_pgq_*()` + DuckPGQ | Large graphs, traversal queries | In-database |

The pattern: use `vx_pgq_*` for traversal on the full dataset, then pull a
subgraph into `vertex_graph()` for scenario analysis and optimization.

## Installation

```r
# install.packages("remotes")
remotes::install_github("CathalByrneGit/vertexR")

# For DuckPGQ support (optional):
# Install DuckDB >= 1.0, then in R:
# DBI::dbExecute(con, "INSTALL duckpgq FROM community")
```

## Modules

### 1. Graph Construction (In-Memory)

Build a system graph from an ontologySpecR bundle and a DBI connection.

```r
library(ontologySpecR)
library(vertexR)

b <- read_bundle("aviation-demo.json")
con <- DBI::dbConnect(duckdb::duckdb())
# ... populate tables ...

g <- vertex_graph(b, con)
vx_nodes(g)
vx_edges(g)
vx_neighbors(g, "DUB", depth = 2)
```

The `max_nodes` parameter (default 50000) warns on large graphs.
Use `filter_sql` to pre-filter tables before loading:

```r
g <- vertex_graph(b, con,
  filter_sql = list(airports = "country = 'Ireland'")
)
```

### 2. DuckPGQ Backend (Large Graphs)

For graphs too large to fit in memory, use the DuckPGQ backend.

```r
# Check availability
vx_pgq_available(con)

# Create property graph from bundle
vx_pgq_setup(b, con)
graph_name <- vx_pgq_graph_name(b)

# Query neighbors (executes in DuckDB, not R)
vx_pgq_neighbors(con, graph_name,
  source_type = "Airport",
  source_ids = c("DUB"),
  link_type = "RouteOrigin",
  target_type = "FlightRoute",
  depth = 2
)

# Find shortest path
vx_pgq_shortest_path(con, graph_name,
  from_type = "Airport", from_id = "DUB",
  to_type = "Airport", to_id = "JFK"
)

# Extract subgraph for algorithm work
g <- vx_pgq_subgraph(con, b, graph_name,
  seed_ids = c("DUB"), seed_type = "Airport", depth = 2
)
```

Named investigation patterns (ICIJ-style):

```r
# Find entities sharing an officer
vx_pgq_pattern(con, graph_name, "common_officer")

# Find multi-jurisdiction officers
vx_pgq_pattern(con, graph_name, "multi_jurisdiction_officer",
  params = list(min_jurisdictions = 3)
)

# List available patterns
vx_pgq_list_patterns()
```

### 3. Graph Analysis

Subgraph extraction and topology analysis (in-memory).

```r
vx_subgraph(g, "Airport")
vx_neighbors(g, "DUB", depth = 2)
vx_shortest_path(g, "DUB", "JFK")
vx_connected_components(g, mode = "weak")
vx_cycle_detect(g)
```

### 4. Alert Rules

Define and evaluate graph-based alert conditions.

```r
rule <- vx_alert_rule("over_capacity",
  "Airports over 90% utilization",
  condition = function(g) {
    nodes <- vx_nodes(g)
    a <- nodes[nodes$.object_type == "Airport", ]
    a[a$utilization > 0.9, ]$.node_id
  },
  severity = "critical"
)

vx_evaluate_alerts(g, list(rule))
vx_propagate_risk(g, "DUB", depth = 2)
```

### 5. What-If Simulation

Modify the graph and see consequences without touching the database.

```r
scenario <- vx_scenario(g) |>
  vx_modify_node("DUB", capacity = 0L, status = "closed") |>
  vx_remove_node("CDG") |>
  vx_add_edge("R99", from = "JFK", to = "LHR", .link_type = "RouteOrigin")

g2 <- vx_apply_scenario(scenario)
vx_compare(g, g2)
```

### 6. Optimization (Experimental)

Find optimal interventions using a greedy solver.

```r
result <- vx_optimize(
  graph = g,
  objective = function(g) sum(vx_nodes(g)$unserved_demand, na.rm = TRUE),
  variables = list(
    vx_edge_var("active", type = "binary"),
    vx_node_var("selected", type = "binary")
  ),
  constraints = list(
    vx_node_constraint("Airport", function(node, edges) {
      sum(edges$active) <= node$capacity
    })
  ),
  solver = "greedy",
  max_iter = 100L
)
```

### 7. Visualization

Interactive graph visualization with visNetwork (ggraph fallback).

```r
vx_plot(g, color_by = ".object_type", size_by = "capacity",
        highlight = "DUB")
vx_plot_scenario(g, scenario)
```

### 8. ConceptR Integration

Join concept evaluation results onto graph nodes for visualization.

```r
g_with_concepts <- vx_color_by_concept(g, concept_results)
vx_plot(g_with_concepts, color_by = "busy_airport")
```

## Dependencies

- **Imports**: ontologySpecR, DBI, igraph, tidygraph, dplyr, rlang
- **Suggests**: duckdb, visNetwork, ggraph, ggplot2, actionTypesR, conceptR,
  testthat, knitr, rmarkdown
- **SystemRequirements**: DuckDB >= 1.0 with duckpgq extension (optional)

## Ecosystem

```
vertex_graph()           → igraph / tidygraph (small graphs, <max_nodes)
vx_pgq_*() functions     → DuckPGQ inside DuckDB (large graphs, unlimited)
```

vertexR sits on top of **ontologySpecR** (type definitions) and **DBI**
(database access). It optionally integrates with **objectSetsR** (lazy
queries), **conceptR** (concept evaluation), and **actionTypesR** (action
execution).

## License

MIT
