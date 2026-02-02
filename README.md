# vertexR

Graph exploration, simulation, alerting, and optimization for
ontology-backed system graphs.

vertexR treats the ontology not as isolated tables but as an
**interconnected system** -- a graph where nodes are objects and edges are
links. Every operation (subgraph extraction, impact propagation, scenario
simulation, optimization) works on this graph.

## Installation

```r
# install.packages("remotes")
remotes::install_github("CathalByrneGit/vertexR")
```

## Modules

### 1. Graph Construction

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

### 2. Alert Rules

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

### 3. What-If Simulation

Modify the graph and see consequences without touching the database.

```r
scenario <- vx_scenario(g) |>
  vx_remove_node("DUB")

g2 <- vx_apply_scenario(scenario)
vx_compare(g, g2)
```

### 4. Optimization (Experimental)

Find optimal interventions using a greedy solver.

```r
result <- vx_optimize(
  graph = g,
  objective = function(g) sum(vx_nodes(g)$unserved_demand, na.rm = TRUE),
  variables = list(vx_edge_var("active", type = "binary")),
  constraints = list(),
  solver = "greedy"
)
```

### Visualization

Interactive graph visualization with visNetwork (ggraph fallback).

```r
vx_plot(g, color_by = ".object_type", size_by = "capacity",
        highlight = "DUB")
```

## Dependencies

- **Imports**: ontologySpecR, DBI, igraph, tidygraph, dplyr, rlang
- **Suggests**: duckdb, visNetwork, ggraph, ggplot2, actionTypesR,
  testthat, knitr, rmarkdown

## Ecosystem

vertexR sits on top of **ontologySpecR** (type definitions) and **DBI**
(database access). It optionally integrates with **objectSetsR** (lazy
queries) and **actionTypesR** (action execution).

## License

MIT
