# vertexR 0.2.1

* Schema-qualified object types (`source$schema`) now work. Tables are read
  with `dbplyr::in_schema()` and checked with `DBI::Id()`, and `vx_pgq_setup()`
  qualifies the vertex and edge table names (`schema.table`) in
  `CREATE PROPERTY GRAPH`. Requires ontologySpecR >= 0.2.2. The PGQ qualified
  names have only been checked as generated DDL, not against a live DuckPGQ
  install.

# vertexR 0.2.0

* Adds the DuckPGQ backend (`vx_pgq_*`), new in this release: `vx_pgq_available()`,
  `vx_pgq_setup()`, `vx_pgq_graph_name()`, `vx_pgq_graph_exists()`,
  `vx_pgq_match()`, `vx_pgq_neighbors()`, `vx_pgq_shortest_path()`,
  `vx_pgq_pattern()`, `vx_pgq_list_patterns()`, `vx_pgq_subgraph()` and
  `vx_pgq_components()`.
* Join keys are read with `ontologySpecR::link_join_keys()`. Links without
  join keys are skipped with a single warning naming them.
* Query-backed object types (`source$kind = "query"`) work in `vertex_graph()`
  and `vx_pgq_setup()`. Setup creates views `<graph_name>__<type_id>` for them.
  `filter_sql` can be keyed by object type id.
* `vx_pgq_neighbors()` no longer hard-codes a `name` column: pass
  `target_label` to get a `target_label` column. It does not return
  `path_length` (DuckPGQ cannot supply it for quantified patterns).
* The PGQ backend aborts clearly on composite primary keys.
* Internal bundle accessors return unnamed lists.

* Bundles are now read only through ontologySpecR. Every function that takes
  a bundle calls `ontologySpecR::as_bundle()` first, so legacy-spelled bundles
  are accepted (with a warning from ontologySpecR).
* Internal helpers use `bundle_object_types()`, `bundle_link_types()`,
  `bundle_object_type()`, `link_endpoints()`, `object_table()` and
  `object_pk()` instead of reading bundle fields directly.
* `vx_pgq_graph_name()` uses `bundle_id()`.
* Requires ontologySpecR >= 0.2.0 (see `Remotes`).
