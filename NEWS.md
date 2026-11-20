# vertexR 0.2.0

* Bundles are now read only through ontologySpecR. Every function that takes
  a bundle calls `ontologySpecR::as_bundle()` first, so legacy-spelled bundles
  are accepted (with a warning from ontologySpecR).
* Internal helpers use `bundle_object_types()`, `bundle_link_types()`,
  `bundle_object_type()`, `link_endpoints()`, `object_table()` and
  `object_pk()` instead of reading bundle fields directly.
* `vx_pgq_graph_name()` uses `bundle_id()`.
* Requires ontologySpecR >= 0.2.0 (see `Remotes`).
