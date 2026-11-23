# Schema-qualified object types

schema_bundle <- function() {
  ontologySpecR::bundle(
    bundle_id = "schema-demo",
    bundle_version = "0.1.0",
    objects = list(
      ontologySpecR::object_type(
        id = "Airport",
        properties = list(
          ontologySpecR::property_def("airport_id", "string", nullable = FALSE),
          ontologySpecR::property_def("name", "string")
        ),
        primary_key = "airport_id",
        source_kind = "table",
        source_table = "airports",
        source_schema = "analytics"
      )
    )
  )
}

test_that("vertex_graph reads a schema-qualified table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("ontologySpecR")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA analytics")
  DBI::dbExecute(con, "CREATE TABLE analytics.airports (airport_id TEXT, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO analytics.airports VALUES ('DUB', 'Dublin'), ('JFK', 'JFK')")

  g <- vertex_graph(schema_bundle(), con)
  expect_equal(nrow(vx_nodes(g)), 2L)
})

test_that("PGQ DDL qualifies schema-qualified vertex tables", {
  skip_if_not_installed("ontologySpecR")
  ddl <- .build_pgq_ddl(schema_bundle(), "g", NULL, NULL)
  expect_match(ddl, "analytics.airports", fixed = TRUE)
})
