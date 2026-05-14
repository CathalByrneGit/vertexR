# Tests for DuckPGQ patterns: vx_pgq_pattern, vx_pgq_list_patterns

test_that("vx_pgq_list_patterns returns available patterns", {
  patterns <- vx_pgq_list_patterns()

  expect_s3_class(patterns, "data.frame")
  expect_true("pattern_name" %in% names(patterns))
  expect_true("description" %in% names(patterns))
  expect_gte(nrow(patterns), 1L)

  # Check known patterns exist
  expect_true("common_officer" %in% patterns$pattern_name)
  expect_true("intermediary_hub" %in% patterns$pattern_name)
})


test_that("vx_pgq_pattern errors on unknown pattern", {
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  expect_error(
    vx_pgq_pattern(con, "test_graph", "nonexistent_pattern"),
    "Unknown pattern"
  )
})


test_that("vx_pgq_pattern errors on non-DBI connection", {
  expect_error(
    vx_pgq_pattern("not a connection", "graph", "common_officer"),
    "must be a DBI connection"
  )
})
