# arrow_dataset_to_duckdb(): SQL built from file paths must survive special
# characters, and multi-file datasets must keep a unified schema (as the
# arrow::to_duckdb() fallback always did).

test_that("a Parquet path containing a quote works end to end", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")

  d <- file.path(tempdir(), "l'export datadiff")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  arrow::write_parquet(
    data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)),
    file.path(d, "part-0.parquet")
  )
  ds <- arrow::open_dataset(d)

  res <- suppressMessages(compare_datasets_from_yaml(ds, ds, key = "id"))
  expect_true(res$all_passed)
})

test_that("multi-file datasets with shifted schemas are unified by name", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")

  d <- file.path(tempdir(), "datadiff_union_by_name")
  dir.create(d, showWarnings = FALSE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # Same columns, different physical order in the second file: a positional
  # binding would silently misalign x and y
  arrow::write_parquet(
    data.frame(id = 1:2, x = c(1.0, 2.0), y = c(10.0, 20.0)),
    file.path(d, "part-0.parquet")
  )
  arrow::write_parquet(
    data.frame(y = c(30.0, 40.0), x = c(3.0, 4.0), id = 3:4),
    file.path(d, "part-1.parquet")
  )
  ds <- arrow::open_dataset(d)

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tbl_lazy <- arrow_dataset_to_duckdb(ds, con, "datadiff_union_test")
  collected <- dplyr::collect(tbl_lazy)
  collected <- collected[order(collected$id), , drop = FALSE]

  expect_setequal(names(collected), c("id", "x", "y"))
  expect_identical(collected$x, c(1.0, 2.0, 3.0, 4.0))
  expect_identical(collected$y, c(10.0, 20.0, 30.0, 40.0))
})

test_that("non-file-backed Arrow objects still use the to_duckdb fallback", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")

  tbl_arrow <- arrow::arrow_table(data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)))
  res <- suppressMessages(compare_datasets_from_yaml(tbl_arrow, tbl_arrow, key = "id"))
  expect_true(res$all_passed)
})

test_that("an invalid duckdb_memory_limit is rejected before reaching SQL", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")

  tbl_arrow <- arrow::arrow_table(data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)))
  expect_error(
    compare_datasets_from_yaml(tbl_arrow, tbl_arrow, key = "id",
                               duckdb_memory_limit = "8GB'; DROP TABLE x; --"),
    regexp = "duckdb_memory_limit"
  )
  expect_error(
    compare_datasets_from_yaml(tbl_arrow, tbl_arrow, key = "id",
                               duckdb_memory_limit = "beaucoup"),
    regexp = "duckdb_memory_limit"
  )
})
