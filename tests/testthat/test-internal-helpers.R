# Direct unit tests for small internal helpers that previously had no
# dedicated coverage.

test_that("format_key_examples formats and truncates", {
  keys <- data.frame(id = c(1, 2, 3, 4), grp = c("a", "b", "c", "d"))
  out <- format_key_examples(keys, c("id", "grp"))
  expect_length(out, 4L)                 # 3 examples + "..."
  expect_identical(out[4], "...")
  expect_match(out[1], "id = 1")
  expect_match(out[1], "grp = a")

  out2 <- format_key_examples(keys[1:2, , drop = FALSE], c("id", "grp"))
  expect_length(out2, 2L)                # below threshold: no marker
})

test_that("get_col_names works on data.frames and 0-column inputs", {
  expect_identical(get_col_names(data.frame(a = 1, b = 2)), c("a", "b"))
  expect_length(get_col_names(data.frame()), 0L)
})

test_that("is_non_local and is_arrow classify inputs", {
  df <- data.frame(a = 1)
  expect_false(is_non_local(df))
  expect_false(is_arrow(df))

  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "t", df)
  lazy <- dplyr::tbl(con, "t")
  expect_true(is_non_local(lazy))
  expect_false(is_arrow(lazy))

  skip_if_not_installed("arrow")
  at <- arrow::arrow_table(df)
  expect_true(is_non_local(at))
  expect_true(is_arrow(at))
})

test_that("datadiff_report_html with file = NULL returns the report invisibly", {
  ref <- data.frame(id = 1:2, x = c(1, 2))
  res <- suppressMessages(compare_datasets_from_yaml(ref, ref, key = "id"))
  vis <- withVisible(datadiff_report_html(res, file = NULL))
  expect_false(vis$visible)
  expect_false(is.null(vis$value))
})
