# TDD spec for find_duplicate_keys(): fast duplicate-key detection that returns
# the same {n_dup_keys, n_dup_rows, examples} structure for both local
# data.frames (anyDuplicated/duplicated) and lazy tables (SQL count), so the
# warning message in compare_datasets_from_yaml() is unchanged.

test_that("no duplicate keys returns NULL (local)", {
  df <- data.frame(id = 1:5, v = 1:5)
  expect_null(find_duplicate_keys(df, "id"))
})

test_that("a single duplicated key is reported with counts and example (local)", {
  df <- data.frame(id = c(1, 1, 2, 3), v = 1:4)
  info <- find_duplicate_keys(df, "id")
  expect_equal(info$n_dup_keys, 1L)
  expect_equal(info$n_dup_rows, 2L)
  expect_equal(info$examples, "id = 1")
})

test_that("more than three duplicated keys truncate the examples with ...", {
  df <- data.frame(id = c(1, 1, 2, 2, 3, 3, 4, 4), v = 1:8)
  info <- find_duplicate_keys(df, "id")
  expect_equal(info$n_dup_keys, 4L)
  expect_equal(info$n_dup_rows, 8L)
  expect_length(info$examples, 4L)            # 3 + "..."
  expect_equal(info$examples[4], "...")
})

test_that("composite keys are detected and formatted (local)", {
  df <- data.frame(year = c(2023, 2023, 2024), month = c(1, 1, 1), v = 1:3)
  info <- find_duplicate_keys(df, c("year", "month"))
  expect_equal(info$n_dup_keys, 1L)
  expect_equal(info$n_dup_rows, 2L)
  expect_match(info$examples[1], "year = 2023")
  expect_match(info$examples[1], "month = 1")
})

test_that("local counts match the dplyr count()/group_by reference", {
  set.seed(1)
  df <- data.frame(id = sample(1:50, 200, replace = TRUE))
  info <- find_duplicate_keys(df, "id")
  ref <- df |>
    dplyr::count(dplyr::across(dplyr::all_of("id"))) |>
    dplyr::filter(n > 1L)
  expect_equal(info$n_dup_keys, nrow(ref))
  expect_equal(info$n_dup_rows, sum(ref$n))
})

test_that("lazy tables produce the same structure (SQL path)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "t", data.frame(id = c(1, 1, 2, 3), v = 1:4))
  info <- find_duplicate_keys(dplyr::tbl(con, "t"), "id")
  expect_equal(info$n_dup_keys, 1L)
  expect_equal(info$n_dup_rows, 2L)
})
