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

# A user column named "n" collides with dplyr::count()'s default output name:
# count() then names its result "nn" (documented count()/tally() behavior).
# The lazy helpers must use a reserved name instead of relying on "n".

test_that("lazy duplicate detection works when the key column is named 'n'", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Key values are large on purpose: if filter()/sum() read the key column
  # instead of the count, n_dup_keys / n_dup_rows become absurd.
  duckdb::dbWriteTable(con, "t_n", data.frame(n = c(100, 100, 200), v = 1:3))
  info <- find_duplicate_keys(dplyr::tbl(con, "t_n"), "n")
  expect_equal(info$n_dup_keys, 1L)
  expect_equal(info$n_dup_rows, 2L)
  expect_equal(info$examples, "n = 100")

  # No duplicates: values > 1 in the key column must not be mistaken
  # for duplicate counts
  duckdb::dbWriteTable(con, "t_n_uniq", data.frame(n = c(100, 200, 300), v = 1:3))
  expect_null(find_duplicate_keys(dplyr::tbl(con, "t_n_uniq"), "n"))
})

test_that("lazy_nrow works on a table with a column named 'n'", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckdb::dbWriteTable(con, "t_nrow", data.frame(n = c(10, 20, 30), v = 1:3))
  expect_equal(lazy_nrow(dplyr::tbl(con, "t_nrow")), 3)
})

test_that("full lazy comparison works on tables with a column named 'n'", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(n = c(1, 1, 2), v = c(10, 10, 20))
  duckdb::dbWriteTable(con, "ref_n",  df)
  duckdb::dbWriteTable(con, "cand_n", df)

  # Key named "n" with genuine duplicates: the warning must carry the real
  # counts (1 duplicated key value affecting 2 rows), not sums of key values
  warns <- character(0)
  withCallingHandlers(
    {
      res <- compare_datasets_from_yaml(
        dplyr::tbl(con, "ref_n"), dplyr::tbl(con, "cand_n"),
        key = "n"
      )
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  dup_warns <- grep("Duplicate keys detected", warns, value = TRUE)
  expect_length(dup_warns, 1L)
  expect_match(dup_warns, "1 duplicate key value\\(s\\) affecting 2 rows")

  # Column "n" as a plain compared (non-key) column
  duckdb::dbWriteTable(con, "ref_n2",  data.frame(id = 1:3, n = c(10, 20, 30)))
  duckdb::dbWriteTable(con, "cand_n2", data.frame(id = 1:3, n = c(10, 20, 30)))
  res2 <- compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_n2"), dplyr::tbl(con, "cand_n2"),
    key = "id"
  )
  expect_true(res2$all_passed)
})
