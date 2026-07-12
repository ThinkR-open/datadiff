# Lazy verdict via SQL aggregates: the coverage counts come from one aggregate
# scan in the database, and the boolean slim table is only collected for the
# FAILING columns. A green lazy comparison must not load N rows of booleans
# into R ("without loading data into R memory" is the documented promise).

test_that("a green lazy comparison does not collect the boolean table", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  n <- 5e4
  df <- data.frame(id = seq_len(n), a = rnorm(n), b = rnorm(n),
                   s = sample(letters, n, replace = TRUE))
  duckdb::dbWriteTable(con, "ref_agg",  df)
  duckdb::dbWriteTable(con, "cand_agg", df)

  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_agg"), dplyr::tbl(con, "cand_agg"), key = "id"
  ))
  expect_true(res$all_passed)
  # Coverage counts are intact (they now come from the SQL aggregates)
  cov <- res$coverage
  expect_identical(as.integer(cov$n[cov$column == "a" & cov$check == "tolerance"]), as.integer(n))
  expect_identical(as.integer(sum(cov$n_failed)), 0L)
  # The agent is the constant-size pass placeholder, not N collected rows
  expect_lte(nrow(res$response$tbl), 1L)
})

test_that("a red lazy comparison collects only the failing boolean columns", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  n <- 1e4
  df <- data.frame(id = seq_len(n), a = rnorm(n), b = rnorm(n),
                   s = sample(letters, n, replace = TRUE))
  df2 <- df
  df2$a[c(3, 7)] <- 999
  duckdb::dbWriteTable(con, "ref_red2",  df)
  duckdb::dbWriteTable(con, "cand_red2", df2)

  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_red2"), dplyr::tbl(con, "cand_red2"), key = "id"
  ))
  expect_false(res$all_passed)

  # Only the failing tolerance column (plus the row-count flag consumed by
  # its validation step) reaches the local agent: b__ok and s__eq stay in
  # the database
  agent_cols <- names(res$response$tbl)
  expect_true("a__ok" %in% agent_cols)
  expect_false("b__ok" %in% agent_cols)
  expect_false("s__eq" %in% agent_cols)

  # The verdict details are unchanged: 2 failing rows on a, everything else
  # green in the coverage
  cov <- res$coverage
  expect_identical(as.integer(cov$n_failed[cov$column == "a" & cov$check == "tolerance"]), 2L)
  expect_identical(as.integer(cov$n_failed[cov$column == "b" & cov$check == "tolerance"]), 0L)
  expect_identical(as.integer(cov$n_failed[cov$column == "s" & cov$check == "equality"]), 0L)

  # And the extracts still surface the failing rows
  ex <- pointblank::get_data_extracts(res$response)
  expect_gte(length(ex), 1L)
})

test_that("SQL-aggregated counts match the collected booleans on SQLite too", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- data.frame(id = 1:50, x = c(rep(1, 45), rep(2, 5)),
                   s = rep(c("u", "v"), 25))
  ref  <- data.frame(id = 1:50, x = rep(1, 50), s = rep(c("u", "v"), 25))
  DBI::dbWriteTable(con, "ref_sq",  ref)
  DBI::dbWriteTable(con, "cand_sq", df)

  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_sq"), dplyr::tbl(con, "cand_sq"), key = "id"
  ))
  expect_false(res$all_passed)
  cov <- res$coverage
  expect_identical(as.integer(cov$n_failed[cov$column == "x" & cov$check == "tolerance"]), 5L)
  expect_identical(as.integer(cov$n_failed[cov$column == "s" & cov$check == "equality"]), 0L)
})

test_that("structural-only lazy failure keeps a failing verdict (no value checks)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckdb::dbWriteTable(con, "ref_struct",  data.frame(id = 1:3, x = c(1, 2, 3), extra_ref = 1:3))
  duckdb::dbWriteTable(con, "cand_struct", data.frame(id = 1:3, x = c(1, 2, 3)))

  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines(
    "version: 1\ndefaults: {keys: [id], na_equal: yes}\nrow_validation: {check_count: no}\nby_type:\n  numeric: {abs: 0.001}\n",
    con = p
  )
  res <- suppressMessages(suppressWarnings(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_struct"), dplyr::tbl(con, "cand_struct"),
    key = "id", path = p
  )))
  # The missing column is the only failure: the agent verdict must agree
  # with the coverage, not pass on a 0-unit dummy step
  expect_false(res$all_passed)
  expect_false(pointblank::all_passed(res$response))
  expect_identical(res$missing_in_candidate, "extra_ref")
})

test_that("key-only lazy comparison runs without value columns", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckdb::dbWriteTable(con, "ref_keys",  data.frame(id = 1:3))
  duckdb::dbWriteTable(con, "cand_keys", data.frame(id = 1:3))

  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines(
    "version: 1\ndefaults: {keys: [id], na_equal: yes}\nrow_validation: {check_count: no}\n",
    con = p
  )
  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_keys"), dplyr::tbl(con, "cand_keys"),
    key = "id", path = p
  ))
  expect_true(res$all_passed)
})
