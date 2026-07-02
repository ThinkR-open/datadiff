# Lazy path temp-table hygiene: the slim boolean table computed on
# the user's connection must not outlive the call, and its name must be unique
# per process (no clock-derived names).

test_that("no temp tables leak on a user-supplied connection", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckdb::dbWriteTable(con, "ref_leak",  data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)))
  duckdb::dbWriteTable(con, "cand_leak", data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)))

  for (i in 1:3) {
    res <- compare_datasets_from_yaml(
      dplyr::tbl(con, "ref_leak"), dplyr::tbl(con, "cand_leak"),
      key = "id"
    )
    expect_true(res$all_passed)
  }

  leftover <- DBI::dbGetQuery(con, paste0(
    "SELECT table_name FROM information_schema.tables ",
    "WHERE table_name LIKE 'datadiff%'"
  ))
  expect_equal(nrow(leftover), 0L)
})

test_that("temp tables are cleaned up even when the comparison fails", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  duckdb::dbWriteTable(con, "ref_red",  data.frame(id = 1:3, x = c(1.0, 2.0, 3.0)))
  duckdb::dbWriteTable(con, "cand_red", data.frame(id = 1:3, x = c(9.0, 9.0, 9.0)))

  res <- compare_datasets_from_yaml(
    dplyr::tbl(con, "ref_red"), dplyr::tbl(con, "cand_red"),
    key = "id"
  )
  expect_false(res$all_passed)

  leftover <- DBI::dbGetQuery(con, paste0(
    "SELECT table_name FROM information_schema.tables ",
    "WHERE table_name LIKE 'datadiff%'"
  ))
  expect_equal(nrow(leftover), 0L)
})

test_that("temp table names are unique within a process", {
  n1 <- datadiff_tmp_table_name()
  n2 <- datadiff_tmp_table_name()
  expect_false(identical(n1, n2))
  expect_match(n1, "^datadiff_tmp_")
  # The name embeds the PID so two R processes sharing a database cannot collide
  expect_match(n1, as.character(Sys.getpid()), fixed = TRUE)
})
