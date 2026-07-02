# NaN/Inf semantics of the templated SQL booleans (lazy path), with the R
# tolerance kernel as oracle (NOT the historical dplyr path, which shared the
# same NULL-only blind spot): same-sign infinities pass, a one-sided
# NA/NaN/Inf fails, NA/NaN on both sides follows na_equal.

# Boolean vector produced by the templated SQL for one tolerance column.
lazy_tol_ok <- function(con, cand, ref, abs_tol, rel_tol, na_equal) {
  cmp <- data.frame(id = seq_along(cand), x = cand, x__reference = ref)
  DBI::dbWriteTable(con, "cmp_nan_inf", cmp, overwrite = TRUE)
  rules <- list(x = list(abs = abs_tol, rel = rel_tol))
  out <- add_bool_cols_sql(
    dplyr::tbl(con, "cmp_nan_inf"),
    tol_cols = "x", eq_cols = character(0),
    col_rules = rules, ref_suffix = "__reference", na_equal = na_equal
  )
  res <- dplyr::collect(dplyr::arrange(out, id))
  as.logical(res$x__ok)
}

test_that("DuckDB tolerance booleans match the R kernel on NaN/Inf/NA", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  vals <- c(1, 2.5, NA, NaN, Inf, -Inf, 0)
  grid <- expand.grid(cand = vals, ref = vals)

  for (na_equal in c(TRUE, FALSE)) {
    expected <- compute_tolerance_col(
      grid$cand, grid$ref,
      abs_tol = 0.1, rel_tol = 0, na_equal = na_equal
    )$ok
    got <- lazy_tol_ok(con, grid$cand, grid$ref,
                       abs_tol = 0.1, rel_tol = 0, na_equal = na_equal)
    expect_identical(got, expected)
  }

  # Relative tolerance: an infinite reference must not produce an infinite
  # threshold that lets a finite candidate pass
  for (na_equal in c(TRUE, FALSE)) {
    expected_rel <- compute_tolerance_col(
      grid$cand, grid$ref,
      abs_tol = 0, rel_tol = 0.5, na_equal = na_equal
    )$ok
    got_rel <- lazy_tol_ok(con, grid$cand, grid$ref,
                           abs_tol = 0, rel_tol = 0.5, na_equal = na_equal)
    expect_identical(got_rel, expected_rel)
  }
})

test_that("SQLite tolerance booleans match the R kernel on Inf/NA", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # No NaN in the grid: SQLite has no NaN storage (it arrives as NULL,
  # which the NA rules then cover)
  vals <- c(1, 2.5, NA, Inf, -Inf, 0)
  grid <- expand.grid(cand = vals, ref = vals)

  for (na_equal in c(TRUE, FALSE)) {
    expected <- compute_tolerance_col(
      grid$cand, grid$ref,
      abs_tol = 0.1, rel_tol = 0, na_equal = na_equal
    )$ok
    got <- lazy_tol_ok(con, grid$cand, grid$ref,
                       abs_tol = 0.1, rel_tol = 0, na_equal = na_equal)
    expect_identical(got, expected)
  }
})

test_that("numeric equality columns treat NaN as NA-like on DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  run_eq <- function(cand, ref, na_equal) {
    cmp <- data.frame(id = seq_along(cand), x = cand, x__reference = ref)
    DBI::dbWriteTable(con, "cmp_eq_nan", cmp, overwrite = TRUE)
    out <- add_bool_cols_sql(
      dplyr::tbl(con, "cmp_eq_nan"),
      tol_cols = character(0), eq_cols = "x",
      col_rules = list(), ref_suffix = "__reference", na_equal = na_equal,
      eq_num_cols = "x"
    )
    res <- dplyr::collect(dplyr::arrange(out, id))
    as.logical(res$x__eq)
  }

  cand <- c(NaN, NaN, NaN, 1)
  ref  <- c(NaN, 1,   NA,  1)

  # Local-path oracle: is.na(NaN) is TRUE, so NaN follows the NA rules
  # (one-sided = difference, two-sided follows na_equal)
  expect_identical(run_eq(cand, ref, na_equal = TRUE),
                   c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(run_eq(cand, ref, na_equal = FALSE),
                   c(FALSE, FALSE, FALSE, TRUE))
})

test_that("local and DuckDB verdicts agree end to end on NaN/Inf data", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")

  ref  <- data.frame(id = 1:4, x = c(NaN, Inf, Inf, NaN))
  cand <- data.frame(id = 1:4, x = c(0,   1,   Inf, NaN))

  res_local <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id")
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "ref_e2e",  ref)
  duckdb::dbWriteTable(con, "cand_e2e", cand)
  res_lazy <- suppressMessages(
    compare_datasets_from_yaml(
      dplyr::tbl(con, "ref_e2e"), dplyr::tbl(con, "cand_e2e"),
      key = "id"
    )
  )

  expect_identical(res_local$all_passed, res_lazy$all_passed)
  # Same per-column failure counts, not just the same global verdict:
  # rows 1 (NaN vs 0) and 2 (Inf vs 1) fail, row 3 (Inf vs Inf) and
  # row 4 (NaN vs NaN, na_equal default TRUE) pass
  tol_row_failures <- function(cov) {
    as.integer(cov$n_failed[cov$column == "x" & cov$check == "tolerance"])
  }
  expect_identical(tol_row_failures(res_local$coverage), 2L)
  expect_identical(tol_row_failures(res_lazy$coverage), 2L)
})
