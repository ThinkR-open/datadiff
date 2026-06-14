# TDD spec for add_bool_cols_sql(): generate the <col>__ok / <col>__eq boolean
# columns of the lazy path via a single templated SQL SELECT instead of
# per-expression dplyr::mutate(). The invariant is to reproduce EXACTLY the
# current dplyr lazy result (preserve the lazy verdict, just faster), verified
# on DuckDB and SQLite.

# Reference: the current dplyr approach (add_tolerance_columns lazy + the eq
# case_when mutate), collected to the boolean columns.
lazy_bools_dplyr <- function(cmp, tol_cols, eq_cols, col_rules, ref_suffix, na_equal) {
  cmp <- add_tolerance_columns(cmp, tol_cols, col_rules, ref_suffix, na_equal)
  exprs_eq <- list()
  for (c in eq_cols) {
    c_sym <- dplyr::sym(c); rc_sym <- dplyr::sym(paste0(c, ref_suffix))
    nm <- paste0(c, "__eq")
    exprs_eq[[nm]] <- if (na_equal) {
      rlang::expr(dplyr::case_when(
        is.na(!!c_sym) & is.na(!!rc_sym) ~ TRUE,
        is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
        !!c_sym == !!rc_sym ~ TRUE, .default = FALSE))
    } else {
      rlang::expr(dplyr::case_when(
        is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
        !!c_sym == !!rc_sym ~ TRUE, .default = FALSE))
    }
  }
  if (length(exprs_eq)) cmp <- dplyr::mutate(cmp, !!!exprs_eq)
  dplyr::collect(dplyr::select(cmp, dplyr::any_of(c(paste0(tol_cols, "__ok"),
                                                    paste0(eq_cols, "__eq")))))
}

lazy_bools_sql <- function(cmp, tol_cols, eq_cols, col_rules, ref_suffix, na_equal) {
  cmp <- add_bool_cols_sql(cmp, tol_cols, eq_cols, col_rules, ref_suffix, na_equal)
  dplyr::collect(dplyr::select(cmp, dplyr::any_of(c(paste0(tol_cols, "__ok"),
                                                    paste0(eq_cols, "__eq")))))
}

run_backend <- function(connect) {
  ref <- data.frame(
    id  = 1:6,
    a   = c(1.00, 2.00, 3.00, 100.00, NA, 5.0),       # tolerance
    g   = c("x", "y", "z", "w", NA, "v"),             # equality (char)
    stringsAsFactors = FALSE
  )
  cand <- data.frame(
    id  = 1:6,
    a   = c(1.05, 2.00, 3.50, 100.01, NA, NA),        # row1 within .1, row3 beyond, row4 IEEE boundary, row5 both NA, row6 one NA
    g   = c("x", "Y", "z", "w", NA, "v"),             # row2 differs, row5 both NA
    stringsAsFactors = FALSE
  )
  rules <- list(a = list(abs = 0.1, rel = 0))
  con <- connect()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "ref", ref); DBI::dbWriteTable(con, "cand", cand)
  for (ne in c(TRUE, FALSE)) {
    cmp <- dplyr::left_join(dplyr::tbl(con, "cand"), dplyr::tbl(con, "ref"),
                            by = "id", suffix = c("", "__reference"))
    old <- lazy_bools_dplyr(cmp, "a", "g", rules, "__reference", ne)
    new <- lazy_bools_sql(cmp, "a", "g", rules, "__reference", ne)
    # compare as logical (SQLite returns 0/1 integers)
    testthat::expect_equal(as.logical(new$a__ok), as.logical(old$a__ok),
                           info = paste("a__ok na_equal", ne))
    testthat::expect_equal(as.logical(new$g__eq), as.logical(old$g__eq),
                           info = paste("g__eq na_equal", ne))
  }
}

test_that("templated SQL booleans match dplyr on DuckDB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  run_backend(function() DBI::dbConnect(duckdb::duckdb()))
})

test_that("templated SQL booleans match dplyr on SQLite", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  run_backend(function() DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
})

test_that("a wide lazy table (300 tol cols) builds and stays correct", {
  skip_if_not_installed("duckdb")
  p <- 300L
  set.seed(1)
  df <- data.frame(id = 1:50)
  for (k in seq_len(p)) df[[paste0("m", k)]] <- round(rnorm(50, 100, 10), 2)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "ref", df); DBI::dbWriteTable(con, "cand", df)
  cmp <- dplyr::left_join(dplyr::tbl(con, "cand"), dplyr::tbl(con, "ref"),
                          by = "id", suffix = c("", "__reference"))
  rules <- stats::setNames(lapply(seq_len(p), function(i) list(abs = 0.101, rel = 0)),
                           paste0("m", seq_len(p)))
  out <- add_bool_cols_sql(cmp, paste0("m", seq_len(p)), character(0),
                           rules, "__reference", TRUE)
  ok <- dplyr::collect(dplyr::select(out, dplyr::any_of(paste0("m", seq_len(p), "__ok"))))
  expect_equal(ncol(ok), p)
  expect_true(all(vapply(ok, function(col) all(as.logical(col)), logical(1))))  # identical -> all TRUE
})
