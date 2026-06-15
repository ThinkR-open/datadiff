# TDD spec for the hot-path tolerance helpers:
#  - compute_tolerance_ok(): returns ONLY the boolean within-tolerance vector,
#    with a fast path when a column has no NA/NaN/Inf, and must equal
#    compute_tolerance_col(...)$ok element-for-element in every case.
#  - add_ok_columns(): adds ONLY the <col>__ok columns (no __absdiff/__thresh),
#    identical to add_tolerance_columns()'s __ok columns.

# --- compute_tolerance_ok == compute_tolerance_col$ok -------------------------

expect_ok_equiv <- function(cand, ref, abs_tol, rel_tol, na_equal) {
  expect_equal(
    compute_tolerance_ok(cand, ref, abs_tol, rel_tol, na_equal),
    compute_tolerance_col(cand, ref, abs_tol, rel_tol, na_equal)$ok
  )
}

test_that("fast path (no special values) matches the full kernel", {
  expect_ok_equiv(c(1.05, 1.20, 5), c(1.00, 1.00, 5), 0.1, 0, TRUE)
  expect_ok_equiv(c(105, 120), c(100, 100), 0, 0.1, TRUE)   # relative
  expect_ok_equiv(100.01, 100.00, 0.01, 0, TRUE)            # IEEE boundary
  expect_ok_equiv(c(5L, 6L), c(5L, 5L), 0, 0, TRUE)         # integer, abs 0
})

test_that("special values fall back and still match the full kernel", {
  expect_ok_equiv(c(NA, 10), c(10, NA), 0.1, 0, TRUE)
  expect_ok_equiv(c(NA, 10), c(10, NA), 0.1, 0, FALSE)
  expect_ok_equiv(c(NA_real_, NA_real_), c(NA_real_, NA_real_), 0.1, 0, TRUE)
  expect_ok_equiv(c(NaN, NaN), c(NaN, NaN), 0.1, 0, FALSE)
  expect_ok_equiv(c(Inf, Inf, Inf), c(Inf, -Inf, 5), 0.1, 0, TRUE)
  expect_ok_equiv(Inf, Inf, 0.1, 0, FALSE)
})

test_that("randomised property check: ok-only equals full kernel", {
  set.seed(42)
  pool <- c(rnorm(40), NA, NaN, Inf, -Inf)
  for (i in 1:20) {
    cand <- sample(pool, 15, replace = TRUE)
    ref  <- sample(pool, 15, replace = TRUE)
    abs_tol <- sample(c(0, 0.001, 0.1), 1)
    rel_tol <- sample(c(0, 0.01), 1)
    ne <- sample(c(TRUE, FALSE), 1)
    expect_ok_equiv(cand, ref, abs_tol, rel_tol, ne)
  }
})

# --- add_ok_columns -----------------------------------------------------------

test_that("add_ok_columns adds only __ok, matching add_tolerance_columns", {
  cmp <- data.frame(
    a = c(1.0, 2.0, 3.0), a__reference = c(1.0, 2.05, 3.0),
    b = c(10, 20, 30),     b__reference = c(10, 20, 30)
  )
  rules <- list(a = list(abs = 0.1, rel = 0), b = list(abs = 0, rel = 0))
  full <- add_tolerance_columns(cmp, c("a", "b"), rules, "__reference", TRUE)
  ok   <- add_ok_columns(cmp, c("a", "b"), rules, "__reference", TRUE)

  # __ok columns identical
  expect_equal(ok$a__ok, full$a__ok)
  expect_equal(ok$b__ok, full$b__ok)
  # only __ok added (no __absdiff / __thresh)
  added <- setdiff(names(ok), names(cmp))
  expect_setequal(added, c("a__ok", "b__ok"))
})

test_that("add_ok_columns is a no-op when there are no tolerance columns", {
  cmp <- data.frame(a = 1:3)
  expect_identical(add_ok_columns(cmp, character(0), list(), "__reference", TRUE), cmp)
})
