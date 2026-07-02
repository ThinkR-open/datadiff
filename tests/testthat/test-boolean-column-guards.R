# A missing __ok/__eq boolean column must fail loudly: all(NULL) is TRUE, so a
# silently dropped column would turn into a false "all pass" with n = 0 in the
# coverage - the worst failure mode for a non-regression tool.

test_that("tol_col_bool errors on a missing __ok column", {
  tbl <- data.frame(a = 1:3)
  expect_error(
    tol_col_bool(tbl, col = "a"),
    regexp = "internal error.*a__ok"
  )
})

test_that("eq_col_bool errors when both __eq and the raw pair are missing", {
  tbl_no_ref <- data.frame(a = 1:3)
  expect_error(
    eq_col_bool(tbl_no_ref, col = "a", ref_suffix = "__reference", na_equal = TRUE),
    regexp = "internal error.*a__reference"
  )

  tbl_no_cand <- data.frame(a__reference = 1:3)
  expect_error(
    eq_col_bool(tbl_no_cand, col = "a", ref_suffix = "__reference", na_equal = TRUE),
    regexp = "internal error.*'a'"
  )
})

test_that("the count reducers inherit the guard instead of PASS with n = 0", {
  tbl <- data.frame(a = 1:3)
  expect_error(tol_col_counts(tbl, col = "a"), regexp = "internal error")
  expect_error(
    eq_col_counts(tbl, col = "a", ref_suffix = "__reference", na_equal = TRUE),
    regexp = "internal error"
  )
})

test_that("the pass predicates inherit the guard instead of TRUE", {
  tbl <- data.frame(a = 1:3)
  expect_error(tol_col_passes(tbl, col = "a"), regexp = "internal error")
  expect_error(
    eq_col_passes(tbl, col = "a", ref_suffix = "__reference", na_equal = TRUE),
    regexp = "internal error"
  )
})
