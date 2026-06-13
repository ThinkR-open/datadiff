# TDD spec for compute_tolerance_col(): the per-column vector kernel behind
# add_tolerance_columns. It must reproduce, element-for-element, the per-column
# logic it factors out (absolute/relative tolerance, Inf/-Inf/NaN handling, NA
# semantics under na_equal, and the IEEE 754 rounding correction). Working
# column-by-column keeps the data cache-resident, unlike a whole-block matrix
# kernel which becomes memory-bandwidth bound on tall inputs.

test_that("absolute tolerance decides ok elementwise", {
  out <- compute_tolerance_col(c(1.05, 1.20), c(1.00, 1.00),
                               abs_tol = 0.1, rel_tol = 0, na_equal = TRUE)
  expect_equal(out$ok, c(TRUE, FALSE))
  expect_equal(out$absdiff, c(0.05, 0.20))
  expect_equal(out$thresh, c(0.1, 0.1))
})

test_that("zero absolute tolerance requires exact match", {
  out <- compute_tolerance_col(c(5, 6), c(5, 5),
                               abs_tol = 0, rel_tol = 0, na_equal = TRUE)
  expect_equal(out$ok, c(TRUE, FALSE))
})

test_that("relative tolerance scales the threshold by |ref|", {
  out <- compute_tolerance_col(c(105, 120), c(100, 100),
                               abs_tol = 0, rel_tol = 0.1, na_equal = TRUE)
  expect_equal(out$thresh, c(10, 10))
  expect_equal(out$ok, c(TRUE, FALSE))
})

test_that("NA on both sides follows na_equal", {
  expect_equal(
    compute_tolerance_col(NA_real_, NA_real_, 0.1, 0, na_equal = TRUE)$ok, TRUE
  )
  expect_equal(
    compute_tolerance_col(NA_real_, NA_real_, 0.1, 0, na_equal = FALSE)$ok, FALSE
  )
})

test_that("NA on a single side always fails (tolerance path)", {
  expect_equal(
    compute_tolerance_col(c(NA_real_, 10), c(10, NA_real_), 0.1, 0, na_equal = TRUE)$ok,
    c(FALSE, FALSE)
  )
  expect_equal(
    compute_tolerance_col(c(NA_real_, 10), c(10, NA_real_), 0.1, 0, na_equal = FALSE)$ok,
    c(FALSE, FALSE)
  )
})

test_that("NaN on both sides follows na_equal like NA", {
  expect_equal(compute_tolerance_col(NaN, NaN, 0.1, 0, na_equal = TRUE)$ok, TRUE)
  expect_equal(compute_tolerance_col(NaN, NaN, 0.1, 0, na_equal = FALSE)$ok, FALSE)
})

test_that("identical infinities pass, mismatched or one-sided infinities fail", {
  out <- compute_tolerance_col(c(Inf, Inf, Inf), c(Inf, -Inf, 5),
                               0.1, 0, na_equal = TRUE)
  expect_equal(out$ok, c(TRUE, FALSE, FALSE))
  expect_equal(out$absdiff[1], 0) # same-sign Inf treated as zero difference
})

test_that("identical infinities pass even when na_equal is FALSE", {
  expect_equal(compute_tolerance_col(Inf, Inf, 0.1, 0, na_equal = FALSE)$ok, TRUE)
})

test_that("IEEE 754 rounding at the tolerance boundary is absorbed", {
  # 100.01 - 100.00 = 0.010000000000005116 in double precision, strictly above
  # the nominal threshold 0.01; the fp correction must let it pass.
  out <- compute_tolerance_col(100.01, 100.00, abs_tol = 0.01, rel_tol = 0,
                               na_equal = TRUE)
  expect_true(out$ok)
})
