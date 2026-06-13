# Equivalence guard for the speed optimization of compare_datasets_from_yaml.
#
# These tests pin BOTH the verdict (all_passed) AND the exact set of failing
# cells (as recovered through pointblank::get_data_extracts, the way the
# enc.mco oracle consumes the result). They must stay green before and after
# the optimization: the optimized code may change HOW the verdict is produced,
# never WHAT it produces.

KEY <- c("id", ".row")

make_ref <- function() {
  data.frame(
    id   = 1:5,
    .row = 1L,
    a    = c(10, 20, 30, 40, 50),       # numeric, tolerance path
    b    = c(1.0, 2.0, 3.0, 4.0, 5.0),  # numeric, tolerance path
    n    = c(100L, 200L, 300L, 400L, 500L), # integer, abs = 0
    txt  = c("x", "y", "z", "w", "v"),  # character, exact
    stringsAsFactors = FALSE
  )
}

test_that("identical datasets pass with no failing cells", {
  ref <- make_ref()
  out <- run_compare(ref, ref, key = KEY)
  expect_true(out$all_passed)
  expect_equal(out$cells, character(0))
})

test_that("numeric cell beyond absolute tolerance fails on that cell only", {
  ref <- make_ref(); cand <- ref
  cand$a[3] <- 30.5 # diff 0.5 > 0.101
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
  expect_equal(out$cells, "a@3|1")
})

test_that("numeric cell within absolute tolerance passes", {
  ref <- make_ref(); cand <- ref
  cand$a[3] <- 30.05 # diff 0.05 <= 0.101
  out <- run_compare(ref, cand, key = KEY)
  expect_true(out$all_passed)
  expect_equal(out$cells, character(0))
})

test_that("numeric cell exactly at the tolerance boundary passes", {
  ref <- make_ref(); cand <- ref
  cand$a[3] <- 30 + 0.101 # diff == abs tol
  out <- run_compare(ref, cand, key = KEY)
  expect_true(out$all_passed)
  expect_equal(out$cells, character(0))
})

test_that("integer cell differing by 1 fails when integer_abs = 0", {
  ref <- make_ref(); cand <- ref
  cand$n[2] <- 201L
  out <- run_compare(ref, cand, key = KEY, integer_abs = 0L)
  expect_false(out$all_passed)
  expect_equal(out$cells, "n@2|1")
})

test_that("NA on both sides passes when na_equal = TRUE", {
  ref <- make_ref(); cand <- ref
  ref$a[3] <- NA_real_; cand$a[3] <- NA_real_
  out <- run_compare(ref, cand, key = KEY, na_equal_default = TRUE)
  expect_true(out$all_passed)
  expect_equal(out$cells, character(0))
})

test_that("NA on both sides fails when na_equal = FALSE", {
  ref <- make_ref(); cand <- ref
  ref$a[3] <- NA_real_; cand$a[3] <- NA_real_
  out <- run_compare(ref, cand, key = KEY, na_equal_default = FALSE)
  expect_false(out$all_passed)
  expect_equal(out$cells, "a@3|1")
})

test_that("NA on a single side fails regardless of na_equal", {
  ref <- make_ref(); cand <- ref
  cand$a[4] <- NA_real_ # ref keeps 40
  out <- run_compare(ref, cand, key = KEY, na_equal_default = TRUE)
  expect_false(out$all_passed)
  expect_equal(out$cells, "a@4|1")
})

test_that("character mismatch fails on that cell only", {
  ref <- make_ref(); cand <- ref
  cand$txt[2] <- "Y"
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
  expect_equal(out$cells, "txt@2|1")
})

test_that("multiple failing cells across columns are all surfaced", {
  ref <- make_ref(); cand <- ref
  cand$a[3]   <- 30.5
  cand$txt[2] <- "Y"
  cand$n[5]   <- 999L
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
  expect_equal(out$cells, c("a@3|1", "n@5|1", "txt@2|1"))
})

test_that("character NA (any side) passes under na_equal = TRUE", {
  for (mutate in list(
    function(r, c) { r$txt[2] <- NA; c$txt[2] <- NA; list(r, c) },
    function(r, c) { c$txt[2] <- NA; list(r, c) },
    function(r, c) { r$txt[2] <- NA; list(r, c) }
  )) {
    ref <- make_ref(); cand <- ref
    m <- mutate(ref, cand); ref <- m[[1]]; cand <- m[[2]]
    out <- run_compare(ref, cand, key = KEY, na_equal_default = TRUE)
    expect_true(out$all_passed)
    expect_equal(out$cells, character(0))
  }
})

test_that("character NA (any side) fails under na_equal = FALSE", {
  for (mutate in list(
    function(r, c) { r$txt[2] <- NA; c$txt[2] <- NA; list(r, c) },
    function(r, c) { c$txt[2] <- NA; list(r, c) },
    function(r, c) { r$txt[2] <- NA; list(r, c) }
  )) {
    ref <- make_ref(); cand <- ref
    m <- mutate(ref, cand); ref <- m[[1]]; cand <- m[[2]]
    out <- run_compare(ref, cand, key = KEY, na_equal_default = FALSE)
    expect_false(out$all_passed)
    expect_equal(out$cells, "txt@2|1")
  }
})

test_that("missing column in candidate fails and is reported", {
  ref <- make_ref(); cand <- ref
  cand$b <- NULL
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
  expect_true("b" %in% out$missing)
})

test_that("type mismatch fails", {
  ref <- make_ref(); cand <- ref
  cand$a <- as.character(cand$a)
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
})

test_that("row count mismatch fails when check_count is enabled", {
  ref <- make_ref(); cand <- ref[1:4, , drop = FALSE]
  out <- run_compare(ref, cand, key = KEY, check_count_default = TRUE)
  expect_false(out$all_passed)
})

# Performance invariant: an all-pass comparison must take the fast short-circuit
# (a constant-size agent), not build one validation step per column. This guards
# the optimization itself against silent regression to the slow per-column path.
test_that("all-pass comparison short-circuits the per-column agent", {
  ref <- make_ref()
  out <- run_compare(ref, ref, key = KEY)
  expect_true(out$all_passed)
  expect_lte(nrow(out$res$agent$validation_set), 1L)
})

test_that("a failing comparison still builds the full per-column agent", {
  ref <- make_ref(); cand <- ref
  cand$a[2] <- 999
  out <- run_compare(ref, cand, key = KEY)
  expect_false(out$all_passed)
  expect_gt(nrow(out$res$agent$validation_set), 1L)
})
