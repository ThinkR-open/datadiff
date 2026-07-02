# Tests for the explicit measured-deviation diagnostics in failing extracts.
#
# Up to 0.4.7 the local path materialised <col>__absdiff / <col>__thresh for
# every tolerance column, so the failing-row extracts (and the report CSV)
# showed the measured gap and the applied threshold. The 0.4.8 hot path dropped
# them everywhere; they are now re-added on the failure path for the failing
# columns only (add_diff_columns), keeping the all-pass fast path free of any
# diagnostic arithmetic.

test_that("failing extracts expose the measured deviation and threshold", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.5, 3.0))

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path))
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 0.1, numeric_rel = 0)

  result <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  expect_false(result$all_passed)

  extracts <- pointblank::get_data_extracts(result$reponse)
  expect_true(length(extracts) >= 1)
  ex <- extracts[[1]]

  # Candidate and reference values are still side by side...
  expect_true(all(c("value", "value__reference") %in% names(ex)))
  # ...and the explicit diagnostics are back (regression of 0.4.8, issue #11)
  expect_true(all(c("value__absdiff", "value__thresh") %in% names(ex)))

  expect_equal(nrow(ex), 1L)
  expect_equal(ex$value, 2.5)
  expect_equal(ex$value__reference, 2.0)
  expect_equal(ex$value__absdiff, 0.5)
  expect_equal(ex$value__thresh, 0.1)
})

test_that("diagnostics are computed for failing tolerance columns only", {
  ref  <- data.frame(id = 1:2, bad = c(1.0, 2.0), good = c(5.0, 6.0))
  cand <- data.frame(id = 1:2, bad = c(1.0, 9.0), good = c(5.0, 6.0))

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path))
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 0.1, numeric_rel = 0)

  result <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  expect_false(result$all_passed)

  extracts <- pointblank::get_data_extracts(result$reponse)
  ex <- extracts[[1]]

  expect_true(all(c("bad__absdiff", "bad__thresh") %in% names(ex)))
  # The passing column pays no diagnostic cost
  expect_false(any(c("good__absdiff", "good__thresh") %in% names(ex)))
  expect_equal(ex$bad__absdiff, 7.0)
})

test_that("all-pass fast path materialises no diagnostic columns", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))

  result <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id")
  )

  expect_true(result$all_passed)
  expect_false(any(grepl("__absdiff$|__thresh$", colnames(result$agent$tbl))))
  expect_length(pointblank::get_data_extracts(result$reponse), 0L)
})

test_that("add_diff_columns appends only __absdiff/__thresh, matching the kernel", {
  cmp <- data.frame(
    value = c(1.0, 2.5, NA),
    value__reference = c(1.0, 2.0, 3.0),
    value__ok = c(TRUE, FALSE, FALSE)
  )
  rules <- list(value = list(abs = 0.1, rel = 0.05))

  out <- add_diff_columns(cmp, "value", rules, "__reference", TRUE)

  expect_named(out, c("value", "value__reference", "value__ok",
                      "value__absdiff", "value__thresh"))
  kernel <- compute_tolerance_col(
    cand_vals = cmp$value, ref_vals = cmp$value__reference,
    abs_tol = 0.1, rel_tol = 0.05, na_equal = TRUE
  )
  expect_equal(out$value__absdiff, kernel$absdiff)
  expect_equal(out$value__thresh, kernel$thresh)

  # No-op on an empty column set
  expect_identical(add_diff_columns(cmp, character(0), rules, "__reference", TRUE), cmp)
})
