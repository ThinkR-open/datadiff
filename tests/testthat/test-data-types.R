test_that("detect_column_types works correctly", {
  df <- data.frame(
    int_col = 1:3,
    num_col = c(1.1, 2.2, 3.3),
    char_col = c("a", "b", "c"),
    log_col = c(TRUE, FALSE, TRUE)
  )

  types <- detect_column_types(df)
  expect_equal(object = unname(types["int_col"]), expected = "integer")
  expect_equal(object = unname(types["num_col"]), expected = "numeric")
  expect_equal(object = unname(types["char_col"]), expected = "character")
  expect_equal(object = unname(types["log_col"]), expected = "logical")
})

test_that("detect_column_types handles all data types", {
  # Test with Date
  df_date <- data.frame(date_col = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")))
  types_date <- detect_column_types(df_date)
  expect_equal(object = unname(types_date["date_col"]), expected = "date")

  # Test with POSIXt (datetime)
  df_datetime <- data.frame(dt_col = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 13:00:00")))
  types_datetime <- detect_column_types(df_datetime)
  expect_equal(object = unname(types_datetime["dt_col"]), expected = "datetime")

  # Test with factor (should be character)
  df_factor <- data.frame(factor_col = factor(c("A", "B", "C")))
  types_factor <- detect_column_types(df_factor)
  expect_equal(object = unname(types_factor["factor_col"]), expected = "character")
})

test_that("derive_column_rules merges type and name rules", {
  df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3), c = c("x", "y", "z"))

  rules <- list(
    by_type = list(numeric = list(abs = 0.1), character = list(case_insensitive = TRUE)),
    by_name = list(a = list(abs = 0.5), c = list(trim = TRUE))
  )

  col_rules <- derive_column_rules(df, rules)

  # Check that type rules are applied
  expect_equal(object = col_rules$b$abs, expected = 0.1)
  expect_true(object = col_rules$c$case_insensitive)

  # Check that name rules override type rules
  expect_equal(object = col_rules$a$abs, expected = 0.5)
  expect_true(object = col_rules$c$trim)
})

test_that("analyze_columns ignores specified columns", {
  ref <- data.frame(a = 1, b = 2, c = 3, d = 4)
  cand <- data.frame(a = 1, b = 2, c = 3, e = 5)

  # Without ignore_columns
  result1 <- analyze_columns(ref, cand)
  expect_equal(object = length(result1$missing_in_candidate), expected = 1)
  expect_equal(object = length(result1$extra_in_candidate), expected = 1)

  # With ignore_columns
  result2 <- analyze_columns(ref, cand, ignore_columns = c("d", "e"))
  expect_equal(object = length(result2$missing_in_candidate), expected = 0)
  expect_equal(object = length(result2$extra_in_candidate), expected = 0)
  expect_equal(object = result2$ignored_cols, expected = c("d", "e"))
})

test_that("validate_row_counts works correctly", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:3)

  # Test with check_count = TRUE and matching counts
  rules <- list(row_validation = list(check_count = TRUE, expected_count = 3, tolerance = 0))
  result <- validate_row_counts(ref, cand, rules)
  expect_true(result$check_count)
  expect_equal(result$expected_count, 3)
  expect_equal(result$tolerance, 0)

  # Test with check_count = TRUE and non-matching counts
  rules_fail <- list(row_validation = list(check_count = TRUE, expected_count = 5, tolerance = 0))
  result_fail <- validate_row_counts(ref, cand, rules_fail)
  expect_equal(result_fail$expected_count, 5)
  expect_equal(result_fail$cand_count, 3)

  # Test with tolerance
  rules_tolerance <- list(row_validation = list(check_count = TRUE, expected_count = 4, tolerance = 2))
  result_tolerance <- validate_row_counts(ref, cand, rules_tolerance)
  expect_equal(result_tolerance$expected_count, 4)
  expect_equal(result_tolerance$tolerance, 2)

  # Test with check_count = FALSE (should do nothing)
  rules_disabled <- list(row_validation = list(check_count = FALSE))
  result_disabled <- validate_row_counts(ref, cand, rules_disabled)
  expect_false(result_disabled$check_count)
})

test_that("validate_row_counts handles reference-based validation", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:3)

  # Test with no expected_count (uses reference count)
  rules <- list(row_validation = list(check_count = TRUE, tolerance = 0))
  result <- validate_row_counts(ref, cand, rules)
  expect_true(result$check_count)
  expect_null(result$expected_count)
  expect_equal(result$tolerance, 0)

  # Test with different counts and no expected_count
  cand_diff <- data.frame(a = 1:4)
  rules_diff <- list(row_validation = list(check_count = TRUE, tolerance = 0))
  result_diff <- validate_row_counts(ref, cand_diff, rules_diff)
  expect_equal(result_diff$ref_count, 3)
  expect_equal(result_diff$cand_count, 4)
})
