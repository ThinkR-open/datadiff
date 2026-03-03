test_that("%||% operator works correctly", {
  expect_equal(object = NULL %||% "default", expected = "default")
  expect_equal(object = "value" %||% "default", expected = "value")
  expect_equal(object = 0 %||% 42, expected = 0)
  expect_equal(object = NULL %||% NULL, expected = NULL)
})

test_that("normalize_text handles basic cases", {
  # Test case insensitive
  expect_equal(object = normalize_text(c("HELLO", "world"), case_insensitive = TRUE), expected = c("hello", "world"))
  expect_equal(object = normalize_text(c("HELLO", "world"), case_insensitive = FALSE), expected = c("HELLO", "world"))

  # Test trim
  expect_equal(object = normalize_text(c("  hello  ", "  world  "), trim = TRUE), expected = c("hello", "world"))
  expect_equal(object = normalize_text(c("  hello  ", "  world  "), trim = FALSE), expected = c("  hello  ", "  world  "))

  # Test both
  expect_equal(object = normalize_text(c("  HELLO  ", "  WORLD  "), case_insensitive = TRUE, trim = TRUE), expected = c("hello", "world"))

  # Test non-character input
  expect_equal(object = normalize_text(123), expected = 123)
  expect_equal(object = normalize_text(c(1, 2, 3)), expected = c(1, 2, 3))
})

test_that("preprocess_dataframe applies transformations correctly", {
  df <- data.frame(
    text_col = c("  HELLO  ", "WORLD"),
    num_col = c(1, 2),
    case_col = c("MiXeD", "CaSe")
  )

  # Test with case_insensitive and trim
  rules <- list(
    text_col = list(case_insensitive = TRUE, trim = TRUE),
    num_col = list(),  # No transformation for numeric
    case_col = list(case_insensitive = TRUE)
  )

  result <- preprocess_dataframe(df, rules)

  expect_equal(result$text_col, c("hello", "world"))
  expect_equal(result$num_col, c(1, 2))  # Unchanged
  expect_equal(result$case_col, c("mixed", "case"))
})

test_that("preprocess_dataframe handles equal_mode normalized", {
  df <- data.frame(text_col = c("  HELLO  ", "WORLD"))

  rules <- list(text_col = list(equal_mode = "normalized"))

  result <- preprocess_dataframe(df, rules)

  # Should not apply any transformation since case_insensitive and trim are not set
  expect_equal(result$text_col, c("  HELLO  ", "WORLD"))
})

test_that("preprocess_dataframe handles mixed rules", {
  df <- data.frame(
    col1 = c("  TRIM  ", "no_trim"),
    col2 = c("UPPER", "lower"),
    col3 = c("  BOTH  ", "CaSe")
  )

  rules <- list(
    col1 = list(trim = TRUE),
    col2 = list(case_insensitive = TRUE),
    col3 = list(trim = TRUE, case_insensitive = TRUE)
  )

  result <- preprocess_dataframe(df, rules)

  expect_equal(result$col1, c("TRIM", "no_trim"))
  expect_equal(result$col2, c("upper", "lower"))
  expect_equal(result$col3, c("both", "case"))
})

test_that("preprocess_dataframe handles empty rules", {
  df <- data.frame(text_col = c("  TEST  ", "DATA"))

  rules <- list(text_col = list())

  result <- preprocess_dataframe(df, rules)

  # Should not apply any transformation
  expect_equal(result$text_col, c("  TEST  ", "DATA"))
})
