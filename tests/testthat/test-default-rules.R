# Tests for optional YAML / default rules functionality

test_that("compare_datasets_from_yaml works without path (default rules)", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand)

  expect_true(result$all_passed)
  expect_s3_class(result$reponse, "ptblank_agent")
})

test_that("compare_datasets_from_yaml with key but no path works", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
  expect_s3_class(result$reponse, "ptblank_agent")
})

test_that("compare_datasets_from_yaml detects differences without path", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 999.0))  # Different value

 result <- compare_datasets_from_yaml(ref, cand)

  expect_false(result$all_passed)
})

test_that("compare_datasets_from_yaml with key detects differences without path", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 999.0))  # Different value

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_false(result$all_passed)
})

test_that("default rules use correct tolerances for numeric columns", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  # Small difference within default tolerance (1e-9)
  cand <- data.frame(id = 1:3, value = c(1.0 + 1e-10, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("default rules detect differences outside tolerance", {
 ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  # Difference outside default tolerance
  cand <- data.frame(id = 1:3, value = c(1.0 + 1e-5, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_false(result$all_passed)
})

test_that("default rules work with character columns", {
  ref <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  cand <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("default rules detect character differences", {
  ref <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
  cand <- data.frame(id = 1:3, name = c("Alice", "Bob", "David"))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_false(result$all_passed)
})

test_that("default rules work with mixed column types", {
  ref <- data.frame(
    id = 1:3,
    num = c(1.1, 2.2, 3.3),
    int = 10L:12L,
    char = c("a", "b", "c"),
    logic = c(TRUE, FALSE, TRUE)
  )
  cand <- ref

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("default rules work with date columns", {
  ref <- data.frame(
    id = 1:3,
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
  )
  cand <- ref

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("default rules detect date differences", {
  ref <- data.frame(
    id = 1:3,
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
  )
  cand <- data.frame(
    id = 1:3,
    date = as.Date(c("2024-01-01", "2024-02-01", "2025-03-01"))  # Different year
  )

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_false(result$all_passed)
})

test_that("default rules handle NA values correctly", {
  ref <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  # Default na_equal = TRUE, so NAs should match
 expect_true(result$all_passed)
})

test_that("positional comparison works without key", {
  ref <- data.frame(a = 1:5, b = letters[1:5])
  cand <- data.frame(a = 1:5, b = letters[1:5])

  result <- compare_datasets_from_yaml(ref, cand)

  expect_true(result$all_passed)
})

test_that("positional comparison detects row order differences", {
  ref <- data.frame(a = 1:3, b = c("x", "y", "z"))
  cand <- data.frame(a = c(3, 2, 1), b = c("z", "y", "x"))  # Reversed order

  result <- compare_datasets_from_yaml(ref, cand)

  # Without key, comparison is positional, so order matters
  expect_false(result$all_passed)
})

test_that("keyed comparison handles reordered rows", {
  ref <- data.frame(id = 1:3, value = c(10, 20, 30))
  cand <- data.frame(id = c(3, 1, 2), value = c(30, 10, 20))  # Reordered

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  # With key, join matches by id, so order doesn't matter
 expect_true(result$all_passed)
})

test_that("write_rules_template works without key", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  temp_path <- tempfile(fileext = ".yaml")

  expect_no_error(write_rules_template(df, path = temp_path))

  rules <- read_rules(temp_path)
  expect_null(rules$defaults$keys)
  expect_equal(rules$version, 1)
  expect_true("a" %in% names(rules$by_name))
  expect_true("b" %in% names(rules$by_name))

  unlink(temp_path)
})

test_that("write_rules_template with key still works", {
  df <- data.frame(id = 1:3, value = c(1.1, 2.2, 3.3))
  temp_path <- tempfile(fileext = ".yaml")

  expect_no_error(write_rules_template(df, key = "id", path = temp_path))

  rules <- read_rules(temp_path)
  expect_equal(rules$defaults$keys, "id")

  unlink(temp_path)
})

test_that("default rules applied_rules are returned correctly", {
  ref <- data.frame(id = 1:3, num = c(1.0, 2.0, 3.0), char = c("a", "b", "c"))
  cand <- ref

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true("num" %in% names(result$applied_rules))
  expect_true("char" %in% names(result$applied_rules))
  # Check that numeric rules have abs/rel tolerances
  expect_true(!is.null(result$applied_rules$num$abs))
  expect_true(!is.null(result$applied_rules$num$rel))
})

test_that("custom label is used when path is NULL", {
  ref <- data.frame(id = 1:3, value = 1:3)
  cand <- ref

  result <- compare_datasets_from_yaml(ref, cand, label = "Custom Test Label")

  # The label should be set in the agent
  expect_s3_class(result$reponse, "ptblank_agent")
})

test_that("comparison without path handles missing columns", {
  ref <- data.frame(id = 1:3, a = 1:3, b = 4:6)
  cand <- data.frame(id = 1:3, a = 1:3)  # Missing column b

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true("b" %in% result$missing_in_candidate)
})

test_that("comparison without path handles extra columns", {
  ref <- data.frame(id = 1:3, a = 1:3)
  cand <- data.frame(id = 1:3, a = 1:3, extra = 7:9)  # Extra column

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true("extra" %in% result$extra_in_candidate)
})

test_that("comparison without path works with tibbles", {
  skip_if_not_installed("tibble")

  ref <- tibble::tibble(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- tibble::tibble(id = 1:3, value = c(1.0, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("comparison without path handles empty dataframes",
{
  ref <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  expect_true(result$all_passed)
})

test_that("comparison without path with composite key", {
  ref <- data.frame(
    key1 = c(1, 1, 2),
    key2 = c("a", "b", "a"),
    value = c(10, 20, 30)
  )
  cand <- data.frame(
    key1 = c(2, 1, 1),
    key2 = c("a", "b", "a"),
    value = c(30, 20, 10)
  )

  result <- compare_datasets_from_yaml(ref, cand, key = c("key1", "key2"))

  expect_true(result$all_passed)
})
