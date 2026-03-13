test_that("validate_row_counts returns validation info", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:3)

  # Test matching expected count
  rules <- list(row_validation = list(check_count = TRUE, expected_count = 3, tolerance = 0))
  result <- validate_row_counts(ref, cand, rules)
  expect_true(result$check_count)
  expect_equal(result$ref_count, 3)
  expect_equal(result$cand_count, 3)
  expect_equal(result$expected_count, 3)
  expect_equal(result$tolerance, 0)

  # Test within tolerance
  rules_tol <- list(row_validation = list(check_count = TRUE, expected_count = 4, tolerance = 2))
  result_tol <- validate_row_counts(ref, cand, rules_tol)
  expect_equal(result_tol$expected_count, 4)
  expect_equal(result_tol$tolerance, 2)

  # Test exceeding tolerance
  rules_fail <- list(row_validation = list(check_count = TRUE, expected_count = 5, tolerance = 1))
  result_fail <- validate_row_counts(ref, cand, rules_fail)
  expect_equal(result_fail$expected_count, 5)
  expect_equal(result_fail$tolerance, 1)

  # Test exact match required (tolerance = 0)
  rules_exact <- list(row_validation = list(check_count = TRUE, expected_count = 3, tolerance = 0))
  result_exact <- validate_row_counts(ref, cand, rules_exact)
  expect_equal(result_exact$expected_count, 3)
  expect_equal(result_exact$tolerance, 0)

  cand_wrong <- data.frame(a = 1:4)
  result_wrong <- validate_row_counts(ref, cand_wrong, rules_exact)
  expect_equal(result_wrong$cand_count, 4)
})

test_that("validate_row_counts with check_count = TRUE and reference count", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:3)

  # Test matching reference count
  rules <- list(row_validation = list(check_count = TRUE, tolerance = 0))
  result <- validate_row_counts(ref, cand, rules)
  expect_true(result$check_count)
  expect_null(result$expected_count)
  expect_equal(result$tolerance, 0)

  # Test within tolerance
  rules_tol <- list(row_validation = list(check_count = TRUE, tolerance = 1))
  cand_tol <- data.frame(a = 1:4)
  result_tol <- validate_row_counts(ref, cand_tol, rules_tol)
  expect_equal(result_tol$tolerance, 1)
  expect_equal(result_tol$cand_count, 4)

  # Test exceeding tolerance
  rules_fail <- list(row_validation = list(check_count = TRUE, tolerance = 0))
  result_fail <- validate_row_counts(ref, cand_tol, rules_fail)
  expect_equal(result_fail$tolerance, 0)

  # Test candidate with fewer rows
  cand_fewer <- data.frame(a = 1:2)
  rules_tol2 <- list(row_validation = list(check_count = TRUE, tolerance = 2))
  result_fewer <- validate_row_counts(ref, cand_fewer, rules_tol2)
  expect_equal(result_fewer$cand_count, 2)
  expect_equal(result_fewer$tolerance, 2)
})

test_that("validate_row_counts with check_count = FALSE does nothing", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:4)  # Different row count

  rules <- list(row_validation = list(check_count = FALSE))
  expect_no_error(validate_row_counts(ref, cand, rules))

  # Also test with missing row_validation
  rules_missing <- list()
  expect_no_error(validate_row_counts(ref, cand, rules_missing))
})

test_that("validate_row_counts handles edge cases", {
  ref <- data.frame(a = 1:3)
  cand <- data.frame(a = 1:3)

  # Test tolerance = NULL (should default to 0)
  rules <- list(row_validation = list(check_count = TRUE, expected_count = 3))
  result <- validate_row_counts(ref, cand, rules)
  expect_equal(result$tolerance, 0)

  # Test with negative tolerance
  rules_neg <- list(row_validation = list(check_count = TRUE, expected_count = 2, tolerance = -1))
  result_neg <- validate_row_counts(ref, cand, rules_neg)
  expect_equal(result_neg$tolerance, -1)
})

test_that("compare_datasets_from_yaml integrates row validation correctly", {
  ref <- data.frame(id = 1:3, value = 1:3)
  cand <- data.frame(id = 1:3, value = 1:3)

  # Test with check_count = TRUE and matching count
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
row_validation:
  check_count: true
  expected_count: 3
by_type:
  numeric:
    abs: 0.01
'

  yaml_path <- tempfile(fileext = ".yaml")
  yaml_fail_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(yaml_path, yaml_fail_path)), add = TRUE)

  writeLines(yaml_content, yaml_path)
  expect_no_error(compare_datasets_from_yaml(ref, cand, path = yaml_path))

  # Test with check_count = TRUE and wrong count
  yaml_fail <- '
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
row_validation:
  check_count: true
  expected_count: 5
by_type:
  numeric:
    abs: 0.01
'

  writeLines(yaml_fail, yaml_fail_path)
  result_fail <- compare_datasets_from_yaml(ref, cand, path = yaml_fail_path)
  expect_false(result_fail$all_passed)
})
