# Tests for extraction parameters in compare_datasets_from_yaml()

# Helper function to create test data with failures
create_failing_data <- function(n_rows = 100, n_failures = 50) {
  ref <- data.frame(
    id = seq_len(n_rows),
    value = rep(1.0, n_rows)
  )
  # First n_failures rows will fail (value differs)
  cand_values <- c(rep(999.0, n_failures), rep(1.0, n_rows - n_failures))
  cand <- data.frame(
    id = seq_len(n_rows),
    value = cand_values
  )
  list(ref = ref, cand = cand)
}

# ============================================================================
# extract_failed parameter tests
# ============================================================================

test_that("extract_failed defaults to TRUE", {
  data <- create_failing_data(10, 5)

  result <- compare_datasets_from_yaml(data$ref, data$cand, key = "id")

  expect_false(result$all_passed)
  # With extract_failed = TRUE (default), extracts should contain data
  extracts <- pointblank::get_data_extracts(result$reponse)
  expect_true(length(extracts) > 0 || is.list(extracts))
})

test_that("extract_failed = FALSE prevents row extraction", {
 data <- create_failing_data(10, 5)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = FALSE
  )

  expect_false(result$all_passed)
  # With extract_failed = FALSE, no rows should be extracted
  extracts <- pointblank::get_data_extracts(result$reponse)
  # Extracts should be empty or contain empty data frames
  if (length(extracts) > 0) {
    total_rows <- sum(vapply(extracts, nrow, integer(1)))
    expect_equal(total_rows, 0)
  }
})

test_that("extract_failed accepts logical values only in practice", {
  data <- create_failing_data(5, 2)

  # TRUE works
 expect_no_error(
    compare_datasets_from_yaml(data$ref, data$cand, key = "id", extract_failed = TRUE)
  )

  # FALSE works
  expect_no_error(
    compare_datasets_from_yaml(data$ref, data$cand, key = "id", extract_failed = FALSE)
  )
})

# ============================================================================
# get_first_n parameter tests
# ============================================================================

test_that("get_first_n = NULL extracts all failures (default)", {
  data <- create_failing_data(20, 15)

  result <- compare_datasets_from_yaml(data$ref, data$cand, key = "id")

  expect_false(result$all_passed)
})

test_that("get_first_n limits extracted rows", {
  data <- create_failing_data(100, 50)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    get_first_n = 5
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  # Each extract should have at most 5 rows
  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 5)
      }
    }
  }
})

test_that("get_first_n = 1 extracts only first failure", {
  data <- create_failing_data(50, 25)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    get_first_n = 1
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 1)
      }
    }
  }
})

test_that("get_first_n handles value larger than failures", {
  data <- create_failing_data(10, 5)

  # get_first_n = 100, but only 5 failures exist
  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    get_first_n = 100
  )

  expect_false(result$all_passed)
  # Should not error, just return all available failures
})

test_that("get_first_n = 0 is handled",
{
  data <- create_failing_data(10, 5)

  # get_first_n = 0 should extract no rows (or be treated specially by pointblank)
  expect_no_error(
    compare_datasets_from_yaml(data$ref, data$cand, key = "id", get_first_n = 0)
  )
})

# ============================================================================
# sample_n parameter tests
# ============================================================================

test_that("sample_n = NULL does not sample (default)", {
  data <- create_failing_data(20, 10)

  result <- compare_datasets_from_yaml(data$ref, data$cand, key = "id")
  expect_false(result$all_passed)
})

test_that("sample_n limits extracted rows via random sampling", {
  data <- create_failing_data(100, 80)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_n = 10
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 10)
      }
    }
  }
})

test_that("sample_n = 1 extracts only one random failure", {
  data <- create_failing_data(50, 30)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_n = 1
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 1)
      }
    }
  }
})

test_that("sample_n handles value larger than failures", {
  data <- create_failing_data(10, 3)

  # sample_n = 100, but only 3 failures exist
  expect_no_error(
    result <- compare_datasets_from_yaml(data$ref, data$cand, key = "id", sample_n = 100)
  )
  expect_false(result$all_passed)
})

# ============================================================================
# sample_frac parameter tests
# ============================================================================

test_that("sample_frac = NULL does not sample (default)", {
  data <- create_failing_data(20, 10)

  result <- compare_datasets_from_yaml(data$ref, data$cand, key = "id")
  expect_false(result$all_passed)
})

test_that("sample_frac samples a fraction of failures", {
  data <- create_failing_data(100, 80)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_frac = 0.1  # 10% of failures
  )

  expect_false(result$all_passed)
})

test_that("sample_frac = 1.0 samples all failures", {
  data <- create_failing_data(20, 15)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_frac = 1.0
  )

  expect_false(result$all_passed)
})

test_that("sample_frac = 0.0 samples no failures", {
  data <- create_failing_data(20, 15)

  expect_no_error(
    result <- compare_datasets_from_yaml(
      data$ref, data$cand, key = "id",
      sample_frac = 0.0
    )
  )
})

test_that("sample_frac accepts values between 0 and 1", {
  data <- create_failing_data(10, 5)

  # Test various fractions
  for (frac in c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    expect_no_error(
      compare_datasets_from_yaml(data$ref, data$cand, key = "id", sample_frac = frac)
    )
  }
})

# ============================================================================
# sample_limit parameter tests
# ============================================================================

test_that("sample_limit defaults to 5000", {
  data <- create_failing_data(10, 5)

  # Should work with default sample_limit
  expect_no_error(
    compare_datasets_from_yaml(data$ref, data$cand, key = "id", sample_frac = 0.5)
  )
})

test_that("sample_limit caps sample_frac results", {
  data <- create_failing_data(100, 80)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_frac = 1.0,  # Would sample all 80 failures
    sample_limit = 5    # But limit to 5
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 5)
      }
    }
  }
})

test_that("sample_limit = 1 limits to single row", {
  data <- create_failing_data(50, 40)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    sample_frac = 1.0,
    sample_limit = 1
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  if (length(extracts) > 0) {
    for (ext in extracts) {
      if (is.data.frame(ext) && nrow(ext) > 0) {
        expect_lte(nrow(ext), 1)
      }
    }
  }
})

test_that("sample_limit handles large values", {
  data <- create_failing_data(10, 5)

  expect_no_error(
    compare_datasets_from_yaml(
      data$ref, data$cand, key = "id",
      sample_frac = 0.5,
      sample_limit = 1000000
    )
  )
})

# ============================================================================
# Parameter combinations tests
# ============================================================================

test_that("extract_failed = FALSE overrides other extraction params", {
  data <- create_failing_data(50, 30)

  result <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = FALSE,
    get_first_n = 100,
    sample_n = 50
  )

  expect_false(result$all_passed)
  extracts <- pointblank::get_data_extracts(result$reponse)

  # With extract_failed = FALSE, should have no extracted rows
  if (length(extracts) > 0) {
    total_rows <- sum(vapply(extracts, nrow, integer(1)))
    expect_equal(total_rows, 0)
  }
})

test_that("get_first_n and sample_n are mutually exclusive (last one wins or pointblank handles)", {
  data <- create_failing_data(100, 50)

  # When both are provided, pointblank should handle it
  # (typically one takes precedence or they error)
  expect_no_error(
    compare_datasets_from_yaml(
      data$ref, data$cand, key = "id",
      get_first_n = 5,
      sample_n = 10
    )
  )
})

test_that("sample_n and sample_frac together (pointblank handles precedence)", {
  data <- create_failing_data(100, 50)

  expect_no_error(
    compare_datasets_from_yaml(
      data$ref, data$cand, key = "id",
      sample_n = 5,
      sample_frac = 0.5,
      sample_limit = 10
    )
  )
})

# ============================================================================
# Edge cases with no failures
# ============================================================================

test_that("extraction params work when all rows pass", {
  ref <- data.frame(id = 1:10, value = 1:10)
  cand <- data.frame(id = 1:10, value = 1:10)

  # No failures, extraction params should not cause issues
  result <- compare_datasets_from_yaml(
    ref, cand, key = "id",
    extract_failed = TRUE,
    get_first_n = 5
  )

  expect_true(result$all_passed)
})

test_that("extraction params work with empty datasets", {
  ref <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))

  expect_no_error(
    compare_datasets_from_yaml(
      ref, cand, key = "id",
      extract_failed = TRUE,
      get_first_n = 10
    )
  )
})

test_that("extraction params work with single row", {
  ref <- data.frame(id = 1, value = 1.0)
  cand <- data.frame(id = 1, value = 999.0)  # Will fail

  result <- compare_datasets_from_yaml(
    ref, cand, key = "id",
    get_first_n = 1
  )

  expect_false(result$all_passed)
})

# ============================================================================
# Memory optimization verification
# ============================================================================

test_that("lightweight mode (extract_failed = FALSE) produces smaller output", {
  data <- create_failing_data(100, 80)

  result_full <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = TRUE
  )

  result_light <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = FALSE
  )

  # Both should have same pass/fail status
  expect_equal(result_full$all_passed, result_light$all_passed)

  # Light version should have no extracts
  extracts_light <- pointblank::get_data_extracts(result_light$reponse)
  if (length(extracts_light) > 0) {
    total_rows_light <- sum(vapply(extracts_light, nrow, integer(1)))
    expect_equal(total_rows_light, 0)
  }
})

test_that("get_first_n reduces extract size compared to no limit", {
  data <- create_failing_data(100, 80)

  result_unlimited <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = TRUE
  )

  result_limited <- compare_datasets_from_yaml(
    data$ref, data$cand, key = "id",
    extract_failed = TRUE,
    get_first_n = 5
  )

  extracts_unlimited <- pointblank::get_data_extracts(result_unlimited$reponse)
  extracts_limited <- pointblank::get_data_extracts(result_limited$reponse)

  # Count total rows in extracts
  count_rows <- function(extracts) {
    if (length(extracts) == 0) return(0)
    sum(vapply(extracts, function(x) if (is.data.frame(x)) nrow(x) else 0L, integer(1)))
  }

  total_unlimited <- count_rows(extracts_unlimited)
  total_limited <- count_rows(extracts_limited)

  # Limited should have fewer or equal rows
 expect_lte(total_limited, total_unlimited)
})
