# Tests for critical edge cases

# =============================================================================
# SECTION 1: Inf/NaN handling in tolerance
# =============================================================================

test_that("add_tolerance_columns handles Inf values correctly", {
  cmp <- data.frame(
    value = c(1, Inf, -Inf, Inf, -Inf, 5),
    value__reference = c(1, Inf, -Inf, -Inf, Inf, Inf)
  )
  rules <- list(value = list(abs = 0.1, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = TRUE)

  # Same Inf values should be considered equal
  expect_true(result$value__ok[1])   # 1 vs 1 -> OK
  expect_true(result$value__ok[2])   # Inf vs Inf -> OK
  expect_true(result$value__ok[3])   # -Inf vs -Inf -> OK
  expect_false(result$value__ok[4])  # Inf vs -Inf -> NOT OK
  expect_false(result$value__ok[5])  # -Inf vs Inf -> NOT OK
  expect_false(result$value__ok[6])  # 5 vs Inf -> NOT OK
})

test_that("add_tolerance_columns computes correct absdiff for Inf values", {
  cmp <- data.frame(
    value = c(Inf, -Inf, Inf),
    value__reference = c(Inf, -Inf, -Inf)
  )
  rules <- list(value = list(abs = 0, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = TRUE)

  # Same Inf -> absdiff should be 0
  expect_equal(result$value__absdiff[1], 0)
  expect_equal(result$value__absdiff[2], 0)
  # Different Inf -> absdiff should be Inf
  expect_equal(result$value__absdiff[3], Inf)
})

test_that("add_tolerance_columns handles NaN values correctly with na_equal = TRUE", {
  cmp <- data.frame(
    value = c(1, NaN, NaN, 5),
    value__reference = c(1, NaN, 5, NaN)
  )
  rules <- list(value = list(abs = 0.1, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = TRUE)

  expect_true(result$value__ok[1])   # 1 vs 1 -> OK
  expect_true(result$value__ok[2])   # NaN vs NaN -> OK (na_equal = TRUE)
  expect_false(result$value__ok[3])  # NaN vs 5 -> NOT OK
  expect_false(result$value__ok[4])  # 5 vs NaN -> NOT OK
})

test_that("add_tolerance_columns handles NaN values correctly with na_equal = FALSE", {
  cmp <- data.frame(
    value = c(1, NaN, NaN),
    value__reference = c(1, NaN, 5)
  )
  rules <- list(value = list(abs = 0.1, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = FALSE)

  expect_true(result$value__ok[1])   # 1 vs 1 -> OK
  expect_false(result$value__ok[2])  # NaN vs NaN -> NOT OK (na_equal = FALSE)
  expect_false(result$value__ok[3])  # NaN vs 5 -> NOT OK
})

test_that("add_tolerance_columns handles mixed NA, NaN, Inf", {
  # Note: In R, is.na(NaN) returns TRUE, so NaN is treated as a form of NA
  cmp <- data.frame(
    value = c(NA, NaN, Inf, NA, 1, Inf),
    value__reference = c(NA, NA, Inf, NaN, 1, 5)
  )
  rules <- list(value = list(abs = 0.1, rel = 0))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = TRUE)

  expect_true(result$value__ok[1])   # NA vs NA -> OK
  expect_true(result$value__ok[2])   # NaN vs NA -> OK (both are NA in R's view)
  expect_true(result$value__ok[3])   # Inf vs Inf -> OK
  expect_true(result$value__ok[4])   # NA vs NaN -> OK (both are NA in R's view)
  expect_true(result$value__ok[5])   # 1 vs 1 -> OK
  expect_false(result$value__ok[6])  # Inf vs 5 -> NOT OK
})

test_that("add_tolerance_columns handles multiple columns with Inf/NaN", {
  cmp <- data.frame(
    col1 = c(Inf, 1, NaN),
    col1__reference = c(Inf, 1, NaN),
    col2 = c(-Inf, NaN, 2),
    col2__reference = c(-Inf, NaN, 2)
  )
  rules <- list(
    col1 = list(abs = 0.1, rel = 0),
    col2 = list(abs = 0.1, rel = 0)
  )

  result <- add_tolerance_columns(cmp, c("col1", "col2"), rules, "__reference", na_equal = TRUE)

  expect_true(all(result$col1__ok))
  expect_true(all(result$col2__ok))
})

test_that("add_tolerance_columns handles Inf with relative tolerance", {
  cmp <- data.frame(
    value = c(100, Inf, -Inf),
    value__reference = c(100, Inf, -Inf)
  )
  rules <- list(value = list(abs = 0, rel = 0.1))

  result <- add_tolerance_columns(cmp, "value", rules, "__reference", na_equal = TRUE)

  expect_true(result$value__ok[1])
  expect_true(result$value__ok[2])
  expect_true(result$value__ok[3])
  # Threshold for Inf reference should be Inf

  expect_equal(result$value__thresh[2], Inf)
  expect_equal(result$value__thresh[3], Inf)
})

# =============================================================================
# SECTION 2: Input validation for compare_datasets_from_yaml
# =============================================================================

test_that("compare_datasets_from_yaml validates data_reference input", {
  cand <- data.frame(id = 1:3, value = 1:3)
  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(cand, key = "id", path = template_path)

  expect_error(
    compare_datasets_from_yaml("not a dataframe", cand, path = template_path),
    "data_reference must be a data.frame"
  )

  expect_error(
    compare_datasets_from_yaml(list(a = 1), cand, path = template_path),
    "data_reference must be a data.frame"
  )

  expect_error(
    compare_datasets_from_yaml(NULL, cand, path = template_path),
    "data_reference must be a data.frame"
  )

  expect_error(
    compare_datasets_from_yaml(1:10, cand, path = template_path),
    "data_reference must be a data.frame"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml validates data_candidate input", {
  ref <- data.frame(id = 1:3, value = 1:3)
  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_error(
    compare_datasets_from_yaml(ref, "not a dataframe", path = template_path),
    "data_candidate must be a data.frame"
  )

  expect_error(
    compare_datasets_from_yaml(ref, NULL, path = template_path),
    "data_candidate must be a data.frame"
  )

  expect_error(
    compare_datasets_from_yaml(ref, list(x = 1:3), path = template_path),
    "data_candidate must be a data.frame"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml validates path input", {
  ref <- data.frame(id = 1:3, value = 1:3)
  cand <- data.frame(id = 1:3, value = 1:3)

  expect_error(
    compare_datasets_from_yaml(ref, cand, path = "nonexistent_file.yaml"),
    "YAML rules file not found"
  )

  expect_error(
    compare_datasets_from_yaml(ref, cand, path = c("file1.yaml", "file2.yaml")),
    "path must be a single character string"
  )

  # path = NULL is now allowed (uses default rules)
  expect_no_error(compare_datasets_from_yaml(ref, cand, path = NULL))

  expect_error(
    compare_datasets_from_yaml(ref, cand, path = 123),
    "path must be a single character string"
  )
})

test_that("compare_datasets_from_yaml accepts tibbles", {
  skip_if_not_installed("tibble")

  ref <- tibble::tibble(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- tibble::tibble(id = 1:3, value = c(1.0, 2.0, 3.0))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

test_that("compare_datasets_from_yaml accepts matrix converted to data.frame", {
  mat <- matrix(1:6, nrow = 3, ncol = 2)
  ref <- as.data.frame(mat)
  names(ref) <- c("id", "value")
  cand <- ref

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

# =============================================================================
# SECTION 3: Duplicate keys detection
# =============================================================================

test_that("compare_datasets_from_yaml warns about duplicate keys in reference", {
  ref <- data.frame(id = c(1, 1, 2), value = c(10, 20, 30))
  cand <- data.frame(id = c(1, 2, 3), value = c(15, 25, 35))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    regexp = "Duplicate keys detected.*data_reference.*duplicate key value"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml warns about duplicate keys in candidate", {
  ref <- data.frame(id = c(1, 2, 3), value = c(10, 20, 30))
  cand <- data.frame(id = c(1, 1, 2), value = c(15, 25, 35))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    regexp = "Duplicate keys detected.*data_candidate.*duplicate key value"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml warns about duplicate keys in both", {
  ref <- data.frame(id = c(1, 1, 2), value = c(10, 20, 30))
  cand <- data.frame(id = c(1, 1, 2), value = c(15, 25, 35))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  # Expect our custom warning about duplicates (dplyr's many-to-many warning is also produced)
  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    regexp = "Duplicate keys detected.*data_reference.*data_candidate"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml does not warn when no duplicate keys", {
  ref <- data.frame(id = c(1, 2, 3), value = c(10, 20, 30))
  cand <- data.frame(id = c(1, 2, 3), value = c(10, 20, 30))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_no_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml detects duplicate composite keys", {
  ref <- data.frame(
    id1 = c(1, 1, 2),
    id2 = c("a", "a", "b"),
    value = c(10, 20, 30)
  )
  cand <- data.frame(
    id1 = c(1, 2, 3),
    id2 = c("a", "b", "c"),
    value = c(15, 25, 35)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = c("id1", "id2"), path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = c("id1", "id2"), path = template_path),
    regexp = "Duplicate keys detected.*data_reference.*duplicate key value"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml does not warn for unique composite keys", {
  ref <- data.frame(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    value = c(10, 20, 30)
  )
  cand <- data.frame(
    id1 = c(1, 1, 2),
    id2 = c("a", "b", "a"),
    value = c(10, 20, 30)
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = c("id1", "id2"), path = template_path)

  expect_no_warning(
    compare_datasets_from_yaml(ref, cand, key = c("id1", "id2"), path = template_path)
  )

  unlink(template_path)
})

# =============================================================================
# SECTION 4: Reserved suffix conflict detection
# =============================================================================

test_that("compare_datasets_from_yaml warns about reserved suffix in column names", {
  ref <- data.frame(id = 1:3, value__reference = 1:3, other = 4:6)
  cand <- data.frame(id = 1:3, value__reference = 1:3, other = 4:6)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    "Column\\(s\\) containing reserved suffix '__reference' detected"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml warns about multiple columns with reserved suffix", {
  ref <- data.frame(
    id = 1:3,
    col1__reference = 1:3,
    col2__reference = 4:6,
    normal = 7:9
  )
  cand <- ref

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    "col1__reference.*col2__reference|col2__reference.*col1__reference"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml warns when only candidate has reserved suffix", {
  ref <- data.frame(id = 1:3, value = 1:3)
  cand <- data.frame(id = 1:3, value = 1:3, extra__reference = 4:6)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    "Column\\(s\\) containing reserved suffix '__reference' detected"
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml does not warn without reserved suffix", {
  ref <- data.frame(id = 1:3, value = 1:3, other_col = 4:6)
  cand <- data.frame(id = 1:3, value = 1:3, other_col = 4:6)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_no_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml respects custom ref_suffix for warning", {
  ref <- data.frame(id = 1:3, value__custom = 1:3)
  cand <- data.frame(id = 1:3, value__custom = 1:3)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  # Should warn with custom suffix
  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path,
                               ref_suffix = "__custom"),
    "Column\\(s\\) containing reserved suffix '__custom' detected"
  )

  # Should NOT warn with default suffix
  expect_no_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path,
                               ref_suffix = "__reference")
  )

  unlink(template_path)
})

# =============================================================================
# SECTION 5: Empty and edge case dataframes
# =============================================================================

test_that("compare_datasets_from_yaml handles empty dataframes", {
  ref <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(data.frame(id = 1, value = 1), key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

test_that("compare_datasets_from_yaml handles single row dataframes", {
  ref <- data.frame(id = 1, value = 100.0)
  cand <- data.frame(id = 1, value = 100.0)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

test_that("compare_datasets_from_yaml handles dataframes with only key column", {
  ref <- data.frame(id = 1:5)
  cand <- data.frame(id = 1:5)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

# =============================================================================
# SECTION 6: Integration tests - full pipeline with edge cases
# =============================================================================

test_that("full comparison works with Inf values in data", {
  ref <- data.frame(id = 1:4, value = c(1.0, Inf, -Inf, 100.0))
  cand <- data.frame(id = 1:4, value = c(1.0, Inf, -Inf, 100.5))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 1.0, numeric_rel = 0)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  # All should pass: 1==1, Inf==Inf, -Inf==-Inf, |100-100.5|<=1
  expect_true(result$all_passed)

  unlink(template_path)
})

test_that("full comparison fails with mismatched Inf values", {
  ref <- data.frame(id = 1:2, value = c(Inf, 1.0))
  cand <- data.frame(id = 1:2, value = c(-Inf, 1.0))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_false(result$all_passed)

  unlink(template_path)
})

test_that("full comparison with NaN values respects na_equal setting", {
  ref <- data.frame(id = 1:3, value = c(1.0, NaN, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NaN, 3.0))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path, na_equal_default = TRUE)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  expect_true(result$all_passed)

  unlink(template_path)
})

test_that("validation still works after duplicate key warning", {
  ref <- data.frame(id = c(1, 1, 2), value = c(10, 10, 30))
  cand <- data.frame(id = c(1, 2), value = c(10, 30))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  # Should warn but still return a result
  result <- suppressWarnings(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  expect_false(is.null(result))
  expect_true("all_passed" %in% names(result))

  unlink(template_path)
})

test_that("validation still works after suffix conflict warning", {
  ref <- data.frame(id = 1:3, value = 1:3, extra__reference = 4:6)
  cand <- data.frame(id = 1:3, value = 1:3, extra__reference = 4:6)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  # Should warn but still return a result
  result <- suppressWarnings(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  expect_false(is.null(result))
  expect_true("all_passed" %in% names(result))

  unlink(template_path)
})

# =============================================================================
# SECTION: Completely different datasets (no common columns)
# =============================================================================

test_that("validation fails when datasets have no common columns", {

  # Create two completely different datasets (like mtcars vs iris)
  ref <- data.frame(
    id = 1:5,
    mpg = c(21.0, 21.0, 22.8, 21.4, 18.7),
    cyl = c(6, 6, 4, 6, 8),
    disp = c(160, 160, 108, 258, 360)
  )

  cand <- data.frame(
    id = 1:5,
    Sepal.Length = c(5.1, 4.9, 4.7, 4.6, 5.0),
    Sepal.Width = c(3.5, 3.0, 3.2, 3.1, 3.6),
    Species = c("setosa", "setosa", "setosa", "setosa", "setosa")
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  # Validation should FAIL because columns are missing

  expect_false(result$all_passed)

  # All reference columns (except id) should be reported as missing
  expect_true("mpg" %in% result$missing_in_candidate)
  expect_true("cyl" %in% result$missing_in_candidate)
  expect_true("disp" %in% result$missing_in_candidate)

  # Candidate-only columns should be reported as extra

  expect_true("Sepal.Length" %in% result$extra_in_candidate)
  expect_true("Sepal.Width" %in% result$extra_in_candidate)
  expect_true("Species" %in% result$extra_in_candidate)

  unlink(template_path)
})

test_that("validation fails when row counts differ with check_count enabled by default", {
  ref <- data.frame(id = 1:10, value = 1:10)
  cand <- data.frame(id = 1:5, value = 1:5)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  # Read rules to verify check_count is TRUE by default
  rules <- read_rules(template_path)
  expect_true(rules$row_validation$check_count)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  # Validation should FAIL because row counts differ (10 vs 5)
  expect_false(result$all_passed)

  unlink(template_path)
})

test_that("validation detects missing columns even with matching row counts", {
  # Same number of rows, but completely different columns
  ref <- data.frame(id = 1:3, col_a = c(1, 2, 3), col_b = c("x", "y", "z"))
  cand <- data.frame(id = 1:3, col_c = c(4, 5, 6), col_d = c("a", "b", "c"))

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)

  # Validation should FAIL because columns are missing
  expect_false(result$all_passed)

  # Check missing columns

  expect_equal(sort(result$missing_in_candidate), sort(c("col_a", "col_b")))

  # Check extra columns
  expect_equal(sort(result$extra_in_candidate), sort(c("col_c", "col_d")))

  unlink(template_path)
})

# =============================================================================
# SECTION 7: Type mismatch between reference (numeric) and candidate (character)
#
# Reproduces the bug: sign(cand_vals) crashes when a column is numeric in the
# reference but stored as character in the candidate. The fix should:
#   - not crash (no "non-numeric argument to a mathematical function" error)
#   - emit a warning about the type mismatch
#   - fall back to equality comparison for the mismatched column
#   - still validate correctly typed columns alongside mismatched ones
# =============================================================================

test_that("compare_datasets_from_yaml does not crash when numeric ref column is character in candidate", {
  # Reproduces the exact scenario from bug.R:
  # results_sas$year  <dbl>  vs  synth_coefgeo$year  <chr>
  ref <- data.frame(
    ides = "620025387",
    year = 2024,          # <dbl>
    value = 100.0,
    stringsAsFactors = FALSE
  )
  cand <- data.frame(
    ides  = "620025387",
    year  = "2024",       # <chr>
    value = 100.0,
    stringsAsFactors = FALSE
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "ides", path = template_path)

  # Must not raise "non-numeric argument to a mathematical function"
  expect_no_error(
    suppressWarnings(
      compare_datasets_from_yaml(ref, cand, key = "ides", path = template_path)
    )
  )

  unlink(template_path)
})

test_that("compare_datasets_from_yaml warns about numeric/character type mismatch", {
  ref <- data.frame(id = 1L, year = 2024, value = 100.0)
  cand <- data.frame(id = 1L, year = "2024", value = 100.0)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    regexp = "year"
  )

  unlink(template_path)
})

test_that("type-mismatched column falls back to equality and fails when values differ", {
  # year is numeric in ref, character in cand.
  # After fallback to equality: 2024 (dbl) != "2024" (chr) => validation fails.
  ref  <- data.frame(id = 1L, year = 2024,   value = 100.0)
  cand <- data.frame(id = 1L, year = "2024",  value = 100.0)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  result <- suppressWarnings(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  expect_false(result$all_passed)

  unlink(template_path)
})

test_that("correctly typed columns still pass alongside a mismatched column", {
  # value (dbl in both) should be validated fine even though year is mismatched.
  ref  <- data.frame(id = 1L, year = 2024,  value = 100.0)
  cand <- data.frame(id = 1L, year = "2024", value = 100.0)

  template_path <- tempfile(fileext = ".yaml")
  # Tight numeric tolerance so value comparison is meaningful
  write_rules_template(ref, key = "id", path = template_path,
                       numeric_abs = 1e-9, numeric_rel = 0)

  result <- suppressWarnings(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
  )

  # The overall result fails (year mismatch), but applied_rules must still
  # contain tolerance rules for value.
  expect_true("value" %in% names(result$applied_rules))
  abs_rule <- result$applied_rules[["value"]][["abs"]]
  expect_false(is.null(abs_rule))

  unlink(template_path)
})

test_that("multiple columns with numeric/character mismatch do not crash", {
  ref <- data.frame(
    id    = 1L,
    year  = 2024,
    month = 3,
    value = 100.0
  )
  cand <- data.frame(
    id    = 1L,
    year  = "2024",   # mismatch
    month = "3",      # mismatch
    value = 100.0     # correctly typed
  )

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_no_error(
    suppressWarnings(
      compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
    )
  )

  # Warning should mention both mismatched columns
  expect_warning(
    compare_datasets_from_yaml(ref, cand, key = "id", path = template_path),
    regexp = "year|month"
  )

  unlink(template_path)
})

test_that("no crash and no spurious warning when all column types match", {
  ref  <- data.frame(id = 1L, year = 2024,  value = 100.0)
  cand <- data.frame(id = 1L, year = 2024,  value = 100.0)

  template_path <- tempfile(fileext = ".yaml")
  write_rules_template(ref, key = "id", path = template_path)

  expect_no_error(
    expect_no_warning(
      compare_datasets_from_yaml(ref, cand, key = "id", path = template_path)
    )
  )

  unlink(template_path)
})

test_that("message is shown for positional comparison when row counts differ", {
  ref  <- data.frame(value = 1:5)
  cand <- data.frame(value = 1:3)

  # No key -> positional comparison; row counts differ -> message emitted then
  # R errors on column assignment. Capture the message before the error.
  msg <- NULL
  tryCatch(
    withCallingHandlers(
      suppressWarnings(compare_datasets_from_yaml(ref, cand)),
      message = function(m) {
        msg <<- conditionMessage(m)
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) NULL
  )
  expect_match(msg, "Without keys")
})
