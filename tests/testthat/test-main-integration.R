test_that("compare_datasets_from_yaml handles ignore_columns", {
  ref <- data.frame(a = 1, b = 2, c = 3, to_ignore = 99)
  cand <- data.frame(a = 1, b = 2, c = 3, another_ignore = 100)

  # Create YAML with ignore_columns
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  ignore_columns: ["to_ignore", "another_ignore"]
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  # Test the full integration
  result <- compare_datasets_from_yaml(ref, cand, path = yaml_path)

  # Should not report missing/extra columns
  expect_equal(length(result$missing_in_candidate), 0)

  expect_equal(length(result$extra_in_candidate), 0)
})

test_that("compare_datasets_from_yaml works with keys", {
  ref <- data.frame(id = c(1, 2, 3), value = c(10, 20, 30))
  cand <- data.frame(id = c(1, 2, 3), value = c(10.1, 20.1, 30.1))

  # Create simple rules
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
by_type:
  numeric:
    abs: 0.5
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  result <- compare_datasets_from_yaml(ref, cand, key = "id", path = yaml_path)

  # Should have a valid result
  expect_type(result, "list")
  expect_s3_class(result$agent, "ptblank_agent")
})

test_that("compare_datasets_from_yaml handles row validation", {
  ref <- data.frame(a = 1:3, b = 2:4)
  cand <- data.frame(a = 1:3, b = 2:4)  # Same number of rows

  yaml_content <- '
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: yes
  expected_count: 3
by_type:
  numeric:
    abs: 0.01
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  # Should not error with matching row counts
  expect_no_error(compare_datasets_from_yaml(ref, cand, path = yaml_path))
})

test_that("column-specific rules override type rules for numeric and character columns", {
  # Create reference data
  ref <- data.frame(
    id = 1:2,
    num_type_only = c(1.0, 2.0),
    num_specific = c(1.0, 2.0),
    char_type_only = c("Hello", "World"),
    char_specific = c("Hello", "World")
  )

  # Create candidate data with differences that would fail type rules but pass specific rules
  cand <- data.frame(
    id = 1:2,
    num_type_only = c(1.005, 2.005),  # diff 0.005 < 0.01 (type abs), should pass
    num_specific = c(1.06, 2.06),     # diff 0.06 > 0.01 but < 0.1 (specific abs), should pass
    char_type_only = c("Hello", "World"),  # exact match, should pass
    char_specific = c("hello", "world")     # case differs, should pass with case_insensitive=true
  )

  # YAML with type rules and overriding column-specific rules
  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
by_type:
  numeric:
    abs: 0.01
  character:
    equal_mode: exact
    case_insensitive: false
    trim: false
by_name:
  num_specific:
    abs: 0.1
  char_specific:
    case_insensitive: true
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  result <- compare_datasets_from_yaml(ref, cand, path = yaml_path)

  # Check that applied_rules reflect the priority: column-specific override type rules
  applied <- result$applied_rules

  # num_type_only should have type rule (abs = 0.01)
  expect_equal(applied$num_type_only$abs, 0.01)

  # num_specific should have overridden rule (abs = 0.1)
  expect_equal(applied$num_specific$abs, 0.1)

  # char_type_only should have type rules
  expect_false(applied$char_type_only$case_insensitive)
  expect_equal(applied$char_type_only$equal_mode, "exact")

  # char_specific should have overridden rule
  expect_true(applied$char_specific$case_insensitive)

  # The comparison should pass since differences are within the applied rules
  expect_true(result$all_passed)
})



test_that("compare_datasets_from_yaml handles different row counts with keys", {
  # Reference has 3 rows, candidate has 4 rows with different keys
  ref <- data.frame(id = 1:3, value = c(10, 20, 30))
  cand <- data.frame(id = 1:4, value = c(10.1, 20.1, 30.1, 40.1))  # Extra row with id=4

  yaml_content <- '
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: false  # Row count validation disabled
by_type:
  numeric:
    abs: 0.5
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  # Should not error even with different row counts when keys are provided
  expect_no_error(result <- compare_datasets_from_yaml(ref, cand, key = "id", path = yaml_path))

  # Should have a valid result structure
  expect_type(result, "list")
  expect_s3_class(result$agent, "ptblank_agent")
  expect_false(result$all_passed)  # Should fail because of extra row in candidate
})

test_that("compare_datasets_from_yaml uses English language by default", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))

  result <- compare_datasets_from_yaml(ref, cand, key = "id")

  # Agent should have English as default language
  expect_equal(result$agent$lang, "en")
  expect_equal(result$agent$locale, "en_US")
})

test_that("compare_datasets_from_yaml accepts custom language parameters", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))

  # Test English
  result_en <- compare_datasets_from_yaml(ref, cand, key = "id", lang = "en", locale = "en_US")
  expect_equal(result_en$agent$lang, "en")
  expect_equal(result_en$agent$locale, "en_US")

  # Test German
  result_de <- compare_datasets_from_yaml(ref, cand, key = "id", lang = "de", locale = "de_DE")
  expect_equal(result_de$agent$lang, "de")
  expect_equal(result_de$agent$locale, "de_DE")

  # Test Spanish
  result_es <- compare_datasets_from_yaml(ref, cand, key = "id", lang = "es", locale = "es_ES")
  expect_equal(result_es$agent$lang, "es")
  expect_equal(result_es$agent$locale, "es_ES")
})

test_that("compare_datasets_from_yaml propagates language to agent with YAML rules", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.1, 2.1, 3.1))

  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
by_type:
  numeric:
    abs: 0.5
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  # Test with English language
  result <- compare_datasets_from_yaml(ref, cand, path = yaml_path, lang = "en", locale = "en_GB")

  expect_equal(result$agent$lang, "en")
  expect_equal(result$agent$locale, "en_GB")
  expect_true(result$all_passed)
})

test_that("compare_datasets_from_yaml accepts extract_failed parameter", {
  ref <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))

  # Test with extract_failed = TRUE (default)
  result_extract <- compare_datasets_from_yaml(ref, cand, key = "id", extract_failed = TRUE)
  expect_type(result_extract, "list")
  expect_true(result_extract$all_passed)

  # Test with extract_failed = FALSE (lightweight mode)
  result_no_extract <- compare_datasets_from_yaml(ref, cand, key = "id", extract_failed = FALSE)
  expect_type(result_no_extract, "list")
  expect_true(result_no_extract$all_passed)
})

test_that("compare_datasets_from_yaml accepts get_first_n parameter", {
  # Create data with many failures
  ref <- data.frame(id = 1:100, value = rep(1.0, 100))
  cand <- data.frame(id = 1:100, value = rep(2.0, 100))  # All values differ

  yaml_content <- '
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
by_type:
  numeric:
    abs: 0.001
'

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(yaml_content, yaml_path)

  # Test with get_first_n = 5 (limit extracted failures)
  result <- compare_datasets_from_yaml(ref, cand, path = yaml_path, get_first_n = 5)

  expect_type(result, "list")
  expect_false(result$all_passed)  # Should fail because values differ
})

test_that("compare_datasets_from_yaml accepts sample_n parameter", {
  ref <- data.frame(id = 1:50, value = rep(1.0, 50))
  cand <- data.frame(id = 1:50, value = rep(2.0, 50))

  # Test with sample_n = 10
  result <- compare_datasets_from_yaml(ref, cand, key = "id", sample_n = 10)

  expect_type(result, "list")
  expect_false(result$all_passed)
})

test_that("compare_datasets_from_yaml accepts sample_frac and sample_limit parameters", {
  ref <- data.frame(id = 1:100, value = rep(1.0, 100))
  cand <- data.frame(id = 1:100, value = rep(2.0, 100))

  # Test with sample_frac = 0.1 and sample_limit = 5
  result <- compare_datasets_from_yaml(
    ref, cand, key = "id",
    sample_frac = 0.1,
    sample_limit = 5
  )

  expect_type(result, "list")
  expect_false(result$all_passed)
})

test_that("compare_datasets_from_yaml extraction parameters work together", {
  ref <- data.frame(id = 1:10, value = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0))
  cand <- data.frame(id = 1:10, value = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0))

  # Test combining extract_failed = FALSE with other parameters (should work)
  result <- compare_datasets_from_yaml(
    ref, cand, key = "id",
    extract_failed = FALSE,
    sample_limit = 100
  )

  expect_type(result, "list")
  expect_true(result$all_passed)
})
