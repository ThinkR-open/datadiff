test_that("write_rules_template supports all configuration parameters", {
  # Create test data
  df <- data.frame(
    id = 1:3,
    value = c(1.0, 2.0, 3.0),
    name = c("A", "B", "C"),
    count = c(10L, 20L, 30L)
  )

  # Generate template with all parameters configured
  template_path <- "test_full_config.yaml"
  write_rules_template(
    df,
    key = "id",
    label = "Test Label",
    path = template_path,
    ignore_columns_default = c("count"),
    check_count_default = TRUE,
    expected_count_default = 5,
    row_count_tolerance_default = 2,
    numeric_abs = 0.01,
    numeric_rel = 0.02,
    character_equal_mode = "normalized",
    character_case_insensitive = TRUE,
    character_trim = TRUE
  )

  # Read back the generated YAML
  rules <- read_rules(template_path)

  # Check defaults section
  expect_true(rules$defaults$na_equal)
  expect_equal(rules$defaults$ignore_columns, "count")
  expect_equal(rules$defaults$keys, "id")
  expect_equal(rules$defaults$label, "Test Label")

  # Check row_validation section
  expect_true(rules$row_validation$check_count)
  expect_equal(rules$row_validation$expected_count, 5)
  expect_equal(rules$row_validation$tolerance, 2)

  # Check by_type section
  expect_equal(rules$by_type$numeric$abs, 0.01)
  expect_equal(rules$by_type$numeric$rel, 0.02)
  expect_equal(rules$by_type$character$equal_mode, "normalized")
  expect_true(rules$by_type$character$case_insensitive)
  expect_true(rules$by_type$character$trim)

  # Clean up
  unlink(template_path)
})

test_that("write_rules_template defaults work correctly", {
  df <- data.frame(id = 1:2, value = c(1.0, 2.0))

  # Generate template with minimal parameters (using defaults)
  template_path <- "test_defaults.yaml"
  write_rules_template(df, key = "id", path = template_path)

  rules <- read_rules(template_path)

  # Check that defaults are applied
  expect_true(rules$defaults$na_equal)
  expect_equal(rules$defaults$ignore_columns, list())
  expect_equal(rules$defaults$keys, "id")

  expect_true(rules$row_validation$check_count)
  expect_null(rules$row_validation$expected_count)
  expect_equal(rules$row_validation$tolerance, 0)

  expect_equal(rules$by_type$numeric$abs, 0.000000001)
  expect_equal(rules$by_type$character$equal_mode, "exact")
  expect_false(rules$by_type$character$case_insensitive)

  # Clean up
  unlink(template_path)
})

test_that("write_rules_template ignore_columns parameter works", {
  df <- data.frame(id = 1:2, value = 1:2, temp = c("a", "b"))

  template_path <- "test_ignore.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = c("temp"), path = template_path)

  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, "temp")

  # Clean up
  unlink(template_path)
  # Clean up
  unlink(template_path)
})

test_that("write_rules_template ignore_columns_default parameter works", {
  df <- data.frame(id = 1:2, value = 1:2, temp = c("a", "b"), extra = c(10, 20))

  # Test with single column to ignore
  template_path <- "test_ignore_single.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = "temp", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, "temp")
  unlink(template_path)

  # Test with multiple columns to ignore
  template_path <- "test_ignore_multiple.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = c("temp", "extra"), path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, c("temp", "extra"))
  unlink(template_path)

  # Test with empty vector (default)
  template_path <- "test_ignore_empty.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = character(0), path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, list())
  unlink(template_path)
})

test_that("write_rules_template row validation parameters work", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test with check_count enabled and expected_count
  template_path <- "test_row_val_full.yaml"
  write_rules_template(df, key = "id",
                       check_count_default = TRUE,
                       expected_count_default = 100,
                       row_count_tolerance_default = 10,
                       path = template_path)
  rules <- read_rules(template_path)
  expect_true(rules$row_validation$check_count)
  expect_equal(rules$row_validation$expected_count, 100)
  expect_equal(rules$row_validation$tolerance, 10)
  unlink(template_path)

  # Test with check_count enabled but no expected_count (uses reference count)
  template_path <- "test_row_val_no_expected.yaml"
  write_rules_template(df, key = "id",
                       check_count_default = TRUE,
                       expected_count_default = NULL,
                       row_count_tolerance_default = 5,
                       path = template_path)
  rules <- read_rules(template_path)
  expect_true(rules$row_validation$check_count)
  expect_null(rules$row_validation$expected_count)
  expect_equal(rules$row_validation$tolerance, 5)
  unlink(template_path)

  # Test with check_count explicitly disabled
  template_path <- "test_row_val_disabled.yaml"
  write_rules_template(df, key = "id",
                       check_count_default = FALSE,
                       path = template_path)
  rules <- read_rules(template_path)
  expect_false(rules$row_validation$check_count)
  expect_null(rules$row_validation$expected_count)
  expect_equal(rules$row_validation$tolerance, 0)
  unlink(template_path)
})

test_that("write_rules_template parameter validation works", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test with invalid expected_count (negative)
  template_path <- "test_invalid_expected.yaml"
  write_rules_template(df, key = "id",
                       expected_count_default = -5,
                       path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$expected_count, -5)  # YAML allows it, validation happens later
  unlink(template_path)

  # Test with invalid tolerance (negative)
  template_path <- "test_invalid_tolerance.yaml"
  write_rules_template(df, key = "id",
                       row_count_tolerance_default = -1,
                       path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, -1)  # YAML allows it, validation happens later
  unlink(template_path)
})

test_that("write_rules_template integration with compare_datasets_from_yaml", {
  # Create test data
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.01, 2.01, 3.01))

  # Generate template with custom settings
  template_path <- "test_integration_template.yaml"
  write_rules_template(ref, key = "id",
                       ignore_columns_default = character(0),
                       check_count_default = TRUE,
                       expected_count_default = 3,
                       row_count_tolerance_default = 0,
                       numeric_abs = 0.1,
                       path = template_path)

  # Compare should succeed
  result <- compare_datasets_from_yaml(ref, cand, path = template_path)
  expect_true(result$all_passed)

  # Test with wrong row count
  cand_wrong_rows <- data.frame(id = 1:4, value = c(1.01, 2.01, 3.01, 4.01))
  result_wrong <- compare_datasets_from_yaml(ref, cand_wrong_rows, path = template_path)
  expect_false(result_wrong$all_passed)

  # Clean up
  unlink(template_path)
})
