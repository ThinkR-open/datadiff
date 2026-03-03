test_that("write_rules_template handles invalid key parameter", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test missing key (now allowed - creates rules without key)
  temp_path <- tempfile(fileext = ".yaml")
  expect_no_error(write_rules_template(df, path = temp_path))
  rules <- read_rules(temp_path)
  expect_null(rules$defaults$keys)
  unlink(temp_path)

  # Test empty key vector (should error)
  expect_error(write_rules_template(df, key = character(0), path = "test.yaml"))

  # Test non-character key (should error)
  expect_error(write_rules_template(df, key = 123, path = "test.yaml"))

  # Test non-existent column as key (should error)
  expect_error(write_rules_template(df, key = "nonexistent", path = "test.yaml"))
})

test_that("write_rules_template handles invalid ignore_columns_default", {
  df <- data.frame(id = 1:2, value = 1:2, temp = c("a", "b"))

  # Test NULL (should work, treated as empty - YAML may not write empty lists)
  template_path <- "test_ignore_null.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = NULL, path = template_path)
  rules <- read_rules(template_path)
  # When ignore_columns is empty list, YAML behavior may vary
  expect_true(is.null(rules$defaults$ignore_columns) || identical(rules$defaults$ignore_columns, list()))
  unlink(template_path)

  # Test with non-existent column (YAML accepts it, validation happens later)
  template_path <- "test_ignore_invalid.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = "nonexistent", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, "nonexistent")
  unlink(template_path)

  # Test with mixed valid/invalid columns
  template_path <- "test_ignore_mixed.yaml"
  write_rules_template(df, key = "id", ignore_columns_default = c("value", "invalid"), path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, c("value", "invalid"))
  unlink(template_path)
})

test_that("write_rules_template handles invalid row validation parameters", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test non-logical check_count (should work, YAML coerces)
  template_path <- "test_check_count_invalid.yaml"
  write_rules_template(df, key = "id", check_count_default = "true", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$check_count, "true")  # YAML stores as string
  unlink(template_path)

  # Test non-numeric expected_count
  template_path <- "test_expected_count_invalid.yaml"
  write_rules_template(df, key = "id", expected_count_default = "invalid", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$expected_count, "invalid")
  unlink(template_path)

  # Test non-numeric tolerance
  template_path <- "test_tolerance_invalid.yaml"
  write_rules_template(df, key = "id", row_count_tolerance_default = "not_a_number", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, "not_a_number")
  unlink(template_path)
})

test_that("write_rules_template handles extreme values", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test very large expected_count
  template_path <- "test_large_expected.yaml"
  write_rules_template(df, key = "id", expected_count_default = 1e10, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$expected_count, 1e10)
  unlink(template_path)

  # Test very small tolerance
  template_path <- "test_small_tolerance.yaml"
  write_rules_template(df, key = "id", row_count_tolerance_default = 1e-10, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, 1e-10)
  unlink(template_path)

  # Test zero tolerance
  template_path <- "test_zero_tolerance.yaml"
  write_rules_template(df, key = "id", row_count_tolerance_default = 0, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, 0)
  unlink(template_path)
})

test_that("write_rules_template handles invalid data types", {
  # Test with non-dataframe input (should error)
  expect_error(write_rules_template("not_a_dataframe", key = "id", path = "test.yaml"))

  # Test with empty dataframe (should error on key)
  empty_df <- data.frame()
  expect_error(write_rules_template(empty_df, key = "id", path = "test.yaml"))

  # Test with dataframe having no rows (should work - creates template with empty by_name)
  no_rows_df <- data.frame(id = integer(0), value = numeric(0))
  expect_no_error(write_rules_template(no_rows_df, key = "id", path = "test_no_rows.yaml"))
  if (file.exists("test_no_rows.yaml")) unlink("test_no_rows.yaml")
})

test_that("write_rules_template handles file path issues", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test with empty path (should work, creates file in current dir)
  suppressWarnings(expect_no_error(write_rules_template(df, key = "id", path = "")))
  if (file.exists("")) unlink("")

  # Test with directory path (should error or overwrite)
  temp_dir <- tempdir()
  dir_path <- file.path(temp_dir, "test_dir")
  dir.create(dir_path, showWarnings = FALSE)
  suppressWarnings(
  expect_error(write_rules_template(df, key = "id", path = dir_path))
)
  # Test with invalid characters in path (OS dependent)
  # This might work on some systems but fail on others
  invalid_path <- "test<file>invalid.yaml"
  tryCatch({
    suppressWarnings(write_rules_template(df, key = "id", path = invalid_path))
    if (file.exists(invalid_path)) unlink(invalid_path)
  }, error = function(e) {
    # Expected to potentially error
    expect_true(TRUE)
  })
})

test_that("write_rules_template handles label edge cases", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test NULL label (should use default)
  template_path <- "test_null_label.yaml"
  write_rules_template(df, key = "id", label = NULL, path = template_path)
  rules <- read_rules(template_path)
  expect_match(rules$defaults$label, "comparaison")
  unlink(template_path)

  # Test empty label (should use default)
  template_path <- "test_empty_label.yaml"
  write_rules_template(df, key = "id", label = "", path = template_path)
  rules <- read_rules(template_path)
  expect_match(rules$defaults$label, "comparaison")
  unlink(template_path)

  # Test very long label
  long_label <- paste(rep("very_long_label", 100), collapse = "_")
  template_path <- "test_long_label.yaml"
  write_rules_template(df, key = "id", label = long_label, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$label, long_label)
  unlink(template_path)
})

test_that("write_rules_template handles numeric parameter edge cases", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test negative numeric_abs
  template_path <- "test_negative_abs.yaml"
  write_rules_template(df, key = "id", numeric_abs = -0.01, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$abs, -0.01)
  unlink(template_path)

  # Test zero numeric_abs
  template_path <- "test_zero_abs.yaml"
  write_rules_template(df, key = "id", numeric_abs = 0, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$abs, 0)
  unlink(template_path)

  # Test very small numeric_rel
  template_path <- "test_small_rel.yaml"
  write_rules_template(df, key = "id", numeric_rel = 1e-20, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$rel, 1e-20)
  unlink(template_path)
})

test_that("write_rules_template handles character parameter edge cases", {
  df <- data.frame(id = 1:2, value = 1:2)

  # Test empty string for equal_mode
  template_path <- "test_empty_mode.yaml"
  write_rules_template(df, key = "id", character_equal_mode = "", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$equal_mode, "")
  unlink(template_path)

  # Test invalid equal_mode
  template_path <- "test_invalid_mode.yaml"
  write_rules_template(df, key = "id", character_equal_mode = "invalid_mode", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$equal_mode, "invalid_mode")
  unlink(template_path)

  # Test non-logical case_insensitive (coerced to logical-like)
  template_path <- "test_nonlogical_case.yaml"
  write_rules_template(df, key = "id", character_case_insensitive = "TRUE", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$case_insensitive, "TRUE")
  unlink(template_path)
})
