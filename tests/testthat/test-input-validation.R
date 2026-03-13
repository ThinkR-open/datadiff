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

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test NULL (should work, treated as empty - YAML may not write empty lists)
  write_rules_template(df, key = "id", ignore_columns_default = NULL, path = template_path)
  rules <- read_rules(template_path)
  # When ignore_columns is empty list, YAML behavior may vary
  expect_true(is.null(rules$defaults$ignore_columns) || identical(rules$defaults$ignore_columns, list()))

  # Test with non-existent column (YAML accepts it, validation happens later)
  write_rules_template(df, key = "id", ignore_columns_default = "nonexistent", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, "nonexistent")

  # Test with mixed valid/invalid columns
  write_rules_template(df, key = "id", ignore_columns_default = c("value", "invalid"), path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$ignore_columns, c("value", "invalid"))
})

test_that("write_rules_template handles invalid row validation parameters", {
  df <- data.frame(id = 1:2, value = 1:2)

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test non-logical check_count (should work, YAML coerces)
  write_rules_template(df, key = "id", check_count_default = "true", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$check_count, "true")  # YAML stores as string

  # Test non-numeric expected_count
  write_rules_template(df, key = "id", expected_count_default = "invalid", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$expected_count, "invalid")

  # Test non-numeric tolerance
  write_rules_template(df, key = "id", row_count_tolerance_default = "not_a_number", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, "not_a_number")
})

test_that("write_rules_template handles extreme values", {
  df <- data.frame(id = 1:2, value = 1:2)

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test very large expected_count
  write_rules_template(df, key = "id", expected_count_default = 1e10, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$expected_count, 1e10)

  # Test very small tolerance
  write_rules_template(df, key = "id", row_count_tolerance_default = 1e-10, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, 1e-10)

  # Test zero tolerance
  write_rules_template(df, key = "id", row_count_tolerance_default = 0, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$row_validation$tolerance, 0)
})

test_that("write_rules_template handles invalid data types", {
  # Test with non-dataframe input (should error)
  expect_error(write_rules_template("not_a_dataframe", key = "id", path = "test.yaml"))

  # Test with empty dataframe (should error on key)
  empty_df <- data.frame()
  expect_error(write_rules_template(empty_df, key = "id", path = "test.yaml"))

  # Test with dataframe having no rows (should work - creates template with empty by_name)
  no_rows_df <- data.frame(id = integer(0), value = numeric(0))
  tmp_no_rows <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp_no_rows), add = TRUE)
  expect_no_error(write_rules_template(no_rows_df, key = "id", path = tmp_no_rows))
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

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test NULL label (should use default)
  write_rules_template(df, key = "id", label = NULL, path = template_path)
  rules <- read_rules(template_path)
  expect_match(rules$defaults$label, "comparison")

  # Test empty label (should use default)
  write_rules_template(df, key = "id", label = "", path = template_path)
  rules <- read_rules(template_path)
  expect_match(rules$defaults$label, "comparison")

  # Test very long label
  long_label <- paste(rep("very_long_label", 100), collapse = "_")
  write_rules_template(df, key = "id", label = long_label, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$defaults$label, long_label)
})

test_that("write_rules_template handles numeric parameter edge cases", {
  df <- data.frame(id = 1:2, value = 1:2)

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test negative numeric_abs
  write_rules_template(df, key = "id", numeric_abs = -0.01, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$abs, -0.01)

  # Test zero numeric_abs
  write_rules_template(df, key = "id", numeric_abs = 0, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$abs, 0)

  # Test very small numeric_rel
  write_rules_template(df, key = "id", numeric_rel = 1e-20, path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$numeric$rel, 1e-20)
})

test_that("write_rules_template handles character parameter edge cases", {
  df <- data.frame(id = 1:2, value = 1:2)

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test empty string for equal_mode
  write_rules_template(df, key = "id", character_equal_mode = "", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$equal_mode, "")

  # Test invalid equal_mode
  write_rules_template(df, key = "id", character_equal_mode = "invalid_mode", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$equal_mode, "invalid_mode")

  # Test non-logical case_insensitive (coerced to logical-like)
  write_rules_template(df, key = "id", character_case_insensitive = "TRUE", path = template_path)
  rules <- read_rules(template_path)
  expect_equal(rules$by_type$character$case_insensitive, "TRUE")
})
