test_that("write_rules_template creates valid YAML", {
  df <- data.frame(
    id = 1:3,
    amount = c(100.0, 200.0, 300.0),
    category = c("A", "B", "C")
  )

  template_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(template_path), add = TRUE)

  # Test that function runs without error
  expect_no_error(object = write_rules_template(df, key = "id", path = template_path))

  # Test that file was created
  expect_true(object = file.exists(template_path))

  # Test that we can read the rules back
  rules <- read_rules(template_path)
  expect_equal(object = rules$version, expected = 1)
  expect_true(object = is.list(rules$by_type))
  expect_true(object = is.list(rules$by_name))
  expect_equal(object = rules$defaults$keys, expected = "id")  # Check that key is stored
})

test_that("write_rules_template validates key parameter", {
  df <- data.frame(id = 1:3, value = c(1.1, 2.2, 3.3))

  # Test missing key parameter (now allowed - creates rules without key)
  temp_path <- tempfile(fileext = ".yaml")
  expect_no_error(write_rules_template(df, path = temp_path))
  rules <- read_rules(temp_path)
  expect_null(rules$defaults$keys)
  unlink(temp_path)

  # Test empty key
  expect_error(write_rules_template(df, key = character(0)), "must be a non-empty character vector")

  # Test non-character key
  expect_error(write_rules_template(df, key = 123), "must be a non-empty character vector")

  # Test key not in data
  expect_error(write_rules_template(df, key = "nonexistent"), "Key column\\(s\\) not found in data")
})

test_that("read_rules validates YAML structure", {
  valid_path <- tempfile(fileext = ".yaml")
  invalid_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(valid_path, invalid_path)), add = TRUE)

  # Valid rules
  valid_rules <- list(version = 1, defaults = list(), by_type = list(), by_name = list())
  yaml_content <- yaml::as.yaml(x = valid_rules)
  writeLines(text = yaml_content, con = valid_path)

  expect_no_error(object = read_rules(valid_path))

  # Invalid version
  invalid_rules <- list(version = 2, defaults = list(), by_type = list(), by_name = list())
  yaml_content <- yaml::as.yaml(x = invalid_rules)
  writeLines(text = yaml_content, con = invalid_path)

  expect_error(object = read_rules(invalid_path))
})
