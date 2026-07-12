test_that("write_rules_template creates valid YAML", {
  df <- data.frame(
    id = 1:3,
    amount = c(100.0, 200.0, 300.0),
    category = c("A", "B", "C")
  )
  temp_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_path), add = TRUE)

  # Test that function runs without error
  expect_no_error(object = write_rules_template(df, key = "id", path = temp_path))

  # Test that file was created
  expect_true(object = file.exists(temp_path))

  # Test that we can read the rules back
  rules <- read_rules(temp_path)
  expect_equal(object = rules$version, expected = 1)
  expect_true(object = is.list(rules$by_type))
  expect_true(object = is.list(rules$by_name))
  expect_equal(object = rules$defaults$keys, expected = "id")  # Check that key is stored
})

test_that("write_rules_template validates key parameter", {
  df <- data.frame(id = 1:3, value = c(1.1, 2.2, 3.3))

  # Test missing key parameter (now allowed - creates rules without key)
  temp_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(temp_path), add = TRUE)
  expect_no_error(write_rules_template(df, path = temp_path))
  rules <- read_rules(temp_path)
  expect_null(rules$defaults$keys)

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

test_that("read_rules gives an actionable error for unsupported versions", {
  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines('version: 2\ndefaults: {na_equal: yes}', con = p)
  err <- tryCatch(read_rules(p), error = function(e) {
    conditionMessage(e)
  })
  expect_match(err, "version", ignore.case = TRUE)
  expect_match(err, "2", fixed = TRUE)          # the offending version
  expect_match(err, "1", fixed = TRUE)          # the supported version
  expect_no_match(err, "is not TRUE", fixed = TRUE)  # no raw stopifnot output
})

test_that("read_rules warns on unknown top-level and defaults fields", {
  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines('
version: 1
defaults: {keys: [id], na_equal: yes, no_equal: yes}
by_nmae:
  numeric: {abs: 0.1}
', con = p)
  warns <- character(0)
  withCallingHandlers(
    rules <- read_rules(p),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("by_nmae", warns)))     # typo of by_name flagged
  expect_true(any(grepl("no_equal", warns)))    # typo in defaults flagged
})

test_that("write_rules_template rejects an unsupported version upfront", {
  df <- data.frame(id = 1:2, x = c(1, 2))
  expect_error(
    write_rules_template(df, key = "id", path = tempfile(fileext = ".yaml"),
                         version = 2),
    regexp = "version"
  )
})

test_that("read_rules stays actionable on non-scalar versions", {
  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines('version: {v: 1}\ndefaults: {na_equal: yes}', con = p)
  err <- tryCatch(read_rules(p), error = function(e) {
    conditionMessage(e)
  })
  expect_match(err, "version", ignore.case = TRUE)
  expect_no_match(err, "invalid format", fixed = TRUE)  # no raw sprintf crash
})

test_that("read_rules rejects non-mapping sections explicitly", {
  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)
  writeLines('version: 1\ndefaults: yes', con = p)
  err <- tryCatch(read_rules(p), error = function(e) {
    conditionMessage(e)
  })
  expect_match(err, "defaults")
  expect_match(err, "mapping", ignore.case = TRUE)
})
