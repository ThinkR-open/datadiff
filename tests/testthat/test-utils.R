test_that("%||% operator works correctly", {
  expect_equal(object = NULL %||% "default", expected = "default")
  expect_equal(object = "value" %||% "default", expected = "value")
  expect_equal(object = 0 %||% 42, expected = 0)
  expect_equal(object = NULL %||% NULL, expected = NULL)
})

test_that("%||% is internal (not exported), per NEWS 0.4.4", {
  # Exporting it masks rlang's and base R's (R >= 4.4) own %||% at load time
  expect_false("%||%" %in% getNamespaceExports("datadiff"))
})

test_that("normalize_text handles basic cases", {
  # Test case insensitive
  expect_equal(object = normalize_text(c("HELLO", "world"), case_insensitive = TRUE), expected = c("hello", "world"))
  expect_equal(object = normalize_text(c("HELLO", "world"), case_insensitive = FALSE), expected = c("HELLO", "world"))

  # Test trim
  expect_equal(object = normalize_text(c("  hello  ", "  world  "), trim = TRUE), expected = c("hello", "world"))
  expect_equal(object = normalize_text(c("  hello  ", "  world  "), trim = FALSE), expected = c("  hello  ", "  world  "))

  # Test both
  expect_equal(object = normalize_text(c("  HELLO  ", "  WORLD  "), case_insensitive = TRUE, trim = TRUE), expected = c("hello", "world"))

  # Test non-character input
  expect_equal(object = normalize_text(123), expected = 123)
  expect_equal(object = normalize_text(c(1, 2, 3)), expected = c(1, 2, 3))
})

test_that("preprocess_dataframe applies transformations correctly", {
  df <- data.frame(
    text_col = c("  HELLO  ", "WORLD"),
    num_col = c(1, 2),
    case_col = c("MiXeD", "CaSe")
  )

  # Test with case_insensitive and trim
  rules <- list(
    text_col = list(case_insensitive = TRUE, trim = TRUE),
    num_col = list(),  # No transformation for numeric
    case_col = list(case_insensitive = TRUE)
  )

  result <- preprocess_dataframe(df, rules)

  expect_equal(result$text_col, c("hello", "world"))
  expect_equal(result$num_col, c(1, 2))  # Unchanged
  expect_equal(result$case_col, c("mixed", "case"))
})

test_that("equal_mode normalized implies case_insensitive and trim by default", {
  df <- data.frame(text_col = c("  HELLO  ", "WORLD"))

  rules <- list(text_col = list(equal_mode = "normalized"))
  result <- preprocess_dataframe(df, rules)
  expect_equal(result$text_col, c("hello", "world"))

  # An explicit flag overrides the implied default
  rules_no_trim <- list(text_col = list(equal_mode = "normalized", trim = FALSE))
  result_no_trim <- preprocess_dataframe(df, rules_no_trim)
  expect_equal(result_no_trim$text_col, c("  hello  ", "world"))

  rules_no_case <- list(text_col = list(equal_mode = "normalized",
                                        case_insensitive = FALSE))
  result_no_case <- preprocess_dataframe(df, rules_no_case)
  expect_equal(result_no_case$text_col, c("HELLO", "WORLD"))
})

test_that("equal_mode normalized alone changes the verdict (issue reprex)", {
  ref  <- data.frame(id = 1:2, s = c("A", " b"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, s = c("a", "b"), stringsAsFactors = FALSE)

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults: {keys: [id], na_equal: true}
row_validation: {check_count: false}
by_type:
  character: {equal_mode: normalized}
', con = yaml_path)

  res <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id", path = yaml_path)
  )
  expect_true(res$all_passed)
})

test_that("preprocess_dataframe handles mixed rules", {
  df <- data.frame(
    col1 = c("  TRIM  ", "no_trim"),
    col2 = c("UPPER", "lower"),
    col3 = c("  BOTH  ", "CaSe")
  )

  rules <- list(
    col1 = list(trim = TRUE),
    col2 = list(case_insensitive = TRUE),
    col3 = list(trim = TRUE, case_insensitive = TRUE)
  )

  result <- preprocess_dataframe(df, rules)

  expect_equal(result$col1, c("TRIM", "no_trim"))
  expect_equal(result$col2, c("upper", "lower"))
  expect_equal(result$col3, c("both", "case"))
})

test_that("preprocess_dataframe handles empty rules", {
  df <- data.frame(text_col = c("  TEST  ", "DATA"))

  rules <- list(text_col = list())

  result <- preprocess_dataframe(df, rules)

  # Should not apply any transformation
  expect_equal(result$text_col, c("  TEST  ", "DATA"))
})

test_that("get_col_names falls back to names() when colnames() returns empty", {
  # A plain named list has names() but colnames() returns NULL (length 0)
  x <- list(col_a = 1:3, col_b = letters[1:3])
  expect_equal(get_col_names(x), c("col_a", "col_b"))
})

test_that("preprocess_dataframe applies stringr trim on Arrow objects", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("stringr")

  arrow_tbl <- arrow::as_arrow_table(data.frame(
    name = c("  Alice  ", "  Bob  "),
    stringsAsFactors = FALSE
  ))

  # schema is required: Arrow columns are ChunkedArrays, not character vectors,
  # so is.character(arrow_tbl[["name"]]) returns FALSE without it.
  schema <- data.frame(name = character(0))
  rules <- list(name = list(trim = TRUE))
  result <- preprocess_dataframe(arrow_tbl, rules, schema = schema) %>% dplyr::collect()

  expect_equal(result$name, c("Alice", "Bob"))
})

test_that("a normalized template does not neutralise its own mode", {
  ref <- data.frame(id = 1:2, s = c("A", " b"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, s = c("a", "b"), stringsAsFactors = FALSE)

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(ref, key = "id", path = yaml_path,
                       character_equal_mode = "normalized")

  rules <- read_rules(yaml_path)
  # The default FALSE flags must not be co-written next to the mode
  expect_null(rules$by_type$character$case_insensitive)
  expect_null(rules$by_type$character$trim)

  res <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id", path = yaml_path)
  )
  expect_true(res$all_passed)

  # An explicitly supplied flag still wins over the mode
  yaml_path2 <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path2), add = TRUE)
  write_rules_template(ref, key = "id", path = yaml_path2,
                       character_equal_mode = "normalized",
                       character_trim = FALSE)
  rules2 <- read_rules(yaml_path2)
  expect_false(rules2$by_type$character$trim)
})
