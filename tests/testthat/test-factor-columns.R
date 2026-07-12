# Factor columns are classed "character" by detect_column_types() and receive
# the character rules, so they must be compared as the character values they
# display: preprocessing converts them, making the verdict independent of
# stringsAsFactors / haven-style imports.

test_that("factor and character candidates get the same verdict under normalization", {
  ref      <- data.frame(id = 1:2, s = c("alpha", "beta"), stringsAsFactors = FALSE)
  cand_chr <- data.frame(id = 1:2, s = c("ALPHA", " beta"), stringsAsFactors = FALSE)
  cand_fct <- data.frame(id = 1:2, s = factor(c("ALPHA", " beta")))

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(
    ref,
    key = "id", path = yaml_path,
    character_case_insensitive = TRUE, character_trim = TRUE
  )

  res_chr <- compare_datasets_from_yaml(ref, cand_chr, key = "id", path = yaml_path)
  res_fct <- compare_datasets_from_yaml(ref, cand_fct, key = "id", path = yaml_path)
  expect_true(res_chr$all_passed)
  expect_true(res_fct$all_passed)
})

test_that("factor columns are normalized on the reference side and both sides", {
  ref_fct  <- data.frame(id = 1:2, s = factor(c("ALPHA", " beta")))
  cand_chr <- data.frame(id = 1:2, s = c("alpha", "beta"), stringsAsFactors = FALSE)
  cand_fct <- data.frame(id = 1:2, s = factor(c("alpha", "beta")))

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(
    data.frame(id = 1:2, s = c("x", "y"), stringsAsFactors = FALSE),
    key = "id", path = yaml_path,
    character_case_insensitive = TRUE, character_trim = TRUE
  )

  res_ref_side <- compare_datasets_from_yaml(ref_fct, cand_chr, key = "id", path = yaml_path)
  expect_true(res_ref_side$all_passed)

  res_both <- compare_datasets_from_yaml(ref_fct, cand_fct, key = "id", path = yaml_path)
  expect_true(res_both$all_passed)
})

test_that("factors with differing level sets compare cleanly", {
  # == on two factors with different level sets errors in base R; after the
  # character conversion the comparison must work and give the right verdict
  ref  <- data.frame(id = 1:3, s = factor(c("a", "b", "c"), levels = c("a", "b", "c")))
  cand <- data.frame(id = 1:3, s = factor(c("a", "b", "d"), levels = c("a", "b", "d")))

  res <- compare_datasets_from_yaml(ref, cand, key = "id")
  expect_false(res$all_passed)
  cov <- res$coverage
  expect_identical(
    as.integer(cov$n_failed[cov$column == "s" & cov$check == "equality"]),
    1L
  )

  # Identical displayed values with different (unused) levels pass
  cand_same <- data.frame(id = 1:3, s = factor(c("a", "b", "c"), levels = c("c", "b", "a", "z")))
  res_same <- compare_datasets_from_yaml(ref, cand_same, key = "id")
  expect_true(res_same$all_passed)
})

test_that("a factor key column joins correctly", {
  ref  <- data.frame(id = factor(c("k1", "k2", "k3")), x = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = c("k3", "k1", "k2"), x = c(3.0, 1.0, 2.0),
                     stringsAsFactors = FALSE)

  res <- compare_datasets_from_yaml(ref, cand, key = "id")
  expect_true(res$all_passed)
})

test_that("Arrow dictionary (factor) columns are normalized on the lazy path", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("stringr")

  # Arrow carries factors as dictionary columns: the raw 0-row schema still
  # answers is.factor() TRUE, which must route them through the lazy
  # normalization path
  ref  <- data.frame(id = 1:2, s = c("alpha", "beta"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, s = factor(c("ALPHA", " beta")))

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(
    ref,
    key = "id", path = yaml_path,
    character_case_insensitive = TRUE, character_trim = TRUE
  )

  res <- suppressMessages(compare_datasets_from_yaml(
    arrow::arrow_table(ref), arrow::arrow_table(cand),
    key = "id", path = yaml_path
  ))
  expect_true(res$all_passed)
})
