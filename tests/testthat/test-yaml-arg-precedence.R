# Precedence between explicit arguments and YAML rules (issue #20):
# explicit argument > YAML defaults > built-in default.

test_that("explicit label argument wins over the YAML label", {
  ref <- data.frame(id = 1:2, x = c(1.0, 2.0))

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(ref, key = "id", label = "yaml label", path = yaml_path)

  res <- compare_datasets_from_yaml(
    ref, ref,
    key = "id", path = yaml_path, label = "explicit label"
  )
  expect_identical(res$reponse$label, "explicit label")

  # Without an explicit argument, the YAML label applies
  res_yaml <- compare_datasets_from_yaml(ref, ref, key = "id", path = yaml_path)
  expect_identical(res_yaml$reponse$label, "yaml label")
})

test_that("YAML 'keys' field is read without partial matching", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  # Shuffled candidate: only a comparison joined on the YAML key passes
  cand_shuffled <- ref[c(3, 1, 2), , drop = FALSE]

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults:
  keys: [id]
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.000000001
', con = yaml_path)

  warns <- character(0)
  withCallingHandlers(
    {
      res <- compare_datasets_from_yaml(ref, cand_shuffled, path = yaml_path)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(res$all_passed)
  expect_false(any(grepl("partial match", warns)))
})

test_that("YAML 'keys' field is honored under warnPartialMatchDollar", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand_shuffled <- ref[c(2, 3, 1), , drop = FALSE]

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults:
  keys: [id]
row_validation:
  check_count: no
', con = yaml_path)

  old <- options(warnPartialMatchDollar = TRUE)
  on.exit(options(old), add = TRUE)

  # The keys resolution itself must not rely on $ partial matching
  rules <- read_rules(yaml_path)
  expect_no_warning(rules$defaults[["keys"]] %||% rules$defaults[["key"]])

  res <- suppressWarnings(
    compare_datasets_from_yaml(ref, cand_shuffled, path = yaml_path)
  )
  expect_true(res$all_passed)
})

test_that("YAML with both 'keys' and legacy 'key' fields: 'keys' wins", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand_shuffled <- ref[c(3, 1, 2), , drop = FALSE]

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults:
  keys: [id]
  key: [nonexistent]
row_validation:
  check_count: no
', con = yaml_path)

  # If the legacy singular field won, the comparison would error on a
  # missing key column
  res <- compare_datasets_from_yaml(ref, cand_shuffled, path = yaml_path)
  expect_true(res$all_passed)
})

test_that("legacy singular 'key' YAML field is still honored", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand_shuffled <- ref[c(3, 1, 2), , drop = FALSE]

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults:
  key: [id]
row_validation:
  check_count: no
', con = yaml_path)

  res <- compare_datasets_from_yaml(ref, cand_shuffled, path = yaml_path)
  expect_true(res$all_passed)
})

test_that("key argument wins over the YAML keys field", {
  ref <- data.frame(id = 1:3, other = c(10L, 20L, 30L), value = c(1.0, 2.0, 3.0))
  cand_shuffled <- ref[c(3, 1, 2), , drop = FALSE]

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines('
version: 1
defaults:
  keys: [nonexistent]
row_validation:
  check_count: no
', con = yaml_path)

  # The explicit argument must shadow the (broken) YAML keys entirely
  res <- compare_datasets_from_yaml(ref, cand_shuffled, key = "id", path = yaml_path)
  expect_true(res$all_passed)
})
