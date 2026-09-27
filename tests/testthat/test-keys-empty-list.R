# In a rules file, `keys: []` means "no key", exactly like an absent field or
# `keys: ~`: yaml::read_yaml() reads [] as list(), which is not NULL, and must
# not be mistaken for an empty key. The `key` argument, R code, stays strict.

# legacy = TRUE adds a singular `key: [f2]` field, which a present `keys`
# field must beat whatever its value: without it, the no-key spellings could
# not be told apart from one another.
write_rules_with <- function(key_line, legacy = FALSE) {
  path <- tempfile(fileext = ".yaml")
  writeLines(c(
    "version: 1",
    "defaults:",
    key_line,
    if (legacy) "  key: [f2]",
    "  label: essai",
    "by_type:",
    "  character:",
    "    equal_mode: exact"
  ), con = path)
  path
}

tbl <- data.frame(f2 = "OUI", f3 = "NON", stringsAsFactors = FALSE)

test_that("keys: [] in the rules is read as no key: identical tables pass", {
  path <- write_rules_with("  keys: []")
  on.exit(unlink(path), add = TRUE)
  expect_no_error(res <- compare_datasets_from_yaml(tbl, tbl, path = path))
  expect_true(res$all_passed)
})

test_that("keys: [] behaves like keys: ~ and like an absent keys field", {
  paths <- c(write_rules_with("  keys: []"), write_rules_with("  keys: ~"),
             write_rules_with("  na_equal: yes"))
  on.exit(unlink(paths), add = TRUE)
  cand <- data.frame(f2 = "OUI", f3 = "non", stringsAsFactors = FALSE)
  covs <- lapply(paths, FUN = function(p) compare_datasets_from_yaml(tbl, cand, path = p)$coverage)
  expect_false(all(covs[[1]]$n_failed == 0))
  expect_identical(covs[[1]], covs[[2]])
  expect_identical(covs[[1]], covs[[3]])
})

test_that("keys: [] is positional: every column is compared, unequal counts error", {
  path <- write_rules_with("  keys: []")
  on.exit(unlink(path), add = TRUE)
  res <- compare_datasets_from_yaml(tbl, tbl, path = path)
  expect_setequal(res$coverage$column[res$coverage$check == "equality"], c("f2", "f3"))
  expect_error(
    compare_datasets_from_yaml(tbl, rbind(tbl, tbl), path = path),
    regexp = "same number of rows"
  )
})

test_that("legacy singular key: [] is read as no key as well", {
  path <- write_rules_with("  key: []")
  on.exit(unlink(path), add = TRUE)
  expect_true(compare_datasets_from_yaml(tbl, tbl, path = path)$all_passed)
})

test_that("a present keys field wins over a legacy key field, whatever its no-key spelling", {
  paths <- c(write_rules_with("  keys: []", legacy = TRUE), write_rules_with("  keys: ~", legacy = TRUE))
  on.exit(unlink(paths), add = TRUE)
  covs <- lapply(paths, FUN = function(p) compare_datasets_from_yaml(tbl, tbl, path = p)$coverage)
  for (cov in covs) {
    expect_setequal(cov$column[cov$check == "equality"], c("f2", "f3"))
  }
  expect_identical(covs[[1]], covs[[2]])
})

test_that("an absent keys field still falls back to the legacy key field", {
  path <- write_rules_with("  na_equal: yes", legacy = TRUE)
  on.exit(unlink(path), add = TRUE)
  res <- compare_datasets_from_yaml(tbl, tbl, path = path)
  expect_identical(res$coverage$column[res$coverage$check == "equality"], "f3")
})

test_that("lazy path: keys: [] is positional as well", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, name = "ref_kel", df = tbl)
  duckdb::duckdb_register(con, name = "cand_kel", df = tbl)
  path <- write_rules_with("  keys: []")
  on.exit(unlink(path), add = TRUE)
  res <- suppressMessages(
    compare_datasets_from_yaml(dplyr::tbl(con, "ref_kel"), dplyr::tbl(con, "cand_kel"), path = path)
  )
  expect_true(res$all_passed)
  expect_setequal(res$coverage$column[res$coverage$check == "equality"], c("f2", "f3"))
})

test_that("the key argument stays strict: an empty vector is an error, not a positional comparison", {
  path <- write_rules_with("  keys: [f2]")
  on.exit(unlink(path), add = TRUE)
  expect_error(
    compare_datasets_from_yaml(tbl, tbl, key = character(0), path = path),
    regexp = "non-empty character vector"
  )
  expect_error(
    write_rules_template(tbl, key = character(0), path = tempfile(fileext = ".yaml")),
    regexp = "non-empty character vector"
  )
})
