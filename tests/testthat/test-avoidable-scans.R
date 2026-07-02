# Avoidable scans and transfers: row counts are only computed when consumed,
# the keyed join carries only the key and the compared columns, and the lazy
# duplicate-key detection aggregates in SQL instead of collecting every
# duplicated group.

test_that("validate_row_counts can skip the COUNT(*) when nothing consumes it", {
  ref  <- data.frame(id = 1:3, x = 1:3)
  cand <- data.frame(id = 1:3, x = 1:3)
  rules <- list(row_validation = list(check_count = FALSE, expected_count = NULL, tolerance = 0))

  info <- validate_row_counts(ref, cand, rules, count_rows = FALSE)
  expect_false(info$check_count)
  expect_true(is.na(info$ref_count))
  expect_true(is.na(info$cand_count))

  # Default stays counting (backward compatible for direct callers)
  info_default <- validate_row_counts(ref, cand, rules)
  expect_identical(info_default$ref_count, 3L)
  expect_identical(info_default$cand_count, 3L)
})

test_that("ignored and extra columns stay out of the joined comparison", {
  ref  <- data.frame(id = 1:3, x = c(1, 2, 3), noise = c("a", "b", "c"))
  cand <- data.frame(id = 1:3, x = c(1, 2, 9), noise = c("zz", "zz", "zz"),
                     extra = c(TRUE, FALSE, TRUE))

  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  write_rules_template(ref, key = "id", path = yaml_path,
                       ignore_columns_default = "noise")

  res <- suppressWarnings(suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id", path = yaml_path)
  ))
  expect_false(res$all_passed)

  # The failing-row extract carries the key and the compared columns, not the
  # ignored column nor the candidate-only extra column
  ex <- pointblank::get_data_extracts(res$reponse)
  ex1 <- as.data.frame(ex[[1]])
  expect_true("id" %in% names(ex1))
  expect_false("noise" %in% names(ex1))
  expect_false("extra" %in% names(ex1))
})

test_that("lazy duplicate detection aggregates in SQL (few rows transferred)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # 1000 duplicated key values: the old code collected all 1000 groups,
  # the aggregate path must return the same counts and 3 examples + "..."
  df <- data.frame(id = rep(seq_len(1000), each = 2), v = 1)
  duckdb::dbWriteTable(con, "t_many_dups", df)
  info <- find_duplicate_keys(dplyr::tbl(con, "t_many_dups"), "id")
  expect_equal(as.numeric(info$n_dup_keys), 1000)
  expect_equal(as.numeric(info$n_dup_rows), 2000)
  expect_length(info$examples, 4L)          # 3 examples + "..."
  expect_identical(info$examples[4], "...")

  # Below the truncation threshold: no "..." marker
  duckdb::dbWriteTable(con, "t_two_dups",
                       data.frame(id = c(1, 1, 2, 2, 3), v = 1:5))
  info2 <- find_duplicate_keys(dplyr::tbl(con, "t_two_dups"), "id")
  expect_equal(as.numeric(info2$n_dup_keys), 2)
  expect_length(info2$examples, 2L)
})
