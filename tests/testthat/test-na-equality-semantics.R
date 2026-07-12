# NA semantics for equality (non-tolerance) columns, aligned across the local
# and lazy paths: a one-sided NA is always a difference; a two-sided NA follows
# na_equal. Same semantics as the numeric tolerance kernel.

compare_both_paths <- function(ref, cand, key, na_equal) {
  yaml_path <- tempfile(fileext = ".yaml")
  on.exit(unlink(yaml_path), add = TRUE)
  writeLines(sprintf('
version: 1
defaults:
  keys: [%s]
  na_equal: %s
row_validation:
  check_count: no
', key, if (na_equal) "yes" else "no"), con = yaml_path)

  res_local <- suppressWarnings(suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = key, path = yaml_path)
  ))

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::dbWriteTable(con, "ref_na",  ref)
  duckdb::dbWriteTable(con, "cand_na", cand)
  res_lazy <- suppressWarnings(suppressMessages(
    compare_datasets_from_yaml(
      dplyr::tbl(con, "ref_na"), dplyr::tbl(con, "cand_na"),
      key = key, path = yaml_path
    )
  ))
  list(local = res_local$all_passed, lazy = res_lazy$all_passed)
}

test_that("one-sided NA on an equality column fails identically on both paths", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  ref  <- data.frame(id = 1:2, s = c("a", NA), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, s = c("a", "b"), stringsAsFactors = FALSE)

  for (na_equal in c(TRUE, FALSE)) {
    out <- compare_both_paths(ref, cand, key = "id", na_equal = na_equal)
    expect_false(out$local)
    expect_identical(out$local, out$lazy)
  }

  # Mirror case: NA on the candidate side
  for (na_equal in c(TRUE, FALSE)) {
    out <- compare_both_paths(cand, ref, key = "id", na_equal = na_equal)
    expect_false(out$local)
    expect_identical(out$local, out$lazy)
  }
})

test_that("two-sided NA on an equality column follows na_equal on both paths", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  ref  <- data.frame(id = 1:2, s = c("a", NA), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, s = c("a", NA), stringsAsFactors = FALSE)

  out_true <- compare_both_paths(ref, cand, key = "id", na_equal = TRUE)
  expect_true(out_true$local)
  expect_identical(out_true$local, out_true$lazy)

  out_false <- compare_both_paths(ref, cand, key = "id", na_equal = FALSE)
  expect_false(out_false$local)
  expect_identical(out_false$local, out_false$lazy)
})

test_that("a candidate row with no reference match fails on both paths", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  ref  <- data.frame(id = 1:2, s = c("a", "b"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, s = c("a", "b", "c"), stringsAsFactors = FALSE)

  # The left join fills the reference side with NA: one-sided NA, hence a
  # failure, even under na_equal = TRUE
  for (na_equal in c(TRUE, FALSE)) {
    out <- compare_both_paths(ref, cand, key = "id", na_equal = na_equal)
    expect_false(out$local)
    expect_identical(out$local, out$lazy)
  }
})
