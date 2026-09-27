# A tolerance rule (abs / rel) on a column that is NOT numeric in the reference
# cannot be honoured: datadiff never converts the data. Such a column must not
# silently fall back to string equality; it gets a dedicated failing check,
# `tolerance_on_non_numeric`, like `type_mismatch` (issue #60).

write_tol_rules <- function(col = "x", key = "id") {
  path <- withr::local_tempfile(fileext = ".yml", .local_envir = parent.frame())
  writeLines(c(
    "version: 1",
    "defaults:",
    sprintf("  keys: [%s]", key),
    "by_name:",
    sprintf("  %s:", col),
    "    abs: 0.5",
    "    rel: 0.1"
  ), path)
  path
}

test_that("abs/rel on a character column (both sides) is a failing check, not a string equality", {
  ref  <- data.frame(id = 1:3, x = c("1", "2.50", "10"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, x = c("1.0", "2.5", "10.2"), stringsAsFactors = FALSE)
  path <- write_tol_rules()

  expect_warning(
    res <- compare_datasets_from_yaml(ref, cand, path = path),
    regexp = "tolerance.*'x'.*character",
    ignore.case = TRUE
  )
  expect_false(res$all_passed)

  cov <- res$coverage
  expect_equal(cov$check[cov$column == "x"], "tolerance_on_non_numeric")
  expect_equal(cov$n_failed[cov$column == "x"], 1)
  expect_false("equality" %in% cov$check)
  expect_false("tolerance" %in% cov$check)

  expect_null(res$applied_rules$x$abs)
  expect_null(res$applied_rules$x$rel)

  labels <- res$agent$validation_set$label
  expect_true(any(grepl("^tolerance_on_non_numeric: x$", labels)))
})

test_that("equal strings do not rescue a tolerance rule on a character column", {
  ref  <- data.frame(id = 1:2, x = c("1", "2"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, x = c("1", "2"), stringsAsFactors = FALSE)
  path <- write_tol_rules()

  res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path))
  expect_false(res$all_passed)
  expect_equal(res$coverage$check[res$coverage$column == "x"], "tolerance_on_non_numeric")
})

test_that("other columns are still validated next to a tolerance_on_non_numeric column", {
  # "v" and not "y": a bare y is a YAML 1.1 boolean, read_yaml() turns the key into TRUE
  ref  <- data.frame(id = 1:2, x = c("1", "2"), v = c(1.0, 2.0), z = c("a", "b"),
                     stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, x = c("1", "2"), v = c(1.0, 2.4), z = c("a", "B"),
                     stringsAsFactors = FALSE)
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "by_name:",
    "  x:",
    "    abs: 0.5",
    "  v:",
    "    abs: 0.5"
  ), path)

  res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path))
  cov <- res$coverage
  expect_equal(cov$check[cov$column == "x"], "tolerance_on_non_numeric")
  expect_equal(cov$check[cov$column == "v" & cov$check != "col_exists"], "tolerance")
  expect_equal(cov$n_failed[cov$column == "v" & cov$check == "tolerance"], 0)
  expect_equal(cov$check[cov$column == "z" & cov$check != "col_exists"], "equality")
  expect_equal(cov$n_failed[cov$column == "z" & cov$check == "equality"], 1)
  expect_equal(res$applied_rules$v$abs, 0.5)
})

test_that("a numeric reference column with abs/rel is untouched by the new check", {
  ref  <- data.frame(id = 1:3, x = c(1, 2.5, 10))
  cand <- data.frame(id = 1:3, x = c(1, 2.5, 10.2))
  path <- write_tol_rules()

  expect_no_warning(res <- compare_datasets_from_yaml(ref, cand, path = path))
  expect_true(res$all_passed)
  expect_false("tolerance_on_non_numeric" %in% res$coverage$check)
})

test_that("a type-mismatched column keeps its type_mismatch check, not a second one", {
  ref  <- data.frame(id = 1:2, x = c("1", "2"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, x = c(1, 2))
  path <- write_tol_rules()

  res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path))
  expect_equal(res$coverage$check[res$coverage$column == "x"], "type_mismatch")
})

test_that("lazy path: abs/rel on a character column is a failing check as well", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  ref  <- data.frame(id = 1:3, x = c("1", "2.50", "10"), y = c(1, 2, 3),
                     stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, x = c("1.0", "2.5", "10.2"), y = c(1, 2, 3),
                     stringsAsFactors = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "ref_tnn", ref)
  duckdb::duckdb_register(con, "cand_tnn", cand)
  ref_lazy  <- dplyr::tbl(con, "ref_tnn")
  cand_lazy <- dplyr::tbl(con, "cand_tnn")
  path <- write_tol_rules()

  expect_warning(
    res <- compare_datasets_from_yaml(ref_lazy, cand_lazy, path = path),
    regexp = "tolerance.*'x'",
    ignore.case = TRUE
  )
  expect_false(res$all_passed)
  cov <- res$coverage
  expect_equal(cov$check[cov$column == "x"], "tolerance_on_non_numeric")
  expect_equal(cov$n_failed[cov$column == "y" & cov$check == "equality"], 0)
  expect_null(res$applied_rules$x$abs)
  expect_true(any(grepl("^tolerance_on_non_numeric: x$", res$agent$validation_set$label)))
})

test_that("build_coverage lists tolerance_on_non_numeric columns as failing structural checks", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = c(TRUE, TRUE)),
    tol_cols = "a", eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    tolerance_non_numeric_cols = "z",
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  row <- cov[cov$check == "tolerance_on_non_numeric", ]
  expect_equal(row$column, "z")
  expect_equal(row$n_failed, 1)
  expect_equal(row$status, "FAIL")
})

test_that("setup_pointblank_agent adds one always-failing step per tolerance_on_non_numeric column", {
  cmp <- data.frame(a = 1:2, a__reference = 1:2)
  agent <- setup_pointblank_agent(
    cmp, common_cols = "a", tol_cols = character(0),
    row_validation_info = list(check_count = FALSE),
    ref_suffix = "__reference", warn_at = 0.1, stop_at = 0.1, label = "t",
    na_equal = TRUE, tolerance_non_numeric_cols = "z"
  )
  res <- pointblank::interrogate(agent)
  expect_false(pointblank::all_passed(res))
  vs <- res$validation_set
  expect_true("tolerance_on_non_numeric: z" %in% vs$label)
  expect_equal(vs$n_failed[which(vs$label == "tolerance_on_non_numeric: z")], 2)
})

test_that("report_underlying_col strips the tolerance_on_non_numeric prefix", {
  expect_equal(report_underlying_col("__tolerance_non_numeric_c"), "c")
})
