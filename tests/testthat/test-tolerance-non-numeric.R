# A tolerance rule (abs / rel) on a column that is NOT numeric in the reference
# cannot be honoured: datadiff never converts the data. Such a column must not
# silently fall back to string equality; it gets a dedicated failing check,
# `tolerance_on_non_numeric`, like `type_mismatch`.

# Rules file with abs / rel on one column; the caller owns the cleanup.
write_tol_rules <- function(col = "x", key_line = "  keys: [id]") {
  path <- tempfile(fileext = ".yml")
  writeLines(c(
    "version: 1",
    "defaults:",
    key_line,
    "by_name:",
    sprintf("  %s:", col),
    "    abs: 0.5",
    "    rel: 0.1"
  ), con = path)
  path
}

lazy_pair <- function(con, ref, cand, tag = "tnn") {
  ref_name  <- paste0("ref_", tag)
  cand_name <- paste0("cand_", tag)
  duckdb::duckdb_register(con, name = ref_name, df = ref)
  duckdb::duckdb_register(con, name = cand_name, df = cand)
  list(ref = dplyr::tbl(con, ref_name), cand = dplyr::tbl(con, cand_name))
}

test_that("abs/rel on a character column (both sides) is a failing check, not a string equality", {
  ref  <- data.frame(id = 1:3, x = c("1", "2.50", "10"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, x = c("1.0", "2.5", "10.2"), stringsAsFactors = FALSE)
  path <- write_tol_rules()
  on.exit(unlink(path), add = TRUE)

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
  on.exit(unlink(path), add = TRUE)

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
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "by_name:",
    "  x:",
    "    abs: 0.5",
    "  v:",
    "    abs: 0.5"
  ), con = path)

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
  on.exit(unlink(path), add = TRUE)

  expect_no_warning(res <- compare_datasets_from_yaml(ref, cand, path = path))
  expect_true(res$all_passed)
  expect_false("tolerance_on_non_numeric" %in% res$coverage$check)
})

test_that("a type-mismatched column keeps its type_mismatch check, not a second one", {
  ref  <- data.frame(id = 1:2, x = c("1", "2"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:2, x = c(1, 2))
  path <- write_tol_rules()
  on.exit(unlink(path), add = TRUE)

  res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path))
  expect_equal(res$coverage$check[res$coverage$column == "x"], "type_mismatch")
})

test_that("positional comparison (no key) reports the check as well", {
  ref  <- data.frame(x = c("1", "2"), stringsAsFactors = FALSE)
  cand <- data.frame(x = c("1", "2"), stringsAsFactors = FALSE)
  path <- write_tol_rules(key_line = "  keys: ~")
  on.exit(unlink(path), add = TRUE)

  res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path))
  expect_false(res$all_passed)
  expect_equal(res$coverage$check, "tolerance_on_non_numeric")
})

test_that("a by_type rule with abs on character columns triggers the check on every one", {
  ref  <- data.frame(id = 1:2, a = c("x", "y"), b = c("u", "v"), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "by_type:",
    "  character:",
    "    abs: 0.1"
  ), con = path)

  res <- suppressWarnings(compare_datasets_from_yaml(ref, ref, path = path))
  expect_false(res$all_passed)
  expect_setequal(res$coverage$column[res$coverage$check == "tolerance_on_non_numeric"], c("a", "b"))
})

test_that("date and logical reference columns with abs are non-numeric too", {
  ref <- data.frame(id = 1:2, d = as.Date(c("2026-01-01", "2026-01-02")), l = c(TRUE, FALSE))
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "by_name:",
    "  d:",
    "    abs: 1",
    "  l:",
    "    abs: 1"
  ), con = path)

  expect_warning(
    res <- compare_datasets_from_yaml(ref, ref, path = path),
    regexp = "'d' \\(date\\).*'l' \\(logical\\)"
  )
  expect_setequal(res$coverage$column[res$coverage$check == "tolerance_on_non_numeric"], c("d", "l"))
})

test_that("local path: an empty candidate still fails on the structural check", {
  ref  <- data.frame(id = 1:3, x = c("1", "2", "3"), stringsAsFactors = FALSE)
  cand <- data.frame(id = integer(0), x = character(0), stringsAsFactors = FALSE)
  path <- write_tol_rules()
  on.exit(unlink(path), add = TRUE)

  expect_no_error(res <- suppressWarnings(compare_datasets_from_yaml(ref, cand, path = path)))
  expect_false(res$all_passed)
  expect_equal(res$coverage$check[res$coverage$column == "x"], "tolerance_on_non_numeric")
  expect_true(any(grepl("^tolerance_on_non_numeric: x$", res$agent$validation_set$label)))
})

test_that("local path: a failing row count on an empty candidate is a real failure", {
  ref  <- data.frame(id = 1:3, v = c(1, 2, 3))
  cand <- data.frame(id = integer(0), v = numeric(0))
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "row_validation:",
    "  check_count: yes",
    "  tolerance: 0"
  ), con = path)

  res <- compare_datasets_from_yaml(ref, cand, path = path)
  expect_false(res$all_passed)
  expect_equal(res$coverage$n_failed[res$coverage$check == "row_count"], 1)
  expect_false(pointblank::all_passed(res$response))
})

test_that("lazy path: abs/rel on a character column is a failing check as well", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  ref  <- data.frame(id = 1:3, x = c("1", "2.50", "10"), v = c(1, 2, 3),
                     stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, x = c("1.0", "2.5", "10.2"), v = c(1, 2, 3),
                     stringsAsFactors = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  lz <- lazy_pair(con, ref = ref, cand = cand)
  path <- write_tol_rules()
  on.exit(unlink(path), add = TRUE)

  expect_warning(
    res <- compare_datasets_from_yaml(lz$ref, lz$cand, path = path),
    regexp = "tolerance.*'x'",
    ignore.case = TRUE
  )
  expect_false(res$all_passed)
  cov <- res$coverage
  expect_equal(cov$check[cov$column == "x"], "tolerance_on_non_numeric")
  expect_equal(cov$n_failed[cov$column == "v" & cov$check == "equality"], 0)
  expect_null(res$applied_rules$x$abs)
  expect_true(any(grepl("^tolerance_on_non_numeric: x$", res$agent$validation_set$label)))
})

test_that("lazy path: numeric column untouched, type_mismatch keeps priority", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- write_tol_rules()
  on.exit(unlink(path), add = TRUE)

  num <- lazy_pair(con, ref = data.frame(id = 1:2, x = c(1, 2.5)),
                   cand = data.frame(id = 1:2, x = c(1, 2.6)))
  expect_no_warning(res <- compare_datasets_from_yaml(num$ref, num$cand, path = path))
  expect_true(res$all_passed)

  mixed <- lazy_pair(con, ref = data.frame(id = 1:2, x = c("1", "2"), stringsAsFactors = FALSE),
                     cand = data.frame(id = 1:2, x = c(1, 2)), tag = "mixed")
  res2 <- suppressWarnings(compare_datasets_from_yaml(mixed$ref, mixed$cand, path = path))
  expect_equal(res2$coverage$check[res2$coverage$column == "x"], "type_mismatch")
})

test_that("lazy path: empty tables with check_count give a verdict that matches the coverage", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  empty <- data.frame(id = integer(0), x = character(0), stringsAsFactors = FALSE)
  lz <- lazy_pair(con, ref = empty, cand = empty)
  path <- tempfile(fileext = ".yml")
  on.exit(unlink(path), add = TRUE)
  writeLines(c(
    "version: 1",
    "defaults:",
    "  keys: [id]",
    "row_validation:",
    "  check_count: yes",
    "by_name:",
    "  x:",
    "    abs: 0.5"
  ), con = path)

  res <- suppressWarnings(compare_datasets_from_yaml(lz$ref, lz$cand, path = path))
  expect_false(res$all_passed)
  expect_false(pointblank::all_passed(res$response))
  expect_equal(res$summary$all_passed, res$all_passed)
  expect_equal(res$coverage$n_failed[res$coverage$check == "row_count"], 0)
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
