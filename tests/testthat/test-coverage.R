# TDD spec for the faithful coverage summary: build_coverage() turns the
# already-computed boolean validation columns into one row per check actually
# performed (column, check type, n rows, n_failed, status). It must list EVERY
# column and stay consistent with the verdict (status == PASS iff the column
# passes in all_validations_pass / failing_columns).

# --- build_coverage: value checks (tolerance) -------------------------------

test_that("tolerance columns appear once each with correct counts", {
  tbl <- data.frame(
    a__ok = c(TRUE, TRUE, TRUE),
    b__ok = c(TRUE, FALSE, TRUE)
  )
  cov <- build_coverage(
    tbl = tbl, tol_cols = c("a", "b"), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  tol <- cov[cov$check == "tolerance", ]
  expect_equal(tol$column, c("a", "b"))
  expect_equal(tol$n, c(3L, 3L))
  expect_equal(tol$n_failed, c(0L, 1L))
  expect_equal(tol$status, c("PASS", "FAIL"))
  # each column also gets a (passing) col_exists check, distinct from the value check
  expect_setequal(cov$column[cov$check == "col_exists"], c("a", "b"))
  expect_true(all(cov$status[cov$check == "col_exists"] == "PASS"))
})

# --- build_coverage: equality, lazy (__eq) and local (recompute) ------------

test_that("equality columns use pre-computed __eq when present (lazy path)", {
  tbl <- data.frame(txt__eq = c(TRUE, FALSE))
  cov <- build_coverage(
    tbl = tbl, tol_cols = character(0), eq_cols = "txt",
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  eq <- cov[cov$check == "equality", ]
  expect_equal(eq$n, 2L)
  expect_equal(eq$n_failed, 1L)
  expect_equal(eq$status, "FAIL")
  expect_equal(cov$column[cov$check == "col_exists"], "txt")
})

test_that("equality columns recompute from raw values when no __eq (local path)", {
  tbl <- data.frame(
    txt = c("a", "b", "c"),
    txt__reference = c("a", "X", "c"),
    stringsAsFactors = FALSE
  )
  cov <- build_coverage(
    tbl = tbl, tol_cols = character(0), eq_cols = "txt",
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  eq <- cov[cov$check == "equality", ]
  expect_equal(eq$n, 3L)
  expect_equal(eq$n_failed, 1L)
  expect_equal(eq$status, "FAIL")
})

test_that("equality NA semantics follow na_equal (local path)", {
  tbl <- data.frame(
    txt = c("a", NA, "c"),
    txt__reference = c("a", NA, "c"),
    stringsAsFactors = FALSE
  )
  cov_t <- build_coverage(tbl, character(0), "txt", character(0), character(0),
                          list(check_count = FALSE), TRUE, "__reference", TRUE)
  cov_f <- build_coverage(tbl, character(0), "txt", character(0), character(0),
                          list(check_count = FALSE), TRUE, "__reference", FALSE)
  eq_t <- cov_t[cov_t$check == "equality", ]
  eq_f <- cov_f[cov_f$check == "equality", ]
  expect_equal(eq_t$n_failed, 0L) # NA both sides + na_equal TRUE -> pass
  expect_equal(eq_f$n_failed, 1L) # na_equal FALSE -> the NA row fails
})

# --- build_coverage: structural checks --------------------------------------

test_that("missing columns appear as failing structural checks", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = TRUE), tol_cols = "a", eq_cols = character(0),
    missing_in_candidate = c("b", "c"), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  miss <- cov[cov$check == "missing_column", ]
  expect_equal(sort(miss$column), c("b", "c"))
  expect_true(all(miss$status == "FAIL"))
})

test_that("type mismatches appear as failing structural checks", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = TRUE), tol_cols = "a", eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = "z",
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  tm <- cov[cov$check == "type_mismatch", ]
  expect_equal(tm$column, "z")
  expect_equal(tm$status, "FAIL")
})

test_that("row_count appears only when check_count is enabled, with right status", {
  base <- list(tbl = data.frame(a__ok = TRUE), tol_cols = "a",
               eq_cols = character(0), missing_in_candidate = character(0),
               type_mismatch_cols = character(0), ref_suffix = "__reference",
               na_equal = TRUE)
  cov_off <- do.call(build_coverage, c(base, list(
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE)))
  cov_ok <- do.call(build_coverage, c(base, list(
    row_validation_info = list(check_count = TRUE), row_count_ok = TRUE)))
  cov_ko <- do.call(build_coverage, c(base, list(
    row_validation_info = list(check_count = TRUE), row_count_ok = FALSE)))
  expect_false("row_count" %in% cov_off$check)
  expect_equal(cov_ok[cov_ok$check == "row_count", "status"], "PASS")
  expect_equal(cov_ko[cov_ko$check == "row_count", "status"], "FAIL")
})

# --- exhaustivity & degenerate -----------------------------------------------

test_that("every column is covered: common columns get col_exists + a value check", {
  tbl <- data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, FALSE),
                    t__eq = c(TRUE, TRUE))
  cov <- build_coverage(
    tbl = tbl, tol_cols = c("a", "b"), eq_cols = "t",
    missing_in_candidate = "m", type_mismatch_cols = "z",
    row_validation_info = list(check_count = TRUE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  # common columns each appear as a col_exists AND a value (tolerance/equality) check
  expect_setequal(cov$column[cov$check == "col_exists"], c("a", "b", "t"))
  value <- cov[cov$check %in% c("tolerance", "equality"), "column"]
  expect_setequal(value, c("a", "b", "t"))
  # structural checks appear once each
  expect_equal(cov$column[cov$check == "missing_column"], "m")
  expect_equal(cov$column[cov$check == "type_mismatch"], "z")
})

test_that("empty inputs yield a 0-row coverage with the right columns", {
  cov <- build_coverage(
    tbl = data.frame(), tol_cols = character(0), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  expect_equal(nrow(cov), 0L)
  expect_equal(names(cov), c("column", "check", "n", "n_failed", "status"))
})

# --- summarize_coverage ------------------------------------------------------

test_that("summarize_coverage aggregates checks and pass/fail counts", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = c(TRUE, FALSE), b__ok = c(TRUE, TRUE)),
    tol_cols = c("a", "b"), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  # col_exists(a) + col_exists(b) + tolerance(a FAIL) + tolerance(b PASS)
  s <- summarize_coverage(cov)
  expect_equal(s$n_checks, 4L)
  expect_equal(s$n_pass, 3L)
  expect_equal(s$n_fail, 1L)
  expect_false(s$all_passed)
})

# --- print.datadiff_coverage: render in all cases ---------------------------

cov_from <- function(tbl, tol_cols, eq_cols, missing = character(0),
                     mismatch = character(0), check_count = FALSE,
                     row_count_ok = TRUE) {
  build_coverage(tbl, tol_cols, eq_cols, missing, mismatch,
                 list(check_count = check_count), row_count_ok,
                 "__reference", TRUE)
}

test_that("print shows an all-PASS roll-up when everything passes", {
  cov <- cov_from(data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, TRUE)),
                  c("a", "b"), character(0))
  # 2 col_exists + 2 tolerance, all passing
  out <- paste(capture.output(print(cov)), collapse = "\n")
  expect_match(out, "4 checks")
  expect_match(out, "4 PASS")
  expect_match(out, "0 FAIL")
})

test_that("print lists failing checks first when there are failures", {
  cov <- cov_from(data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, FALSE)),
                  c("a", "b"), character(0))
  lines <- capture.output(print(cov))
  expect_match(paste(lines, collapse = "\n"), "1 FAIL")
  body <- lines[grepl("tolerance", lines)]
  expect_match(body[1], "FAIL") # the FAIL row comes first
})

test_that("print handles an all-fail table", {
  cov <- cov_from(data.frame(a__ok = c(FALSE, FALSE)), "a", character(0))
  out <- paste(capture.output(print(cov)), collapse = "\n")
  # col_exists(a) passes, tolerance(a) fails
  expect_match(out, "2 checks - 1 PASS, 1 FAIL")
})

test_that("print handles an empty coverage table without error", {
  cov <- cov_from(data.frame(), character(0), character(0))
  out <- paste(capture.output(print(cov)), collapse = "\n")
  expect_match(out, "0 checks")
})

# --- integration: end-to-end res$coverage ------------------------------------

test_that("green comparison exposes a coverage listing every column as PASS", {
  ref <- data.frame(id = 1:4, .row = 1L, a = c(1, 2, 3, 4) * 1.0,
                    b = c(1, 2, 3, 4) * 2.0, txt = letters[1:4],
                    stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = c("id", ".row"), numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(
    compare_datasets_from_yaml(ref, ref, key = c("id", ".row"), path = tmp)
  )
  expect_true(res$all_passed)
  expect_true(all(res$coverage$status == "PASS"))
  expect_setequal(
    res$coverage$column[res$coverage$check %in% c("tolerance", "equality")],
    c("a", "b", "txt")
  )
  expect_equal(res$summary$all_passed, res$all_passed)
})

test_that("coverage summary verdict matches all_passed on a failing comparison", {
  ref <- data.frame(id = 1:4, .row = 1L, a = c(1, 2, 3, 4) * 1.0,
                    txt = letters[1:4], stringsAsFactors = FALSE)
  cand <- ref
  cand$a[2] <- 99
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = c("id", ".row"), numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = c("id", ".row"), path = tmp)
  )
  expect_false(res$all_passed)
  expect_equal(res$summary$all_passed, res$all_passed)
  cov <- res$coverage
  expect_equal(cov[cov$column == "a" & cov$check == "tolerance", "status"], "FAIL")
  expect_equal(cov[cov$column == "a" & cov$check == "col_exists", "status"], "PASS")
})
