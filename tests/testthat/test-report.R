# TDD spec for the lazy pointblank-style HTML report.
#
# The fast path keeps res$reponse a real interrogated agent (so all_passed() and
# get_data_extracts() keep working), but prefixes class "datadiff_report" and
# attaches the coverage so that PRINTING res$reponse lazily builds a full
# pointblank report (one step per column) on demand - the cost is paid only when
# the report is displayed, never on the compute path.

KEYR <- c("id", ".row")

mk_ref <- function() {
  data.frame(id = 1:4, .row = 1L, a = c(1, 2, 3, 4) * 1.0,
             b = c(1, 2, 3, 4) * 2.0, txt = letters[1:4],
             stringsAsFactors = FALSE)
}

run <- function(ref, cand) {
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = KEYR, numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = KEYR, path = tmp)
  )
}

# --- build_report_agent: one step per coverage row, injected results --------

test_that("build_report_agent produces one validation step per coverage row", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, FALSE),
                     t__eq = c(TRUE, TRUE)),
    tol_cols = c("a", "b"), eq_cols = "t",
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  ag <- build_report_agent(cov, label = "L", lang = "en", locale = "en_US")
  expect_s3_class(ag, "ptblank_agent")
  expect_equal(nrow(ag$validation_set), nrow(cov))
  # injected counts are consistent with the coverage table
  expect_equal(ag$validation_set$n_passed, cov$n - cov$n_failed)
})

test_that("build_report_agent warn/stop honour the supplied thresholds", {
  # column b fails 1 of 2 rows -> f_failed = 0.5.
  cov <- build_coverage(
    tbl = data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, FALSE)),
    tol_cols = c("a", "b"), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  b_row <- which(cov$column == "b" & cov$check == "tolerance")

  # Thresholds ABOVE the fraction: column b should stay green.
  ag_loose <- build_report_agent(
    cov, label = "L", lang = "en", locale = "en_US",
    warn_at = 0.6, stop_at = 0.8
  )
  expect_false(ag_loose$validation_set$warn[b_row])
  expect_false(ag_loose$validation_set$stop[b_row])

  # Thresholds BELOW the fraction: column b should warn and stop.
  ag_tight <- build_report_agent(
    cov, label = "L", lang = "en", locale = "en_US",
    warn_at = 0.4, stop_at = 0.45
  )
  expect_true(ag_tight$validation_set$warn[b_row])
  expect_true(ag_tight$validation_set$stop[b_row])
})

test_that("failing report merges the real extract and lists every column", {
  ref  <- data.frame(id = 1:5, .row = 1L, a = c(1, 2, 3, 4, 5) * 1.0,
                     b = c(10, 20, 30, 40, 50) * 1.0)
  cand <- ref
  cand$a[2] <- 999  # 2 failing rows on column a; b passes
  cand$a[4] <- 888
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = c("id", ".row"), numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = c("id", ".row"), path = tmp)
  )
  expect_false(res$all_passed)

  ag <- build_report_agent(res$coverage, label = "L", lang = "en",
                           locale = "en_US", real_agent = res$reponse)
  vs <- ag$validation_set

  # every coverage column is represented (full "X tests" overview)
  expect_setequal(unlist(vs$column), unique(res$coverage$column))

  # the failing column 'a' VALUE check keeps the REAL extract (2 failing rows),
  # carried over to the new step index in agent$extracts
  is_a <- vapply(vs$column, identical, logical(1), "a")
  i_a <- which(is_a & vs$assertion_type == "col_vals_equal")
  expect_length(i_a, 1L)
  extract_a <- ag$extracts[[as.character(vs$i[i_a])]]
  expect_false(is.null(extract_a))
  expect_equal(nrow(as.data.frame(extract_a)), 2L)
  expect_equal(vs$n_failed[i_a], 2)

  # passing column 'b' value check has no extract
  is_b <- vapply(vs$column, identical, logical(1), "b")
  i_b <- which(is_b & vs$assertion_type == "col_vals_equal")
  expect_null(ag$extracts[[as.character(vs$i[i_b])]])

  expect_s3_class(pointblank::get_agent_report(ag), "gt_tbl")
})

test_that("col_exists rows stay PASS even when the column's value check fails", {
  ref <- mk_ref(); cand <- ref
  cand$a[2] <- 999  # value check for 'a' fails
  res <- run(ref, cand)
  expect_false(res$all_passed)
  ag <- build_report_agent(res$coverage, label = "L", lang = "en",
                           locale = "en_US", real_agent = res$reponse)
  vs <- ag$validation_set
  is_a <- vapply(vs$column, identical, logical(1), "a")

  # the col_exists check for 'a' must remain PASS, with no extract
  ce <- which(is_a & vs$assertion_type == "col_exists")
  expect_length(ce, 1L)
  expect_equal(vs$n_failed[ce], 0)
  expect_null(ag$extracts[[as.character(vs$i[ce])]])

  # the value check for 'a' still fails and keeps its extract
  cv <- which(is_a & vs$assertion_type == "col_vals_equal")
  expect_equal(vs$n_failed[cv], 1)
  expect_false(is.null(ag$extracts[[as.character(vs$i[cv])]]))
})

test_that("report counts match coverage for structural checks (augment path)", {
  ref  <- data.frame(id = 1:5, .row = 1L, a = c(1, 2, 3, 4, 5) * 1.0,
                     b = c(1, 2, 3, 4, 5) * 1.0)
  cand <- ref
  cand$b <- NULL      # column b missing (structural)
  cand$a[2] <- 999    # value check on a fails (keeps the augment path active)
  res <- run(ref, cand)
  expect_false(res$all_passed)

  ag <- build_report_agent(res$coverage, label = "L", lang = "en",
                           locale = "en_US", real_agent = res$reponse)
  vs <- ag$validation_set

  # the missing_column row must report coverage's counts (1/1), not nrow
  cov_b <- res$coverage[res$coverage$column == "b" &
                          res$coverage$check == "missing_column", ]
  i_b <- which(vapply(vs$column, identical, logical(1), "b"))
  expect_equal(vs$n[i_b], cov_b$n)
  expect_equal(vs$n_failed[i_b], cov_b$n_failed)

  # every report row's counts line up with the coverage row in the same position
  expect_equal(vs$n_failed, res$coverage$n_failed)
  expect_equal(vs$n, res$coverage$n)

  # the failing value column 'a' still keeps its real extract
  is_a <- vapply(vs$column, identical, logical(1), "a")
  i_a <- which(is_a & vs$assertion_type == "col_vals_equal")
  expect_false(is.null(ag$extracts[[as.character(vs$i[i_a])]]))
})

test_that("datadiff_report_html on a failing comparison contains the failing values", {
  ref  <- data.frame(id = 1:5, .row = 1L, a = c(1, 2, 3, 4, 5) * 1.0)
  cand <- ref
  cand$a[2] <- 999
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = c("id", ".row"), numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = c("id", ".row"), path = tmp)
  )
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  datadiff_report_html(res, file = out)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("999", html))   # the failing value is in the report
  expect_true(grepl("CSV", html))   # the extract is downloadable
})

test_that("report presents col_exists AND col_vals_equal as distinct checks", {
  res <- run(mk_ref(), mk_ref())  # all green
  expect_true(res$all_passed)
  ag <- build_report_agent(res$coverage, label = "L", lang = "en",
                           locale = "en_US", real_agent = res$reponse)
  types <- ag$validation_set$assertion_type
  # both kinds of check must appear, not collapsed into one
  expect_true("col_exists" %in% types)
  expect_true("col_vals_equal" %in% types)
  # the existence checks line up with the coverage col_exists rows
  expect_equal(sum(types == "col_exists"),
               sum(res$coverage$check == "col_exists"))
})

test_that("get_agent_report renders the injected agent without error", {
  cov <- build_coverage(
    tbl = data.frame(a__ok = c(TRUE, TRUE), b__ok = c(TRUE, FALSE)),
    tol_cols = c("a", "b"), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  ag <- build_report_agent(cov, label = "L", lang = "en", locale = "en_US")
  expect_no_error(pointblank::get_agent_report(ag, display_table = FALSE))
  gt_rep <- pointblank::get_agent_report(ag, display_table = TRUE)
  expect_s3_class(gt_rep, "gt_tbl")
})

# --- API compatibility: res$reponse stays a usable agent --------------------

test_that("green res$reponse is a datadiff_report that is still a usable agent", {
  res <- run(mk_ref(), mk_ref())
  expect_true(res$all_passed)
  expect_identical(class(res$reponse)[1], "datadiff_report")
  expect_true(inherits(res$reponse, "ptblank_agent"))
  expect_true(inherits(res$reponse, "has_intel"))
  expect_true(pointblank::all_passed(res$reponse))
  expect_length(pointblank::get_data_extracts(res$reponse), 0L)
})

test_that("red res$reponse keeps failing cells extractable", {
  ref <- mk_ref(); cand <- ref
  cand$a[2] <- 99
  res <- run(ref, cand)
  expect_false(res$all_passed)
  expect_identical(class(res$reponse)[1], "datadiff_report")
  expect_true(inherits(res$reponse, "ptblank_agent"))
  expect_equal(failing_cells(res, key = KEYR), "a@2|1")
})

# --- print renders, in green and red ----------------------------------------

test_that("printing res$reponse renders without error (green and red)", {
  res_g <- run(mk_ref(), mk_ref())
  expect_output(print(res_g$reponse))

  ref <- mk_ref(); cand <- ref; cand$txt[3] <- "Z"
  res_r <- run(ref, cand)
  expect_output(print(res_r$reponse))
})

# --- memoization: second render reuses the cache ----------------------------

test_that("the rendered report is memoized after first print", {
  res <- run(mk_ref(), mk_ref())
  cache <- attr(res$reponse, "datadiff_render")
  expect_true(is.environment(cache))
  expect_null(cache$report)
  invisible(capture.output(print(res$reponse)))
  expect_false(is.null(cache$report)) # populated by the first print
})

# --- datadiff_report_html writes an HTML file -------------------------------

test_that("datadiff_report_html writes a non-empty HTML file", {
  res <- run(mk_ref(), mk_ref())
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  datadiff_report_html(res, file = tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 0)
})

# --- lazy path ---------------------------------------------------------------

test_that("lazy comparison also yields a printable datadiff_report", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ref <- mk_ref()
  duckdb::dbWriteTable(con, "ref", ref)
  duckdb::dbWriteTable(con, "cand", ref)
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = KEYR, numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref"), dplyr::tbl(con, "cand"), key = KEYR, path = tmp
  ))
  expect_true(res$all_passed)
  expect_identical(class(res$reponse)[1], "datadiff_report")
  expect_output(print(res$reponse))
})
