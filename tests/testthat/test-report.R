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
  write_rules_template(ref, key = KEYR, numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(compare_datasets_from_yaml(
    dplyr::tbl(con, "ref"), dplyr::tbl(con, "cand"), key = KEYR, path = tmp
  ))
  expect_true(res$all_passed)
  expect_identical(class(res$reponse)[1], "datadiff_report")
  expect_output(print(res$reponse))
})
