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

# --- regression guard for the "mark as interrogated" hack -------------------
#
# The all-green report takes build_report_agent's synthetic count-only branch.
# pointblank fills the EVAL / UNITS / PASS columns of the rendered report ONLY
# for an agent it considers interrogated (pointblank::get_agent_report() gates
# on an internal `has_agent_intel()` that tests `inherits(agent, "has_intel")`);
# a non-interrogated agent renders as a bare "No Interrogation Performed" plan
# with those columns blank. We avoid the O(columns) real interrogation and just
# stamp the markers (the "has_intel" class + timestamps) in mark_agent_interrogated().
#
# These tests fail loudly if a future pointblank release changes that contract -
# renames the marker class, changes the banner text, or stops reading our
# injected counts - so the hack cannot rot silently across upgrades.

test_that("pointblank's interrogated-agent contract still holds (marker class + banner)", {
  # If pointblank renames "has_intel" or its banner, the synthetic report stops
  # rendering counts; pin both so the breakage is attributed here, not blamed on
  # datadiff. Built from a genuine interrogation so it tracks the real contract.
  agent <- pointblank::create_agent(tbl = data.frame(x = TRUE)) %>%
    pointblank::col_vals_equal(columns = "x", value = TRUE) %>%
    pointblank::interrogate()

  # The marker class our hack relies on, asserted against a real interrogation.
  expect_true(inherits(agent, "has_intel"))
  expect_true(exists("has_agent_intel", where = asNamespace("pointblank")))
  expect_true(pointblank:::has_agent_intel(agent))

  # A NON-interrogated agent must still produce the banner we test against below;
  # if pointblank drops/renames it, the HTML assertions are no longer meaningful.
  skip_if_not_installed("gt")
  plan <- pointblank::create_agent(tbl = data.frame(x = TRUE)) %>%
    pointblank::col_vals_equal(columns = "x", value = TRUE)
  plan_html <- as.character(gt::as_raw_html(
    pointblank::get_agent_report(plan, display_table = TRUE)
  ))
  expect_true(grepl("No Interrogation Performed", plan_html))
})

test_that("all-pass synthetic report renders as interrogated (units not blank in HTML)", {
  skip_if_not_installed("gt")
  cov <- build_coverage(
    tbl = data.frame(a__ok = rep(TRUE, 3L), b__ok = rep(TRUE, 3L)),
    tol_cols = c("a", "b"), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  ag <- build_report_agent(cov, label = "L", lang = "en", locale = "en_US")

  # The agent must carry the interrogated markers our hack injects.
  expect_true(inherits(ag, "has_intel"))
  expect_false(is.null(ag$time_start))

  # The rendered HTML must be a real report, not the empty "plan" view.
  html <- as.character(gt::as_raw_html(
    pointblank::get_agent_report(ag, display_table = TRUE)
  ))
  expect_false(grepl("No Interrogation Performed", html))

  # The injected per-check counts survive into the rendered report.
  rep <- pointblank::get_agent_report(ag, display_table = FALSE)
  tol_rows <- which(cov$check == "tolerance")
  expect_false(anyNA(rep$units))
  expect_equal(as.integer(rep$units[tol_rows]), as.integer(cov$n[tol_rows]))
  expect_equal(as.integer(rep$n_pass[tol_rows]),
               as.integer(cov$n[tol_rows] - cov$n_failed[tol_rows]))
})

test_that("end-to-end: all-pass comparison HTML report shows row counts (both paths)", {
  # Faithful reproduction of the user-visible symptom on both compute paths: the
  # rendered HTML for an all-green run must show the evaluated row counts, not a
  # blank "No Interrogation Performed" plan.
  skip_if_not_installed("gt")
  ref <- data.frame(id = 1:50, a = as.numeric(1:50), b = as.numeric(1:50))
  rules <- tempfile(fileext = ".yml")
  on.exit(unlink(rules), add = TRUE)
  write_rules_template(ref, key = "id", path = rules, numeric_abs = 0.001)

  # The "no interrogation" banner is localised; datadiff_report_html defaults to
  # lang = "fr", so grepping only the English string would silently pass against
  # buggy output. Assert against every localisation of the banner pointblank
  # ships, so the guard is locale-robust.
  banners <- unlist(
    pointblank:::get_lsv("agent_report/no_interrogation_performed_text"),
    use.names = FALSE
  )
  has_banner <- function(res) {
    f <- tempfile(fileext = ".html")
    on.exit(unlink(f), add = TRUE)
    datadiff_report_html(res, file = f)
    html <- paste(readLines(f, warn = FALSE), collapse = "\n")
    any(vapply(banners, function(b) grepl(b, html, fixed = TRUE), logical(1)))
  }

  # RAM (local data.frame) path.
  res_ram <- suppressMessages(
    compare_datasets_from_yaml(ref, ref, key = "id", path = rules)
  )
  expect_true(res_ram$all_passed)
  expect_false(has_banner(res_ram))

  # Lazy (DuckDB) path.
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "ref", ref, overwrite = TRUE)
  lref <- dplyr::tbl(con, "ref")
  res_lazy <- suppressMessages(
    compare_datasets_from_yaml(lref, lref, key = "id", path = rules)
  )
  expect_true(res_lazy$all_passed)
  expect_false(has_banner(res_lazy))
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
