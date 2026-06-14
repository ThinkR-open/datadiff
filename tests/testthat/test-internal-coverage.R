# Targeted tests for internal branches that the main suite did not exercise.

test_that("report_underlying_col maps every internal naming scheme", {
  expect_equal(report_underlying_col("a__ok"), "a")
  expect_equal(report_underlying_col("a__eq"), "a")
  expect_equal(report_underlying_col("row_count_ok"), "<row_count>")
  expect_equal(report_underlying_col("__missing_col_b"), "b")
  expect_equal(report_underlying_col("__type_mismatch_c"), "c")
  expect_equal(report_underlying_col("plain"), "plain")
})

test_that("build_report_agent returns an agent for empty coverage (no real agent)", {
  empty <- build_coverage(
    tbl = data.frame(), tol_cols = character(0), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  ag <- build_report_agent(empty, label = "L", lang = "en", locale = "en_US")
  expect_s3_class(ag, "ptblank_agent")
})

test_that("datadiff_report_html errors when the result has no coverage", {
  expect_error(datadiff_report_html(list(reponse = NULL)), "coverage")
})

test_that("datadiff_render_report tolerates a missing cache environment", {
  ref <- data.frame(id = 1:3, .row = 1L, a = c(1, 2, 3) * 1.0)
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(ref, key = c("id", ".row"), numeric_abs = 0.101,
                       integer_abs = 0L, path = tmp)
  res <- suppressMessages(
    compare_datasets_from_yaml(ref, ref, key = c("id", ".row"), path = tmp)
  )
  x <- res$reponse
  attr(x, "datadiff_render") <- NULL # force the fallback branch
  rep <- datadiff_render_report(x)
  expect_s3_class(rep, "gt_tbl")
})

test_that("print.datadiff_coverage truncates beyond 50 checks", {
  p <- 60L
  tbl <- as.data.frame(matrix(TRUE, nrow = 1L, ncol = p))
  names(tbl) <- paste0("c", seq_len(p), "__ok")
  cov <- build_coverage(
    tbl = tbl, tol_cols = paste0("c", seq_len(p)), eq_cols = character(0),
    missing_in_candidate = character(0), type_mismatch_cols = character(0),
    row_validation_info = list(check_count = FALSE), row_count_ok = TRUE,
    ref_suffix = "__reference", na_equal = TRUE
  )
  expect_gt(nrow(cov), 50L)
  out <- paste(capture.output(print(cov)), collapse = "\n")
  expect_match(out, "more checks")
})
