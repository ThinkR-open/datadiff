# The comparison result exposes the interrogated agent under `response`.
# The historical `reponse` name stays readable for one release through the
# `datadiff_result` accessors, with a deprecation warning, so existing
# pipelines keep working while they migrate.

make_pair <- function() {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  list(ref = ref, cand = ref)
}

test_that("the result carries the interrogated agent under `response`", {
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  expect_s3_class(res, "datadiff_result")
  expect_true("response" %in% names(res))
  expect_false("reponse" %in% names(res))
  expect_s3_class(res$response, "ptblank_agent")
  expect_s3_class(res$response, "datadiff_report")
  expect_true(pointblank::all_passed(res$response))
})

test_that("`$reponse` still resolves, with a deprecation warning", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  expect_warning(via_dollar <- res$reponse, "reponse")
  expect_identical(via_dollar, res$response)
})

test_that("`[[\"reponse\"]]` still resolves, with a deprecation warning", {
  rlang::local_options(rlib_warning_verbosity = "verbose")
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  expect_warning(via_brackets <- res[["reponse"]], "reponse")
  expect_identical(via_brackets, res$response)
})

test_that("the other result fields keep plain list access semantics", {
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  expect_identical(res$all_passed, res[["all_passed"]])
  expect_true(res$all_passed)
  expect_null(res$does_not_exist)
  expect_identical(res[[1L]], res$all_passed)
  expect_s3_class(res$coverage, "data.frame")
})

test_that("printing the result does not leak the class attribute", {
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  printed <- capture.output(print(res))
  expect_false(any(grepl("datadiff_result", printed, fixed = TRUE)))
})

test_that("datadiff_report_html() accepts a result saved by an older version", {
  d <- make_pair()
  res <- suppressMessages(
    compare_datasets_from_yaml(d$ref, d$cand, key = "id")
  )
  legacy <- list(
    all_passed = res$all_passed,
    coverage   = res$coverage,
    reponse    = res$response
  )
  expect_no_error(report <- datadiff_report_html(legacy))
})
