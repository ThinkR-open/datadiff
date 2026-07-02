# Report construction robustness: the HTML export shares the print()
# memoization, and a genuine evaluation error on the real agent must reach the
# report instead of being silently replaced by synthetic passing rows.

test_that("datadiff_report_html reads and feeds the print() memoization", {
  ref  <- data.frame(id = 1:2, x = c(1, 2))
  cand <- data.frame(id = 1:2, x = c(1, 3))
  res <- suppressMessages(compare_datasets_from_yaml(ref, cand, key = "id"))

  cache <- attr(res$reponse, "datadiff_render")
  expect_true(is.environment(cache))
  expect_null(cache$report)

  # Rendering through the HTML entry point must populate the shared cache...
  r1 <- datadiff_report_html(res, file = NULL)
  expect_false(is.null(cache$report))

  # ...and a second render must return the memoized object itself
  r2 <- datadiff_report_html(res, file = NULL)
  expect_identical(r2, cache$report)
})

test_that("an eval_error on the real agent reaches the report", {
  ref  <- data.frame(id = 1:2, x = c(1, 2))
  cand <- data.frame(id = 1:2, x = c(1, 3))
  res <- suppressMessages(compare_datasets_from_yaml(ref, cand, key = "id"))

  # Simulate a step whose interrogation failed to evaluate: pointblank stores
  # eval_error = TRUE and n_failed = NA for such steps. Target the x__ok value
  # step (the one the report maps back to the "x" coverage row).
  vs <- res$reponse$validation_set
  err_idx <- which(vapply(vs$column, function(cc) {
    identical(cc[1], "x__ok")
  }, logical(1)))[1]
  vs$eval_error[err_idx] <- TRUE
  vs$n_failed[err_idx]   <- NA_real_
  res$reponse$validation_set <- vs

  agent <- build_report_agent(
    coverage = res$coverage,
    label = "eval error propagation",
    real_agent = res$reponse
  )

  # The synthetic branch must not swallow the failure: the rebuilt validation
  # set still carries the eval_error of the mapped real step
  expect_true(any(agent$validation_set$eval_error, na.rm = TRUE))
})

test_that("an all-NA n_failed agent (pure eval_error) is not treated as all-pass", {
  ref  <- data.frame(id = 1:2, x = c(1, 2))
  cand <- data.frame(id = 1:2, x = c(1, 3))
  res <- suppressMessages(compare_datasets_from_yaml(ref, cand, key = "id"))

  # Every real step errored: n_failed is NA everywhere, eval_error TRUE
  vs <- res$reponse$validation_set
  vs$eval_error <- rep(TRUE, nrow(vs))
  vs$n_failed   <- rep(NA_real_, nrow(vs))
  res$reponse$validation_set <- vs

  agent <- build_report_agent(
    coverage = res$coverage,
    label = "pure eval error",
    real_agent = res$reponse
  )

  # Pre-fix, the any(n_failed > 0, na.rm = TRUE) guard was FALSE and the
  # synthetic count-only branch silently dropped the real agent (and its
  # extracts); the eval_error must survive instead
  expect_true(any(agent$validation_set$eval_error, na.rm = TRUE))
})
