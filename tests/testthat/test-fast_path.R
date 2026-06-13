test_that("build_pass_agent tolerates a zero-column table", {
  # Degenerate input: when there is nothing to validate (no tolerance, equality
  # or row-count columns) the slim table collected on the lazy path has rows but
  # zero columns. build_pass_agent must not error on `tbl[, 1]` and must still
  # produce a trivially-passing agent.
  empty <- data.frame(row.names = 1:3) # 3 rows, 0 columns
  expect_equal(ncol(empty), 0L)

  agent <- build_pass_agent(
    tbl = empty,
    label = "lbl",
    warn_at = 1e-14,
    stop_at = 1e-14,
    lang = "en",
    locale = "en_US"
  )
  res <- pointblank::interrogate(agent)
  expect_true(pointblank::all_passed(res))
})

test_that("build_pass_agent uses the real first column when one exists", {
  tbl <- data.frame(a__ok = c(TRUE, TRUE), b__eq = c(TRUE, TRUE))

  agent <- build_pass_agent(
    tbl = tbl,
    label = "lbl",
    warn_at = 1e-14,
    stop_at = 1e-14,
    lang = "en",
    locale = "en_US"
  )
  res <- pointblank::interrogate(agent)
  expect_true(pointblank::all_passed(res))
})
