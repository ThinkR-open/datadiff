test_that("setup_pointblank_agent creates valid agent", {
  # Create test comparison data
  cmp <- data.frame(
    id = c(1, 2, 3),
    id__reference = c(1, 2, 3),
    value = c(1.1, 2.2, 3.3),
    value__reference = c(1.0, 2.0, 3.0),
    value__absdiff = c(0.1, 0.2, 0.3),
    value__thresh = c(0.2, 0.3, 0.4),
    value__ok = c(TRUE, TRUE, TRUE),
    text = c("a", "b", "c"),
    text__reference = c("a", "b", "c")
  )

  common_cols <- c("id", "value", "text")
  tol_cols <- c("value")
  rules <- list()
  ref_suffix <- "__reference"

  # Test agent creation
  agent <- setup_pointblank_agent(cmp, common_cols = common_cols, tol_cols = tol_cols,
                                 row_validation_info = rules, ref_suffix = ref_suffix,
                                  warn_at = 0.1, stop_at = 0.1, label = "Test", na_equal = TRUE)

  # Check that agent was created
  expect_s3_class(agent, "ptblank_agent")

  # Check agent properties
  expect_equal(agent$label, "Test")
  expect_equal(agent$actions$warn_fraction, 0.1)
  expect_equal(agent$actions$stop_fraction, 0.1)
})

test_that("setup_pointblank_agent handles different column types", {
  # Test with only exact columns (no tolerance)
  cmp <- data.frame(
    id = c(1, 2),
    id__reference = c(1, 2),
    text = c("a", "b"),
    text__reference = c("a", "b")
  )

  common_cols <- c("id", "text")
  tol_cols <- character(0)  # No tolerance columns
  rules <- list()
  ref_suffix <- "__reference"

  agent <- setup_pointblank_agent(cmp, common_cols = common_cols, tol_cols = tol_cols,
                                 row_validation_info = rules, ref_suffix = ref_suffix,
                                  warn_at = 0.5, stop_at = 0.5, label = "Test Exact", na_equal = FALSE)

  expect_s3_class(agent, "ptblank_agent")
  expect_equal(agent$label, "Test Exact")
})

test_that("setup_pointblank_agent handles empty inputs", {
  # Test with minimal data
  cmp <- data.frame(id = 1, id__reference = 1)

  agent <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                  warn_at = 1.0, stop_at = 1.0, label = "Minimal", na_equal = TRUE)

  expect_s3_class(agent, "ptblank_agent")
  expect_equal(agent$label, "Minimal")
})

test_that("setup_pointblank_agent uses French language by default", {
  cmp <- data.frame(id = 1, id__reference = 1)

  agent <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                  warn_at = 0.1, stop_at = 0.1, label = "Test", na_equal = TRUE)

  expect_s3_class(agent, "ptblank_agent")
  expect_equal(agent$lang, "fr")
  expect_equal(agent$locale, "fr_FR")
})

test_that("setup_pointblank_agent accepts custom language parameters", {
  cmp <- data.frame(id = 1, id__reference = 1)

  # Test English
  agent_en <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                     warn_at = 0.1, stop_at = 0.1, label = "Test EN",
                                     na_equal = TRUE, lang = "en", locale = "en_US")

  expect_s3_class(agent_en, "ptblank_agent")
  expect_equal(agent_en$lang, "en")
  expect_equal(agent_en$locale, "en_US")

  # Test German
  agent_de <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                     warn_at = 0.1, stop_at = 0.1, label = "Test DE",
                                     na_equal = TRUE, lang = "de", locale = "de_DE")

  expect_s3_class(agent_de, "ptblank_agent")
  expect_equal(agent_de$lang, "de")
  expect_equal(agent_de$locale, "de_DE")
})

test_that("setup_pointblank_agent accepts various supported languages", {
  cmp <- data.frame(id = 1, id__reference = 1)

  # Test Spanish
  agent_es <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                     warn_at = 0.1, stop_at = 0.1, label = "Test ES",
                                     na_equal = TRUE, lang = "es", locale = "es_ES")
  expect_equal(agent_es$lang, "es")
  expect_equal(agent_es$locale, "es_ES")

  # Test Portuguese
  agent_pt <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                     warn_at = 0.1, stop_at = 0.1, label = "Test PT",
                                     na_equal = TRUE, lang = "pt", locale = "pt_BR")
  expect_equal(agent_pt$lang, "pt")
  expect_equal(agent_pt$locale, "pt_BR")

  # Test Italian
  agent_it <- setup_pointblank_agent(cmp, common_cols = c("id"), tol_cols = character(0),
                                  row_validation_info = list(), ref_suffix = "__reference",
                                     warn_at = 0.1, stop_at = 0.1, label = "Test IT",
                                     na_equal = TRUE, lang = "it", locale = "it_IT")
  expect_equal(agent_it$lang, "it")
  expect_equal(agent_it$locale, "it_IT")
})

test_that("setup_pointblank_agent handles lazy table with missing_in_candidate and type_mismatch_cols", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "test_tbl", data.frame(val__ok = c(TRUE, TRUE)))
  lazy_tbl <- dplyr::tbl(con, "test_tbl")

  # Passing a lazy table exercises the dplyr::mutate() path (lines 52 and 63)
  agent <- setup_pointblank_agent(
    lazy_tbl,
    common_cols     = character(0),
    tol_cols        = c("val"),
    row_validation_info = list(check_count = FALSE),
    ref_suffix      = "__reference",
    warn_at         = 0.1,
    stop_at         = 0.1,
    label           = "lazy test",
    na_equal        = TRUE,
    missing_in_candidate = "absent_col",
    type_mismatch_cols   = "mismatched_col"
  )

  expect_s3_class(agent, "ptblank_agent")
})

test_that("cols_reference is deprecated and unused", {
  cmp <- data.frame(a = 1:3, a__reference = 1:3)
  row_info <- list(check_count = FALSE)
  expect_warning(
    setup_pointblank_agent(cmp, cols_reference = c("a"), common_cols = "a",
                           tol_cols = character(0), row_validation_info = row_info,
                           ref_suffix = "__reference", warn_at = 0.1, stop_at = 0.1,
                           label = "x", na_equal = TRUE),
    regexp = "deprecated"
  )
  # Omitting it works, produces the same agent shape and stays silent
  expect_no_warning(
    agent <- setup_pointblank_agent(cmp, common_cols = "a",
                                  tol_cols = character(0), row_validation_info = row_info,
                                  ref_suffix = "__reference", warn_at = 0.1, stop_at = 0.1,
                                  label = "x", na_equal = TRUE)
  )
  expect_s3_class(agent, "ptblank_agent")
})

test_that("direct equality steps use the shared NA semantics via a derived boolean", {
  # One-sided NA with na_equal = TRUE: must FAIL (kernel/lazy semantics),
  # not pass through col_vals_equal's na_pass
  cmp <- data.frame(
    s = c("a", NA, "c"),
    s__reference = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  row_info <- list(check_count = FALSE)
  agent <- setup_pointblank_agent(cmp, common_cols = "s",
                                  tol_cols = character(0), row_validation_info = row_info,
                                  ref_suffix = "__reference", warn_at = 0.1, stop_at = 0.1,
                                  label = "na semantics", na_equal = TRUE)
  reponse <- pointblank::interrogate(agent)
  expect_false(pointblank::all_passed(reponse))

  # The step no longer embeds the reference VECTOR in the agent: the equality
  # is validated through a derived __eq boolean column
  vs <- reponse$validation_set
  step_cols <- vapply(vs$column, function(cc) cc[1], character(1))
  expect_true("s__eq" %in% step_cols)
})
