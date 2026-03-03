# Tests for lazy table (dbplyr SQL-backed) support in compare_datasets_from_yaml.
#
# Uses DBI + RSQLite + dbplyr to create in-memory lazy SQL tables of various
# sizes. These tests pass with the current collect()-based implementation and
# will serve as a regression suite once collect() is removed in favour of
# native lazy evaluation.
#
# Table sizes used:
#   small  :   5 rows
#   medium : 500 rows
#   large  : 5 000 rows

# ---- helpers ----------------------------------------------------------------

lazy_con <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

as_lazy <- function(df, con, name) {
  DBI::dbWriteTable(con, name, df, overwrite = TRUE)
  dplyr::tbl(con, name)
}

yaml_path <- function(content) {
  p <- tempfile(fileext = ".yaml")
  writeLines(content, p)
  p
}

# ---- small tables (5 rows) --------------------------------------------------

test_that("lazy: identical small tables pass (with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:5L, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1L:5L, value = c(1.0, 2.0, 3.0, 4.0, 5.0))

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id"
  )

  expect_type(result, "list")
  expect_s3_class(result$agent, "ptblank_agent")
  expect_true(result$all_passed)
})

test_that("lazy: small tables within tolerance pass (with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:5L, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1L:5L, value = c(1.005, 2.005, 3.005, 4.005, 5.005))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: small tables outside tolerance fail (with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:5L, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1L:5L, value = c(2.0, 3.0, 4.0, 5.0, 6.0)) # off by 1

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

test_that("lazy: identical small tables pass (positional, no key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(value = c(10.0, 20.0, 30.0))
  cand <- data.frame(value = c(10.0, 20.0, 30.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    path = p
  )

  expect_type(result, "list")
  expect_true(result$all_passed)
})

# ---- medium tables (500 rows) -----------------------------------------------

test_that("lazy: medium identical tables pass (500 rows, with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n    <- 500L
  vals <- seq(1.0, 500.0, by = 1.0)
  ref  <- data.frame(id = 1L:n, value = vals)
  cand <- data.frame(id = 1L:n, value = vals)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id"
  )

  expect_true(result$all_passed)
})

test_that("lazy: medium tables with sparse differences fail (500 rows, with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n    <- 500L
  vals <- seq(1.0, 500.0, by = 1.0)
  cand_vals <- vals
  cand_vals[c(50L, 150L, 250L, 350L, 450L)] <-
    cand_vals[c(50L, 150L, 250L, 350L, 450L)] + 10.0

  ref  <- data.frame(id = 1L:n, value = vals)
  cand <- data.frame(id = 1L:n, value = cand_vals)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

# ---- large tables (5 000 rows) ----------------------------------------------

test_that("lazy: large identical tables pass (5000 rows, with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n    <- 5000L
  vals <- seq(0.001, 5.0, length.out = n)
  ref  <- data.frame(id = 1L:n, value = vals)
  cand <- data.frame(id = 1L:n, value = vals)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id"
  )

  expect_true(result$all_passed)
})

test_that("lazy: large tables with scattered differences fail (5000 rows, with key)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  n    <- 5000L
  vals <- seq(0.001, 5.0, length.out = n)
  cand_vals <- vals
  diff_idx <- seq(500L, 5000L, by = 500L) # 10 differing rows
  cand_vals[diff_idx] <- cand_vals[diff_idx] + 1.0

  ref  <- data.frame(id = 1L:n, value = vals)
  cand <- data.frame(id = 1L:n, value = cand_vals)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

# ---- multiple column types --------------------------------------------------

test_that("lazy: mixed types (numeric, integer, character) pass when identical", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref <- data.frame(
    id      = 1L:6L,
    score   = c(1.1, 2.2, 3.3, 4.4, 5.5, 6.6),
    count   = 10L:15L,
    label   = c("a", "b", "c", "d", "e", "f"),
    stringsAsFactors = FALSE
  )
  cand <- ref

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id"
  )

  expect_true(result$all_passed)
  expect_equal(length(result$missing_in_candidate), 0L)
})

test_that("lazy: character column case difference is detected (exact mode)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(
    id    = 1L:4L,
    label = c("Alice", "Bob", "Charlie", "Diana"),
    stringsAsFactors = FALSE
  )
  cand <- data.frame(
    id    = 1L:4L,
    label = c("Alice", "Bob", "charlie", "Diana"), # "charlie" differs
    stringsAsFactors = FALSE
  )

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    equal_mode: exact
    case_insensitive: false
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

# ---- structural issues ------------------------------------------------------

test_that("lazy: missing column in candidate fails validation", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0), extra = c(10L, 20L, 30L))
  cand <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0)) # 'extra' missing

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id"
  )

  expect_false(result$all_passed)
  expect_true("extra" %in% result$missing_in_candidate)
})

test_that("lazy: row count mismatch fails when check_count is enabled", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:5L, value = seq(1.0, 5.0, by = 1.0))
  cand <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0)) # 3 rows vs 5

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
  keys: ["id"]
row_validation:
  check_count: yes
  tolerance: 0
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    path = p
  )

  expect_false(result$all_passed)
})

# ---- NA handling (na_equal = TRUE) -----------------------------------------

test_that("lazy: NA values match when na_equal = TRUE (numeric)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:4L, value = c(1.0, NA, 3.0, NA))
  cand <- data.frame(id = 1L:4L, value = c(1.0, NA, 3.0, NA))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: NA values match when na_equal = TRUE (character)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:4L, label = c("a", NA, "c", NA), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1L:4L, label = c("a", NA, "c", NA), stringsAsFactors = FALSE)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: NA vs non-NA fails when na_equal = TRUE", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0)) # row 2: NA vs 2.0

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

# ---- NA handling (na_equal = FALSE) ----------------------------------------

test_that("lazy: NA == NA fails when na_equal = FALSE (numeric)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1L:3L, value = c(1.0, NA, 3.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: no
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

test_that("lazy: NA == NA fails when na_equal = FALSE (character)", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, label = c("a", NA, "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1L:3L, label = c("a", NA, "c"), stringsAsFactors = FALSE)

  p <- yaml_path('
version: 1
defaults:
  na_equal: no
row_validation:
  check_count: no
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

test_that("lazy: no NAs passes when na_equal = FALSE", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: no
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

# ---- preprocessing (trim + case_insensitive) on lazy -----------------------

test_that("lazy: case_insensitive preprocessing makes case-different strings match", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, label = c("Alice", "BOB", "Charlie"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1L:3L, label = c("alice", "bob", "charlie"), stringsAsFactors = FALSE)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    equal_mode: normalized
    case_insensitive: yes
    trim: no
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: trim preprocessing removes leading/trailing whitespace", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, label = c("  Alice  ", "Bob  ", "  Charlie"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1L:3L, label = c("Alice", "Bob", "Charlie"), stringsAsFactors = FALSE)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    equal_mode: normalized
    case_insensitive: no
    trim: yes
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: trim + case_insensitive combined preprocessing", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:2L, label = c("  HELLO  ", "  World"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1L:2L, label = c("hello", "world"), stringsAsFactors = FALSE)

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    equal_mode: normalized
    case_insensitive: yes
    trim: yes
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

# ---- relative tolerance on lazy --------------------------------------------

test_that("lazy: relative tolerance passes when within bounds", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # rel = 0.1 means threshold = abs + rel * |ref| = 0 + 0.1 * |ref|
  # For ref = 100, threshold = 10 -> cand = 105 is within
  ref  <- data.frame(id = 1L:3L, value = c(100.0, 200.0, 50.0))
  cand <- data.frame(id = 1L:3L, value = c(105.0, 210.0, 52.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.1
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

test_that("lazy: relative tolerance fails when outside bounds", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # rel = 0.05 means threshold = 0 + 0.05 * 100 = 5
  # cand = 106 is outside (diff = 6 > 5)
  ref  <- data.frame(id = 1L:2L, value = c(100.0, 200.0))
  cand <- data.frame(id = 1L:2L, value = c(106.0, 200.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.05
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_false(result$all_passed)
})

# ---- composite keys --------------------------------------------------------

test_that("lazy: composite key (multi-column) works correctly", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref <- data.frame(
    grp = c("A", "A", "B", "B"),
    sub = c(1L, 2L, 1L, 2L),
    value = c(10.0, 20.0, 30.0, 40.0),
    stringsAsFactors = FALSE
  )
  cand <- ref

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = c("grp", "sub")
  )

  expect_true(result$all_passed)
})

test_that("lazy: composite key detects differences", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref <- data.frame(
    grp = c("A", "A", "B", "B"),
    sub = c(1L, 2L, 1L, 2L),
    value = c(10.0, 20.0, 30.0, 40.0),
    stringsAsFactors = FALSE
  )
  cand <- data.frame(
    grp = c("A", "A", "B", "B"),
    sub = c(1L, 2L, 1L, 2L),
    value = c(10.0, 999.0, 30.0, 40.0), # row 2 differs
    stringsAsFactors = FALSE
  )

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = c("grp", "sub"), path = p
  )

  expect_false(result$all_passed)
})

# ---- write_rules_template with lazy table ----------------------------------

test_that("lazy: write_rules_template works with lazy table input", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  lazy_ref <- as_lazy(ref, con, "ref")

  p <- tempfile(fileext = ".yaml")
  on.exit(unlink(p), add = TRUE)

  write_rules_template(lazy_ref, key = "id", path = p)
  rules <- read_rules(p)

  expect_equal(rules$version, 1)
  expect_equal(rules$defaults$keys, "id")
  expect_true("value" %in% names(rules$by_name))
  expect_true("name" %in% names(rules$by_name))
})

# ---- empty tables (0 rows) ------------------------------------------------

test_that("lazy: empty tables (0 rows) pass validation", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

# ---- duplicate keys warning on lazy ----------------------------------------

test_that("lazy: duplicate keys produce a warning", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = c(1L, 1L, 2L), value = c(1.0, 1.0, 2.0))
  cand <- data.frame(id = c(1L, 2L, 2L), value = c(1.0, 2.0, 2.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  expect_warning(
    compare_datasets_from_yaml(
      as_lazy(ref, con, "ref"),
      as_lazy(cand, con, "cand"),
      key = "id", path = p
    ),
    "Duplicate keys detected"
  )
})

# ---- ignore_columns on lazy ------------------------------------------------

test_that("lazy: ignore_columns excludes columns from comparison", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0), noisy = c(100.0, 200.0, 300.0))
  cand <- data.frame(id = 1L:3L, value = c(1.0, 2.0, 3.0), noisy = c(999.0, 999.0, 999.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
  ignore_columns: ["noisy"]
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  result <- compare_datasets_from_yaml(
    as_lazy(ref, con, "ref"),
    as_lazy(cand, con, "cand"),
    key = "id", path = p
  )

  expect_true(result$all_passed)
})

# ---- positional collect message --------------------------------------------

test_that("lazy: positional comparison emits collect message", {
  con <- lazy_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ref  <- data.frame(value = c(1.0, 2.0))
  cand <- data.frame(value = c(1.0, 2.0))

  p <- yaml_path('
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
')
  on.exit(unlink(p), add = TRUE)

  expect_message(
    compare_datasets_from_yaml(
      as_lazy(ref, con, "ref"),
      as_lazy(cand, con, "cand"),
      path = p
    ),
    "positional comparison requires collecting"
  )
})
