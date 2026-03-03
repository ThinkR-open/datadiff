# Tests for Arrow/Parquet support in compare_datasets_from_yaml.
#
# Uses arrow::read_parquet(as_data_frame=FALSE) for Arrow Tables and
# arrow::open_dataset() for Arrow Datasets. Both are converted to
# DuckDB-backed lazy tables via arrow::to_duckdb() before pointblank
# validation, keeping the pipeline fully lazy.

# ---- helpers ----------------------------------------------------------------

as_arrow_table <- function(df) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  p <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, p)
  arrow::read_parquet(p, as_data_frame = FALSE)
}

as_arrow_dataset <- function(df) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  d <- tempfile()
  dir.create(d)
  arrow::write_parquet(df, file.path(d, "part-0.parquet"))
  arrow::open_dataset(d)
}

yaml_path <- function(content) {
  p <- tempfile(fileext = ".yaml")
  writeLines(content, p)
  p
}

# ---- identical data (with key) ---------------------------------------------

test_that("arrow table: identical data pass (with key)", {
  ref  <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id")
  expect_true(result$all_passed)
})

test_that("arrow dataset: identical data pass (with key)", {
  ref  <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  cand <- data.frame(id = 1:5, value = c(1.0, 2.0, 3.0, 4.0, 5.0))
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id")
  expect_true(result$all_passed)
})

# ---- tolerance abs (pass) ---------------------------------------------------

test_that("arrow table: tolerance abs pass", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.005, 2.005, 3.005))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: tolerance abs pass", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.005, 2.005, 3.005))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- tolerance abs (fail) ---------------------------------------------------

test_that("arrow table: tolerance abs fail", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.5, 2.5, 3.5))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

test_that("arrow dataset: tolerance abs fail", {
  ref  <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.5, 2.5, 3.5))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0.01
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

# ---- tolerance relative (pass) ----------------------------------------------

test_that("arrow table: tolerance rel pass", {
  ref  <- data.frame(id = 1:3, value = c(100.0, 200.0, 300.0))
  cand <- data.frame(id = 1:3, value = c(101.0, 202.0, 303.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.02
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: tolerance rel pass", {
  ref  <- data.frame(id = 1:3, value = c(100.0, 200.0, 300.0))
  cand <- data.frame(id = 1:3, value = c(101.0, 202.0, 303.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.02
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- tolerance relative (fail) ----------------------------------------------

test_that("arrow table: tolerance rel fail", {
  ref  <- data.frame(id = 1:3, value = c(100.0, 200.0, 300.0))
  cand <- data.frame(id = 1:3, value = c(120.0, 240.0, 360.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.02
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

test_that("arrow dataset: tolerance rel fail", {
  ref  <- data.frame(id = 1:3, value = c(100.0, 200.0, 300.0))
  cand <- data.frame(id = 1:3, value = c(120.0, 240.0, 360.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
    rel: 0.02
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

# ---- positional comparison (no key) ----------------------------------------

test_that("arrow table: positional comparison (no key)", {
  ref  <- data.frame(value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"))
  cand <- data.frame(value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"))
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand))
  expect_true(result$all_passed)
})

test_that("arrow dataset: positional comparison (no key)", {
  ref  <- data.frame(value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"))
  cand <- data.frame(value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"))
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand))
  expect_true(result$all_passed)
})

# ---- NA with na_equal=TRUE (numeric) ----------------------------------------

test_that("arrow table: NA numeric na_equal=TRUE", {
  ref  <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: NA numeric na_equal=TRUE", {
  ref  <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- NA with na_equal=TRUE (character) --------------------------------------

test_that("arrow table: NA character na_equal=TRUE", {
  ref  <- data.frame(id = 1:3, name = c("a", NA, "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("a", NA, "c"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: NA character na_equal=TRUE", {
  ref  <- data.frame(id = 1:3, name = c("a", NA, "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("a", NA, "c"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- NA with na_equal=FALSE -------------------------------------------------

test_that("arrow table: NA na_equal=FALSE fails", {
  ref  <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: no
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

test_that("arrow dataset: NA na_equal=FALSE fails", {
  ref  <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  cand <- data.frame(id = 1:3, value = c(1.0, NA, 3.0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: no
row_validation:
  check_count: no
by_type:
  numeric:
    abs: 0
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

# ---- preprocessing case_insensitive ----------------------------------------

test_that("arrow table: case insensitive comparison", {
  ref  <- data.frame(id = 1:3, name = c("Hello", "WORLD", "Foo"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("hello", "world", "foo"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    case_insensitive: yes
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: case insensitive comparison", {
  ref  <- data.frame(id = 1:3, name = c("Hello", "WORLD", "Foo"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("hello", "world", "foo"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    case_insensitive: yes
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- preprocessing trim -----------------------------------------------------

test_that("arrow table: trim whitespace comparison", {
  ref  <- data.frame(id = 1:3, name = c("  a  ", " b ", "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("a", "b", "c"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    trim: yes
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: trim whitespace comparison", {
  ref  <- data.frame(id = 1:3, name = c("  a  ", " b ", "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("a", "b", "c"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    trim: yes
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- preprocessing trim + case combined ------------------------------------

test_that("arrow table: trim + case insensitive combined", {
  ref  <- data.frame(id = 1:3, name = c("  Hello  ", " WORLD ", "Foo"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("hello", "world", "foo"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    case_insensitive: yes
    trim: yes
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: trim + case insensitive combined", {
  ref  <- data.frame(id = 1:3, name = c("  Hello  ", " WORLD ", "Foo"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, name = c("hello", "world", "foo"), stringsAsFactors = FALSE)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: no
by_type:
  character:
    case_insensitive: yes
    trim: yes
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- mixed types (numeric, integer, char) -----------------------------------

test_that("arrow table: mixed types", {
  ref  <- data.frame(id = 1:3, val = c(1.0, 2.0, 3.0), count = 10L:12L,
                     label = c("a", "b", "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, val = c(1.0, 2.0, 3.0), count = 10L:12L,
                     label = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id")
  expect_true(result$all_passed)
})

test_that("arrow dataset: mixed types", {
  ref  <- data.frame(id = 1:3, val = c(1.0, 2.0, 3.0), count = 10L:12L,
                     label = c("a", "b", "c"), stringsAsFactors = FALSE)
  cand <- data.frame(id = 1:3, val = c(1.0, 2.0, 3.0), count = 10L:12L,
                     label = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id")
  expect_true(result$all_passed)
})

# ---- composite keys ---------------------------------------------------------

test_that("arrow table: composite keys", {
  ref  <- data.frame(grp = c("a", "a", "b"), sub = 1:3, value = c(10, 20, 30))
  cand <- data.frame(grp = c("a", "a", "b"), sub = 1:3, value = c(10, 20, 30))
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = c("grp", "sub"))
  expect_true(result$all_passed)
})

test_that("arrow dataset: composite keys", {
  ref  <- data.frame(grp = c("a", "a", "b"), sub = 1:3, value = c(10, 20, 30))
  cand <- data.frame(grp = c("a", "a", "b"), sub = 1:3, value = c(10, 20, 30))
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = c("grp", "sub"))
  expect_true(result$all_passed)
})

# ---- missing column in candidate --------------------------------------------

test_that("arrow table: missing column in candidate", {
  ref  <- data.frame(id = 1:3, value = c(1, 2, 3), extra = c(10, 20, 30))
  cand <- data.frame(id = 1:3, value = c(1, 2, 3))
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id")
  expect_false(result$all_passed)
  expect_true("extra" %in% result$missing_in_candidate)
})

test_that("arrow dataset: missing column in candidate", {
  ref  <- data.frame(id = 1:3, value = c(1, 2, 3), extra = c(10, 20, 30))
  cand <- data.frame(id = 1:3, value = c(1, 2, 3))
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id")
  expect_false(result$all_passed)
  expect_true("extra" %in% result$missing_in_candidate)
})

# ---- row count mismatch -----------------------------------------------------

test_that("arrow table: row count mismatch detected", {
  ref  <- data.frame(id = 1:5, value = 1:5 * 1.0)
  cand <- data.frame(id = 1:3, value = 1:3 * 1.0)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: yes
  tolerance: 0
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

test_that("arrow dataset: row count mismatch detected", {
  ref  <- data.frame(id = 1:5, value = 1:5 * 1.0)
  cand <- data.frame(id = 1:3, value = 1:3 * 1.0)
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
row_validation:
  check_count: yes
  tolerance: 0
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_false(result$all_passed)
})

# ---- empty tables (0 rows) -------------------------------------------------

test_that("arrow table: empty tables (0 rows) pass", {
  ref  <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id")
  expect_true(result$all_passed)
})

test_that("arrow dataset: empty tables (0 rows) pass", {
  ref  <- data.frame(id = integer(0), value = numeric(0))
  cand <- data.frame(id = integer(0), value = numeric(0))
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id")
  expect_true(result$all_passed)
})

# ---- duplicate keys warning -------------------------------------------------

test_that("arrow table: duplicate keys warning", {
  ref  <- data.frame(id = c(1, 1, 2), value = c(1, 2, 3))
  cand <- data.frame(id = c(1, 1, 2), value = c(1, 2, 3))
  expect_warning(
    compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id"),
    "Duplicate keys"
  )
})

test_that("arrow dataset: duplicate keys warning", {
  ref  <- data.frame(id = c(1, 1, 2), value = c(1, 2, 3))
  cand <- data.frame(id = c(1, 1, 2), value = c(1, 2, 3))
  expect_warning(
    compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id"),
    "Duplicate keys"
  )
})

# ---- ignore_columns ---------------------------------------------------------

test_that("arrow table: ignore_columns", {
  ref  <- data.frame(id = 1:3, value = c(1, 2, 3), noise = c(99, 98, 97))
  cand <- data.frame(id = 1:3, value = c(1, 2, 3), noise = c(0, 0, 0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
  ignore_columns:
    - noise
row_validation:
  check_count: no
")
  result <- compare_datasets_from_yaml(as_arrow_table(ref), as_arrow_table(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

test_that("arrow dataset: ignore_columns", {
  ref  <- data.frame(id = 1:3, value = c(1, 2, 3), noise = c(99, 98, 97))
  cand <- data.frame(id = 1:3, value = c(1, 2, 3), noise = c(0, 0, 0))
  p <- yaml_path("
version: 1
defaults:
  na_equal: yes
  ignore_columns:
    - noise
row_validation:
  check_count: no
")
  result <- compare_datasets_from_yaml(as_arrow_dataset(ref), as_arrow_dataset(cand), key = "id", path = p)
  expect_true(result$all_passed)
})

# ---- write_rules_template with Arrow ----------------------------------------

test_that("arrow table: write_rules_template works", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  tbl <- as_arrow_table(ref)
  p <- tempfile(fileext = ".yaml")
  write_rules_template(tbl, key = "id", path = p)
  rules <- read_rules(p)
  expect_equal(rules$version, 1)
  expect_equal(rules$defaults$keys, "id")
})

test_that("arrow dataset: write_rules_template works", {
  ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0), name = c("a", "b", "c"),
                    stringsAsFactors = FALSE)
  ds <- as_arrow_dataset(ref)
  p <- tempfile(fileext = ".yaml")
  write_rules_template(ds, key = "id", path = p)
  rules <- read_rules(p)
  expect_equal(rules$version, 1)
  expect_equal(rules$defaults$keys, "id")
})
