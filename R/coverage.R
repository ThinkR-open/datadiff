# Faithful, O(columns) coverage summary built from the boolean validation
# columns already computed on the fast path. It records one row per check that
# was actually performed, so an all-pass comparison still shows exactly what was
# verified instead of a single trivial step. It is NOT produced by pointblank's
# per-column engine (which is the slow part) but reflects the same verdict,
# because it reuses the very booleans the verdict is derived from.

# n / n_failed for a tolerance column (<col>__ok). NA counts as a failure,
# matching col_vals_equal(..., na_pass = FALSE).
tol_col_counts <- function(tbl, col) {
  ok <- tol_col_bool(tbl, col = col)
  # length - sum(TRUE) counts FALSE and NA in one pass without allocating
  # the two intermediate logical vectors of is.na(ok) | !ok
  list(n = length(ok), n_failed = length(ok) - sum(ok, na.rm = TRUE))
}

# n / n_failed for an equality column (resolved boolean, no NA).
eq_col_counts <- function(tbl, col, ref_suffix, na_equal) {
  b <- eq_col_bool(tbl, col = col, ref_suffix = ref_suffix, na_equal = na_equal)
  list(n = length(b), n_failed = sum(!b))
}

# (n, n_failed) for every boolean validation column of a LAZY table, computed
# in ONE SQL aggregate scan: SUM(CASE WHEN col THEN 0 ELSE 1 END) counts the
# not-TRUE values (FALSE and NULL alike), matching the local reducers
# (tolerance: is.na | !ok; equality: !b, NA-free by construction). This is
# what lets the lazy verdict avoid collecting N x columns booleans into R.
# Returns a named list keyed by the UNDERLYING column name:
# list(<col> = list(n, n_failed), ...).
lazy_boolean_counts <- function(tbl_lazy, tol_cols, eq_cols) {
  con <- dbplyr::remote_con(tbl_lazy)
  sub <- dbplyr::sql_render(tbl_lazy)
  q   <- function(x) as.character(DBI::dbQuoteIdentifier(con, x = x))

  bool_cols <- c(datadiff_ok_col(tol_cols), datadiff_eq_col(eq_cols))
  underlying <- c(tol_cols, eq_cols)
  nf_exprs <- vapply(X = seq_along(bool_cols), FUN = function(i) {
    sprintf("SUM(CASE WHEN %s THEN 0 ELSE 1 END) AS %s",
            q(bool_cols[i]), q(sprintf("..datadiff_nf_%d", i)))
  }, FUN.VALUE = character(1))

  sql <- sprintf(
    "SELECT COUNT(*) AS %s%s FROM (%s) AS %s",
    q("..datadiff_total"),
    if (length(nf_exprs) > 0) {
      paste0(", ", paste(nf_exprs, collapse = ", "))
    } else {
      ""
    },
    sub, q("datadiff_counts")
  )
  row <- DBI::dbGetQuery(con, statement = sql)

  total <- as.numeric(row[["..datadiff_total"]])
  counts <- vector("list", length(underlying))
  names(counts) <- underlying
  for (i in seq_along(underlying)) {
    nf <- as.numeric(row[[sprintf("..datadiff_nf_%d", i)]])
    # SUM over an empty table is NULL: zero rows means zero failures
    counts[[i]] <- list(n = total, n_failed = if (is.na(nf)) {
      0
    } else {
      nf
    })
  }
  counts
}

#' Build a faithful coverage summary of the checks performed
#'
#' @param tbl Local data.frame holding the boolean validation columns (and, on
#'   the local path, the raw + reference columns for equality recomputation).
#'   Ignored when `counts` is supplied.
#' @param tol_cols,eq_cols Character vectors of tolerance / equality columns.
#' @param missing_in_candidate,type_mismatch_cols,tolerance_non_numeric_cols
#'   Structural failures: columns absent from the candidate, columns whose type
#'   differs between the two datasets, and columns carrying a tolerance rule
#'   although they are not numeric in the reference.
#' @param row_validation_info List with `check_count`.
#' @param row_count_ok Logical row-count outcome.
#' @param ref_suffix Suffix identifying reference columns.
#' @param na_equal Logical; NA equality semantics for equality columns.
#' @param counts Optional precomputed per-column counts (named list
#'   `col -> list(n, n_failed)` covering every tolerance and equality column),
#'   as produced by `lazy_boolean_counts()`: the lazy path aggregates in SQL
#'   instead of collecting the booleans.
#' @return A `datadiff_coverage` data.frame with columns `column`, `check`,
#'   `n`, `n_failed`, `status` (one row per check performed).
#' @noRd
build_coverage <- function(tbl, tol_cols, eq_cols,
                           missing_in_candidate, type_mismatch_cols,
                           row_validation_info, row_count_ok,
                           ref_suffix, na_equal, counts = NULL,
                           tolerance_non_numeric_cols = character(0)) {
  columns  <- character(0)
  checks   <- character(0)
  ns       <- numeric(0)
  n_failed <- numeric(0)

  add <- function(column, check, n, nf) {
    columns  <<- c(columns, column)
    checks   <<- c(checks, check)
    # numeric, not integer: the lazy counts come from SQL aggregates and can
    # exceed 2^31 - 1, where as.integer() would overflow to NA and silently
    # corrupt the coverage and the verdict
    ns       <<- c(ns, as.numeric(n))
    n_failed <<- c(n_failed, as.numeric(nf))
  }

  # Existence checks: every common (non-type-mismatch) column gets a col_exists
  # check, distinct from its value check. These pass (the column is present in
  # both datasets by construction), mirroring a full per-column pointblank run.
  for (col_nm in c(tol_cols, eq_cols)) {
    add(col_nm, "col_exists", 1L, 0L)
  }
  for (col_nm in tol_cols) {
    cnt <- counts[[col_nm]] %||% tol_col_counts(tbl, col = col_nm)
    add(col_nm, "tolerance", cnt$n, cnt$n_failed)
  }
  for (col_nm in eq_cols) {
    cnt <- counts[[col_nm]] %||%
      eq_col_counts(tbl, col = col_nm, ref_suffix = ref_suffix, na_equal = na_equal)
    add(col_nm, "equality", cnt$n, cnt$n_failed)
  }
  for (col_nm in missing_in_candidate) {
    add(col_nm, "missing_column", 1L, 1L)
  }
  for (col_nm in type_mismatch_cols) {
    add(col_nm, "type_mismatch", 1L, 1L)
  }
  for (col_nm in tolerance_non_numeric_cols) {
    add(col_nm, "tolerance_on_non_numeric", 1L, 1L)
  }
  if (isTRUE(row_validation_info$check_count)) {
    add("<row_count>", "row_count", 1L, if (isTRUE(row_count_ok)) 0L else 1L)
  }

  out <- data.frame(
    column   = columns,
    check    = checks,
    n        = ns,
    n_failed = n_failed,
    status   = ifelse(n_failed == 0L, "PASS", "FAIL"),
    stringsAsFactors = FALSE
  )
  class(out) <- c("datadiff_coverage", "data.frame")
  out
}

#' Aggregate counts from a coverage table
#'
#' @param coverage A `datadiff_coverage` data.frame from `build_coverage()`.
#' @return A list with `n_checks`, `n_pass`, `n_fail`, `n_rows_failed_total`,
#'   `all_passed`.
#' @noRd
summarize_coverage <- function(coverage) {
  n_fail <- sum(coverage$status == "FAIL")
  list(
    n_checks            = nrow(coverage),
    n_pass              = sum(coverage$status == "PASS"),
    n_fail              = n_fail,
    n_rows_failed_total = sum(coverage$n_failed),
    all_passed          = n_fail == 0L
  )
}

#' Print method for a datadiff coverage summary
#'
#' Shows a one-line roll-up followed by the failing checks first, so an all-pass
#' run still makes visible everything that was verified.
#'
#' @param x A `datadiff_coverage` data.frame.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print datadiff_coverage
print.datadiff_coverage <- function(x, ...) {
  s <- summarize_coverage(x)
  cat(sprintf(
    "datadiff coverage: %d checks - %d PASS, %d FAIL\n",
    s$n_checks, s$n_pass, s$n_fail
  ))
  if (nrow(x) > 0L) {
    ord <- order(x$status != "FAIL") # FAIL rows first
    print(utils::head(as.data.frame(x)[ord, , drop = FALSE], 50L), row.names = FALSE)
    if (nrow(x) > 50L) {
      cat(sprintf("... and %d more checks\n", nrow(x) - 50L))
    }
  }
  invisible(x)
}
