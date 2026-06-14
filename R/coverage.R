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
  list(n = length(ok), n_failed = sum(is.na(ok) | !ok))
}

# n / n_failed for an equality column (resolved boolean, no NA).
eq_col_counts <- function(tbl, col, ref_suffix, na_equal) {
  b <- eq_col_bool(tbl, col = col, ref_suffix = ref_suffix, na_equal = na_equal)
  list(n = length(b), n_failed = sum(!b))
}

#' Build a faithful coverage summary of the checks performed
#'
#' @param tbl Local data.frame holding the boolean validation columns (and, on
#'   the local path, the raw + reference columns for equality recomputation).
#' @param tol_cols,eq_cols Character vectors of tolerance / equality columns.
#' @param missing_in_candidate,type_mismatch_cols Structural failures.
#' @param row_validation_info List with `check_count`.
#' @param row_count_ok Logical row-count outcome.
#' @param ref_suffix Suffix identifying reference columns.
#' @param na_equal Logical; NA equality semantics for equality columns.
#' @return A `datadiff_coverage` data.frame with columns `column`, `check`,
#'   `n`, `n_failed`, `status` (one row per check performed).
#' @noRd
build_coverage <- function(tbl, tol_cols, eq_cols,
                           missing_in_candidate, type_mismatch_cols,
                           row_validation_info, row_count_ok,
                           ref_suffix, na_equal) {
  columns  <- character(0)
  checks   <- character(0)
  ns       <- integer(0)
  n_failed <- integer(0)

  add <- function(column, check, n, nf) {
    columns  <<- c(columns, column)
    checks   <<- c(checks, check)
    ns       <<- c(ns, as.integer(n))
    n_failed <<- c(n_failed, as.integer(nf))
  }

  # Existence checks: every common (non-type-mismatch) column gets a col_exists
  # check, distinct from its value check. These pass (the column is present in
  # both datasets by construction), mirroring a full per-column pointblank run.
  for (c in c(tol_cols, eq_cols)) {
    add(c, "col_exists", 1L, 0L)
  }
  for (c in tol_cols) {
    cnt <- tol_col_counts(tbl, col = c)
    add(c, "tolerance", cnt$n, cnt$n_failed)
  }
  for (c in eq_cols) {
    cnt <- eq_col_counts(tbl, col = c, ref_suffix = ref_suffix, na_equal = na_equal)
    add(c, "equality", cnt$n, cnt$n_failed)
  }
  for (c in missing_in_candidate) {
    add(c, "missing_column", 1L, 1L)
  }
  for (c in type_mismatch_cols) {
    add(c, "type_mismatch", 1L, 1L)
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
