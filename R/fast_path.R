# Per-column pass predicates, shared by the all-pass short-circuit and the
# failing-column selection. They reproduce pointblank's verdict exactly:
#   - a tolerance column passes iff every <col>__ok value is TRUE (NA counts as
#     a failure, matching col_vals_equal(..., na_pass = FALSE));
#   - an equality column passes iff its pre-computed <col>__eq column is all TRUE
#     (lazy path) or, when no __eq exists (local path), the raw comparison holds
#     under the same na_pass = na_equal semantics pointblank uses (any NA on
#     either side passes iff na_equal).

# Per-row boolean outcome of a tolerance column: the pre-computed <col>__ok
# vector. NA (never produced by construction) would count as a failure, matching
# col_vals_equal(..., na_pass = FALSE).
tol_col_bool <- function(tbl, col) {
  tbl[[paste0(col, "__ok")]]
}

# Per-row boolean outcome of an equality column, resolved to TRUE/FALSE (no NA):
# the pre-computed <col>__eq vector (lazy path) or, when absent (local path), the
# raw comparison under the same na_pass = na_equal semantics pointblank uses
# (any NA on either side passes iff na_equal).
eq_col_bool <- function(tbl, col, ref_suffix, na_equal) {
  eq_precomputed <- tbl[[paste0(col, "__eq")]]
  if (!is.null(eq_precomputed)) {
    return(eq_precomputed)
  }
  cand_vals <- tbl[[col]]
  ref_vals  <- tbl[[paste0(col, ref_suffix)]]
  cmp_res   <- cand_vals == ref_vals
  ifelse(is.na(cmp_res), na_equal, cmp_res)
}

tol_col_passes <- function(tbl, col) {
  isTRUE(all(tol_col_bool(tbl, col = col)))
}

eq_col_passes <- function(tbl, col, ref_suffix, na_equal) {
  isTRUE(all(eq_col_bool(tbl, col = col, ref_suffix = ref_suffix, na_equal = na_equal)))
}

#' Fast global pass/fail without building the per-column pointblank agent
#'
#' The verdict produced by \code{compare_datasets_from_yaml} is entirely
#' determined by boolean columns already computed in a single vectorised pass
#' (\code{<col>__ok} for tolerance columns, \code{<col>__eq} for equality
#' columns on the lazy path). pointblank then re-checks those booleans through
#' one validation step per column, which dominates runtime on wide tables. When
#' every check passes there is nothing to extract, so the agent can be skipped.
#'
#' @param tbl Local data.frame holding the boolean validation columns (and, for
#'   the local path, the raw and reference equality columns).
#' @param tol_cols Character vector of tolerance column names.
#' @param eq_cols Character vector of equality column names (already stripped of
#'   key, type-mismatch and tolerance columns).
#' @param ref_suffix Suffix identifying reference columns.
#' @param na_equal Logical; whether NA equals NA for equality columns.
#' @return \code{TRUE} iff every tolerance and equality check passes.
#' @noRd
all_validations_pass <- function(tbl, tol_cols, eq_cols, ref_suffix, na_equal) {
  for (c in tol_cols) {
    if (!tol_col_passes(tbl, col = c)) {
      return(FALSE)
    }
  }
  for (c in eq_cols) {
    if (!eq_col_passes(tbl, col = c, ref_suffix = ref_suffix, na_equal = na_equal)) {
      return(FALSE)
    }
  }
  TRUE
}

#' Subset of columns whose validation fails
#'
#' Used on the failure path to build pointblank steps only for columns that
#' actually fail. Columns that pass produce empty data extracts, so omitting
#' their steps leaves \code{get_data_extracts()} unchanged while avoiding the
#' per-column agent overhead on the passing majority.
#'
#' @inheritParams all_validations_pass
#' @return A list with \code{tol} and \code{eq} character vectors of failing
#'   column names.
#' @noRd
failing_columns <- function(tbl, tol_cols, eq_cols, ref_suffix, na_equal) {
  fail_tol <- tol_cols[vapply(tol_cols, function(c) {
    !tol_col_passes(tbl, col = c)
  }, logical(1))]
  fail_eq <- eq_cols[vapply(eq_cols, function(c) {
    !eq_col_passes(tbl, col = c, ref_suffix = ref_suffix, na_equal = na_equal)
  }, logical(1))]
  list(tol = fail_tol, eq = fail_eq)
}

#' Build a minimal pointblank agent whose interrogation passes
#'
#' Used for the all-pass short-circuit: produces a valid interrogated
#' \code{ptblank_agent} for which \code{pointblank::all_passed()} is \code{TRUE}
#' and \code{pointblank::get_data_extracts()} is empty, at constant cost,
#' instead of one validation step per column.
#'
#' @param tbl Table the agent reports on (only its first column is used, via a
#'   \code{col_exists} step that always passes).
#' @param label,warn_at,stop_at,lang,locale Passed through to
#'   \code{pointblank::create_agent} / \code{action_levels}.
#' @return A configured (not yet interrogated) \code{ptblank_agent}.
#' @noRd
build_pass_agent <- function(tbl, label, warn_at, stop_at, lang, locale) {
  # The trivially-passing col_exists step needs at least one column to target.
  # Degenerate inputs (no tolerance, equality or row-count columns to validate)
  # produce a 0-column table, on which tbl[, 1] would error; synthesise a dummy
  # column so create_agent()/col_exists() stay well-defined and still pass.
  if (ncol(tbl) == 0) {
    tbl <- data.frame(.datadiff_pass = TRUE)
  }
  slim <- tbl[, 1, drop = FALSE]
  agent <- pointblank::create_agent(
    tbl = slim, label = label,
    actions = pointblank::action_levels(warn_at = warn_at, stop_at = stop_at),
    lang = lang, locale = locale
  )
  agent %>% pointblank::col_exists(columns = tidyselect::all_of(names(slim)[1]))
}
