# Per-column pass predicates, shared by the all-pass short-circuit and the
# failing-column selection.
#   - a tolerance column passes iff every <col>__ok value is TRUE (NA counts as
#     a failure, matching col_vals_equal(..., na_pass = FALSE));
#   - an equality column passes iff its pre-computed <col>__eq column is all
#     TRUE (lazy path) or, when no __eq exists (local path), the raw comparison
#     holds under the shared NA semantics: a one-sided NA is always a
#     difference, a two-sided NA follows na_equal. This matches both the
#     numeric tolerance kernel and the lazy SQL CASE WHEN.

# Per-row boolean outcome of a tolerance column: the pre-computed <col>__ok
# vector. NA (never produced by construction) would count as a failure, matching
# col_vals_equal(..., na_pass = FALSE). A missing __ok column is a hard internal
# error: all(NULL) is TRUE, so returning NULL would silently turn a dropped
# column into a false all-pass.
tol_col_bool <- function(tbl, col) {
  b <- tbl[[datadiff_ok_col(col)]]
  if (is.null(b)) {
    stop(sprintf("internal error: boolean column '%s' missing",
                 datadiff_ok_col(col)),
         call. = FALSE)
  }
  b
}

# Per-row boolean outcome of an equality column, resolved to TRUE/FALSE (no NA):
# the pre-computed <col>__eq vector (lazy path) or, when absent (local path),
# the raw comparison with one-sided NA = FALSE and two-sided NA = na_equal.
eq_col_bool <- function(tbl, col, ref_suffix, na_equal) {
  eq_precomputed <- tbl[[datadiff_eq_col(col)]]
  if (!is.null(eq_precomputed)) {
    return(eq_precomputed)
  }
  cand_vals <- tbl[[col]]
  ref_vals  <- tbl[[paste0(col, ref_suffix)]]
  # Same rationale as tol_col_bool: a missing pair must fail loudly, not
  # yield NULL and a silent all-pass downstream
  if (is.null(cand_vals) || is.null(ref_vals)) {
    missing_cols <- c(
      if (is.null(cand_vals)) {
        sprintf("'%s'", col)
      },
      if (is.null(ref_vals)) {
        sprintf("'%s%s'", col, ref_suffix)
      }
    )
    stop(sprintf(
      "internal error: equality column(s) %s missing (no precomputed '%s' either)",
      paste(missing_cols, collapse = " and "), datadiff_eq_col(col)
    ), call. = FALSE)
  }
  cand_na   <- is.na(cand_vals)
  ref_na    <- is.na(ref_vals)
  cmp_res   <- cand_vals == ref_vals
  # Both values present and equal; any NA resolves below
  out <- !is.na(cmp_res) & cmp_res
  # Two-sided NA follows na_equal; a one-sided NA stays FALSE (a value facing
  # a missing value is a difference)
  out[cand_na & ref_na] <- na_equal
  out
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
