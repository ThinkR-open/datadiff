#' Setup pointblank agent for data validation
#'
#' Creates and configures a pointblank validation agent with all necessary validation steps
#' including column existence checks, exact value comparisons, and tolerance validations.
#'
#' The equality steps validate a `<col>__eq` boolean (one-sided NA fails,
#' two-sided NA follows `na_equal`, matching the tolerance kernel and the lazy
#' SQL): when `cmp` does not already carry it, it is derived on the fly for a
#' local data.frame. Tolerance steps expect the `<col>__ok` booleans to be
#' precomputed (see [add_tolerance_columns()]). A lazy `cmp` must carry every
#' boolean column already.
#'
#' @param cmp Comparison dataframe with candidate and reference data
#' @param cols_reference Deprecated and unused; supplying any non-NULL value
#'   raises a warning (an explicit NULL is silent) and the argument will be
#'   removed in a future release
#' @param common_cols Character vector of equality columns to validate (the
#'   internal pipeline passes only the failing ones on its failure path)
#' @param tol_cols Character vector of columns with tolerance rules
#' @param row_validation_info List with row validation information from validate_row_counts
#' @param ref_suffix Suffix for reference columns
#' @param warn_at Warning threshold (fraction of failing tests)
#' @param stop_at Stop threshold (fraction of failing tests)
#' @param label Descriptive label for the validation
#' @param na_equal Logical indicating if NA values are considered equal
#' @param lang Language code for pointblank reports (default: "fr"). Supported values include
#'   "en" (English), "fr" (French), "de" (German), "it" (Italian), "es" (Spanish), "pt" (Portuguese),
#'   "zh" (Chinese), "ja" (Japanese), "ru" (Russian), etc. See pointblank documentation for full list.
#' @param locale Locale code for number and date formatting (default: "fr_FR"). Examples: "en_US",
#'   "en_GB", "de_DE", "es_ES", "pt_BR", "zh_CN", "ja_JP".
#' @param missing_in_candidate Character vector of columns missing in candidate dataset
#' @param type_mismatch_cols Character vector of columns whose type differs between
#'   reference and candidate (e.g. numeric in reference, character in candidate).
#'   A dedicated failing validation step labelled `type_mismatch: <column>` is
#'   added for each such column.
#' @param tolerance_non_numeric_cols Character vector of columns carrying a
#'   tolerance rule (`abs` or `rel`) although they are not numeric in the
#'   reference. \{datadiff\} never converts data, so the rule cannot be
#'   honoured: a dedicated failing validation step labelled
#'   `tolerance_on_non_numeric: <column>` is added for each. The comparison
#'   pipeline keeps such a column out of `common_cols` and `tol_cols`, and out
#'   of `type_mismatch_cols`; this function does not enforce that exclusion.
#'   The step itself is table-wide: it fails on any `cmp`, including a
#'   zero-row one, and does not depend on the number of rows.
#' @param add_col_exists_steps Logical indicating whether to add `col_exists` validation
#'   steps for common columns (default: `TRUE`). Set to `FALSE` for the non-local (lazy
#'   table) path where `cmp` only contains pre-computed boolean columns, not the original
#'   data columns.
#' @return Configured pointblank agent ready for interrogation
#' @examples
#' cmp <- data.frame(
#'   a = 1:3, a__reference = 1:3,
#'   b = c(1.1, 2.2, 3.3), b__reference = c(1.0, 2.0, 3.0),
#'   b__ok = c(FALSE, FALSE, FALSE)
#' )
#' row_info <- list(check_count = FALSE)
#' agent <- setup_pointblank_agent(
#'   cmp,
#'   common_cols = "a", tol_cols = "b", row_validation_info = row_info,
#'   ref_suffix = "__reference", warn_at = 0.1, stop_at = 0.1,
#'   label = "Test", na_equal = TRUE
#' )
#' pointblank::all_passed(pointblank::interrogate(agent))
#' @importFrom pointblank create_agent col_exists col_vals_equal action_levels
#' @importFrom tidyselect all_of
#' @importFrom rlang :=
#' @export
setup_pointblank_agent <- function(cmp, cols_reference = NULL, common_cols, tol_cols,
                                   row_validation_info = NULL, ref_suffix, warn_at, stop_at, label,
                                   na_equal, lang = "fr", locale = "fr_FR",
                                   missing_in_candidate = character(0),
                                   type_mismatch_cols = character(0),
                                   add_col_exists_steps = TRUE,
                                   tolerance_non_numeric_cols = character(0)) {
  if (!is.null(cols_reference)) {
    warning(
      "The 'cols_reference' argument of setup_pointblank_agent() is deprecated and unused; it will be removed in a future release.",
      call. = FALSE
    )
  }

  # Derive the missing <col>__eq booleans BEFORE creating the agent, so every
  # equality step validates the same boolean the verdict logic uses (shared NA
  # semantics) instead of embedding the whole reference vector in the step
  # (O(n_rows) per step, serialised with the report, and na_pass cannot
  # express the one-sided/two-sided NA distinction).
  eq_cols <- setdiff(x = common_cols, y = tol_cols)
  if (!is_non_local(cmp)) {
    for (col_nm in eq_cols) {
      eq_col <- datadiff_eq_col(col_nm)
      if (!(eq_col %in% get_col_names(cmp))) {
        cmp[[eq_col]] <- eq_col_bool(
          cmp, col = col_nm, ref_suffix = ref_suffix, na_equal = na_equal
        )
      }
    }
  }
  # Hoisted once, AFTER every cmp mutation above
  col_names <- get_col_names(cmp)

  # A structural failure (missing column, type mismatch, tolerance rule on a
  # non-numeric column) is a property of the schema, not of the rows: its step
  # is table-wide, a col_exists() on a reserved name that is deliberately
  # absent, so it fails on any table, empty or not, local or lazy, with one
  # unit. A real column carrying that name would turn the step into a passing
  # one: refuse it loudly rather than emit a wrong verdict.
  structural_cols <- c(
    datadiff_structural_col(missing_in_candidate, prefix = datadiff_prefix_missing_col),
    datadiff_structural_col(type_mismatch_cols, prefix = datadiff_prefix_type_mismatch),
    datadiff_structural_col(tolerance_non_numeric_cols, prefix = datadiff_prefix_tolerance_non_numeric)
  )
  collisions <- intersect(x = structural_cols, y = col_names)
  if (length(collisions) > 0) {
    stop(sprintf(
      "cmp carries reserved structural column name(s): %s",
      paste(collisions, collapse = ", ")
    ), call. = FALSE)
  }

  agent <- create_agent(tbl = cmp, label = label,
                        actions = action_levels(warn_at = warn_at, stop_at = stop_at),
                        lang = lang,
                        locale = locale
  )

  # Validate that common columns exist (these will pass).
  # Skipped for the non-local path where cmp is a slim table containing only
  # boolean validation columns (the original data columns are not present).
  if (add_col_exists_steps) {
    for (col_nm in common_cols) {
      agent <- agent %>% col_exists(columns = all_of(col_nm))
    }
  }

  # Structural steps: one always-failing, table-wide step per column.
  for (col_nm in missing_in_candidate) {
    agent <- agent %>%
      col_exists(
        columns = all_of(datadiff_structural_col(col_nm, prefix = datadiff_prefix_missing_col)),
        label = paste("col_exists:", col_nm)
      )
  }
  for (col_nm in type_mismatch_cols) {
    agent <- agent %>%
      col_exists(
        columns = all_of(datadiff_structural_col(col_nm, prefix = datadiff_prefix_type_mismatch)),
        label = paste("type_mismatch:", col_nm)
      )
  }
  for (col_nm in tolerance_non_numeric_cols) {
    agent <- agent %>%
      col_exists(
        columns = all_of(datadiff_structural_col(col_nm, prefix = datadiff_prefix_tolerance_non_numeric)),
        label = paste("tolerance_on_non_numeric:", col_nm)
      )
  }

  for (col_nm in eq_cols) {
    eq_col <- datadiff_eq_col(col_nm)
    if (!(eq_col %in% col_names)) {
      # Only reachable with a lazy cmp missing its precomputed boolean:
      # the documented contract requires it (deriving it here would need
      # the raw columns, absent from the slim lazy table)
      stop(sprintf(
        "equality boolean column '%s' missing from cmp (a lazy cmp must carry the precomputed __eq columns)",
        eq_col
      ), call. = FALSE)
    }
    agent <- agent %>%
      col_vals_equal(columns = all_of(eq_col), value = TRUE, na_pass = FALSE)
  }

  for (col_nm in tol_cols) {
    ok_col <- datadiff_ok_col(col_nm)
    agent <- agent %>% col_vals_equal(columns = all_of(ok_col), value = TRUE, na_pass = FALSE)
  }

  # Add row count validation if needed
  # Use colnames() instead of names() to work correctly with lazy tables
  if (!is.null(row_validation_info) && isTRUE(row_validation_info$check_count) && "row_count_ok" %in% col_names) {
    agent <- agent %>% col_vals_equal(columns = all_of("row_count_ok"), value = TRUE, na_pass = FALSE)
  }

  agent
}

# Name of the reserved column carrying a structural failure step. The step
# asserts this column does NOT exist, so it fails on any table, empty or not,
# local or lazy. Length-guarded like datadiff_ok_col(): paste0 with
# character(0) would yield the bare prefix, a phantom column name.
datadiff_structural_col <- function(col, prefix) {
  if (length(col) == 0) {
    return(character(0))
  }
  paste0(prefix, col)
}
