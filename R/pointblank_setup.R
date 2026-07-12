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
                                   add_col_exists_steps = TRUE) {
  if (!is.null(cols_reference)) {
    warning(
      "The 'cols_reference' argument of setup_pointblank_agent() is deprecated and unused; it will be removed in a future release.",
      call. = FALSE
    )
  }

  # Add dummy columns for missing columns BEFORE creating the agent
  # These columns are set to FALSE and we'll check they equal TRUE (will fail)
  for (col_nm in missing_in_candidate) {
    dummy_col <- paste0(datadiff_prefix_missing_col, col_nm)
    if (is_non_local(cmp)) {
      cmp <- dplyr::mutate(cmp, !!dummy_col := FALSE)
    } else {
      cmp[[dummy_col]] <- FALSE
    }
  }

  # Add dummy FALSE columns for type-mismatched columns.
  # These will generate a dedicated failing validation step per column.
  for (col_nm in type_mismatch_cols) {
    dummy_col <- paste0(datadiff_prefix_type_mismatch, col_nm)
    if (is_non_local(cmp)) {
      cmp <- dplyr::mutate(cmp, !!dummy_col := FALSE)
    } else {
      cmp[[dummy_col]] <- FALSE
    }
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

  # For missing columns, add a validation that will fail
  for (col_nm in missing_in_candidate) {
    dummy_col <- paste0(datadiff_prefix_missing_col, col_nm)
    agent <- agent %>%
      col_vals_equal(
        columns = all_of(dummy_col),
        value = TRUE,
        na_pass = FALSE,
        label = paste("col_exists:", col_nm)
      )
  }

  # For type-mismatched columns, add a validation that will always fail.
  for (col_nm in type_mismatch_cols) {
    dummy_col <- paste0(datadiff_prefix_type_mismatch, col_nm)
    agent <- agent %>%
      col_vals_equal(
        columns = all_of(dummy_col),
        value = TRUE,
        na_pass = FALSE,
        label = paste("type_mismatch:", col_nm)
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
