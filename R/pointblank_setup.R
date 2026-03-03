#' Setup pointblank agent for data validation
#'
#' Creates and configures a pointblank validation agent with all necessary validation steps
#' including column existence checks, exact value comparisons, and tolerance validations.
#'
#' @param cmp Comparison dataframe with candidate and reference data
#' @param cols_reference Character vector of reference column names
#' @param common_cols Character vector of columns present in both datasets
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
#' @param add_col_exists_steps Logical indicating whether to add `col_exists` validation
#'   steps for common columns (default: `TRUE`). Set to `FALSE` for the non-local (lazy
#'   table) path where `cmp` only contains pre-computed boolean columns, not the original
#'   data columns.
#' @return Configured pointblank agent ready for interrogation
#' @examples
#' cmp <- data.frame(a = 1:3, a__reference = 1:3, b = c(1.1, 2.2, 3.3),
#'  b__reference = c(1.0, 2.0, 3.0))
#' row_info <- list(check_count = FALSE)
#' setup_pointblank_agent(cmp, c("a", "b"), c("a", "b"), "b", row_info,
#'  "__reference", 0.1, 0.1, "Test", TRUE)
#' @importFrom pointblank create_agent col_exists col_vals_equal action_levels
#' @importFrom tidyselect all_of
#' @importFrom rlang :=
#' @export
setup_pointblank_agent <- function(cmp, cols_reference, common_cols, tol_cols,
                                   row_validation_info = NULL, ref_suffix, warn_at, stop_at, label,
                                   na_equal, lang = "fr", locale = "fr_FR",
                                   missing_in_candidate = character(0),
                                   add_col_exists_steps = TRUE) {
  # Add dummy columns for missing columns BEFORE creating the agent
  # These columns are set to FALSE and we'll check they equal TRUE (will fail)
  for (c in missing_in_candidate) {
    dummy_col <- paste0("__missing_col_", c)
    if (is_non_local(cmp)) {
      cmp <- dplyr::mutate(cmp, !!dummy_col := FALSE)
    } else {
      cmp[[dummy_col]] <- FALSE
    }
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
    for (c in common_cols) {
      agent <- agent %>% col_exists(columns = all_of(c))
    }
  }

  # For missing columns, add a validation that will fail
  for (c in missing_in_candidate) {
    dummy_col <- paste0("__missing_col_", c)
    agent <- agent %>%
      col_vals_equal(
        columns = all_of(dummy_col),
        value = TRUE,
        na_pass = FALSE,
        label = paste("col_exists:", c)
      )
  }

  eq_cols <- setdiff(x = common_cols, y = tol_cols)
  for (c in eq_cols) {
    eq_precomputed <- paste0(c, "__eq")
    if (eq_precomputed %in% get_col_names(cmp)) {
      # Lazy path: use pre-computed boolean equality column
      agent <- agent %>%
        col_vals_equal(columns = all_of(eq_precomputed), value = TRUE, na_pass = FALSE)
    } else {
      # Local path: compare directly against reference column values
      agent <- agent %>%
        col_vals_equal(
          columns = all_of(c),
          value = cmp[[paste0(c, ref_suffix)]],
          na_pass = na_equal
        )
    }
  }

  for (c in tol_cols) {
    ok_col <- paste0(c, "__ok")
    agent <- agent %>% col_vals_equal(columns = all_of(ok_col), value = TRUE, na_pass = FALSE)
  }

  # Add row count validation if needed
  # Use colnames() instead of names() to work correctly with lazy tables
  if (!is.null(row_validation_info) && isTRUE(row_validation_info$check_count) && "row_count_ok" %in% get_col_names(cmp)) {
    agent <- agent %>% col_vals_equal(columns = all_of("row_count_ok"), value = TRUE, na_pass = FALSE)
  }

  agent
}
