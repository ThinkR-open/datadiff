lazy_nrow <- function(x) {
  if (is_non_local(x)) {
    dplyr::pull(dplyr::collect(dplyr::count(x)), n)
  } else {
    nrow(x)
  }
}

#' Validate row counts according to rules
#'
#' Checks if the number of rows in candidate data matches expectations defined in rules.
#' Can validate against a fixed expected count or against reference data count with tolerance.
#' Returns validation information that can be used by pointblank agents.
#'
#' @param data_reference_p Preprocessed reference dataframe
#' @param data_candidate_p Preprocessed candidate dataframe
#' @param rules List of validation rules containing row_validation settings
#' @return A list with validation information for pointblank integration
#' @examples
#' ref <- data.frame(a = 1:3)
#' cand <- data.frame(a = 1:3)
#' rules <- list(row_validation = list(check_count = TRUE, expected_count = 3, tolerance = 0))
#' validate_row_counts(ref, cand, rules)
#' @export
validate_row_counts <- function(data_reference_p, data_candidate_p, rules) {
  list(
    check_count = isTRUE(rules$row_validation$check_count),
    ref_count = lazy_nrow(data_reference_p),
    cand_count = lazy_nrow(data_candidate_p),
    expected_count = rules$row_validation$expected_count,
    tolerance = rules$row_validation$tolerance %||% 0
  )
}

#' Analyze column differences between datasets
#'
#' Compares column names between reference and candidate datasets to identify
#' common columns, missing columns, and extra columns. Can ignore specified columns.
#'
#' @param data_reference Reference dataframe
#' @param data_candidate Candidate dataframe to compare
#' @param ignore_columns Character vector of column names to ignore during comparison
#' @return A list containing:
#'   \item{cols_reference}{Column names in reference data (excluding ignored)}
#'   \item{cols_candidate}{Column names in candidate data (excluding ignored)}
#'   \item{missing_in_candidate}{Columns present in reference but missing in candidate}
#'   \item{extra_in_candidate}{Columns present in candidate but not in reference}
#'   \item{common_cols}{Columns present in both datasets}
#'   \item{ignored_cols}{Columns that were ignored from comparison}
#' @examples
#' ref <- data.frame(a = 1, b = 2, c = 3, d = 4)
#' cand <- data.frame(a = 1, b = 2, c = 3, e = 5)
#' analyze_columns(ref, cand, ignore_columns = c("d", "e"))
#' @export
analyze_columns <- function(data_reference, data_candidate, ignore_columns = character(0)) {
  # Filter out ignored columns
  cols_reference <- setdiff(get_col_names(data_reference), ignore_columns)
  cols_candidate <- setdiff(get_col_names(data_candidate), ignore_columns)

  missing_in_candidate <- setdiff(x = cols_reference, y = cols_candidate)
  extra_in_candidate <- setdiff(x = cols_candidate, y = cols_reference)
  common_cols <- intersect(x = cols_reference, y = cols_candidate)

  list(
    cols_reference = cols_reference,
    cols_candidate = cols_candidate,
    missing_in_candidate = missing_in_candidate,
    extra_in_candidate = extra_in_candidate,
    common_cols = common_cols,
    ignored_cols = ignore_columns
  )
}
