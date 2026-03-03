#' Detect column types in a dataframe
#'
#' Analyzes each column of a dataframe to determine its data type
#' (integer, numeric, date, datetime, logical, or character)
#'
#' @param data A dataframe or tibble
#' @return A named character vector with column names as names and types as values
#' @examples
#' df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3), c = c("x", "y", "z"))
#' detect_column_types(df)
#' @export
detect_column_types <- function(data) {
  sapply(data, function(x) {
    if (is.integer(x)) {
      "integer"
    } else if (is.numeric(x)) {
      "numeric"
    } else if (inherits(x, "Date")) {
      "date"
    } else if (inherits(x, "POSIXt")) {
      "datetime"
    } else if (is.logical(x)) {
      "logical"
    } else {
      "character"
    }
  })
}

#' Derive column-specific validation rules
#'
#' Combines type-based and column-specific validation rules into a unified
#' list of rules for each column in the dataset.
#'
#' @param data_reference Reference dataframe used to determine column types
#' @param rules List of validation rules from read_rules()
#' @return List of column-specific rules with type and name-based rules merged
#' @importFrom utils modifyList
#' @examples
#' df <- data.frame(a = 1:3, b = c(1.1, 2.2, 3.3))
#' rules <- list(by_type = list(numeric = list(abs = 0.1)), by_name = list(a = list(abs = 0.5)))
#' derive_column_rules(df, rules)
#' @export
derive_column_rules <- function(data_reference, rules) {
  types <- detect_column_types(data_reference)
  res <- vector("list", length(types))
  names(res) <- names(types)
  for (nm in names(types)) {
    type_rule <- rules$by_type[[types[[nm]]]] %||% list()
    name_rule <- rules$by_name[[nm]] %||% list()
    res[[nm]] <- modifyList(x = type_rule, val = name_rule, keep.null = TRUE)
  }
  res
}
