#' Normalize text for comparison
#'
#' Applies text normalization transformations including case conversion and whitespace trimming.
#' Non-character inputs are returned unchanged.
#'
#' @param x Vector to normalize (typically character)
#' @param case_insensitive Logical indicating whether to convert to lowercase
#' @param trim Logical indicating whether to trim leading/trailing whitespace
#' @return Normalized vector of the same type as input
#' @examples
#' normalize_text(c("  Hello ", "WORLD  "), case_insensitive = TRUE, trim = TRUE)
#' # Returns: c("hello", "world")
#' @export
normalize_text <- function(x, case_insensitive = FALSE, trim = FALSE) {
  if (!is.character(x)) {
    return(x)
  }
  if (trim) {
    x <- trimws(x)
  }
  if (case_insensitive) {
    x <- tolower(x = x)
  }
  x
}

#' Preprocess dataframe according to column rules
#'
#' Applies preprocessing transformations to dataframe columns based on validation rules,
#' such as text normalization for character columns. Supports both local data.frames
#' and lazy tables (tbl_lazy) via dplyr::mutate().
#'
#' @param df A dataframe or lazy table to preprocess
#' @param col_rules A list of column-specific rules from derive_column_rules()
#' @param schema Optional local data.frame with 0 rows used to determine column
#'   types when \code{df} is a lazy table. Obtained via
#'   \code{dplyr::collect(utils::head(df, 0L))}.
#' @return Preprocessed dataframe (or lazy table) with transformations applied
#' @examples
#' df <- data.frame(text_col = c("  HELLO  ", "world"))
#' rules <- list(text_col = list(equal_mode = "normalized", case_insensitive = TRUE, trim = TRUE))
#' preprocess_dataframe(df, rules)
#' @importFrom rlang :=
#' @export
preprocess_dataframe <- function(df, col_rules, schema = NULL) {
  out <- df
  for (nm in names(col_rules)) {
    cr <- col_rules[[nm]]
    eq_mode <- cr$equal_mode %||% "exact"
    case_insensitive <- isTRUE(cr$case_insensitive)
    trim <- isTRUE(cr$trim)

    # Apply normalization if equal_mode is "normalized" OR if case_insensitive/trim is TRUE
    should_normalize <- identical(eq_mode, "normalized") || case_insensitive || trim

    # Determine column type: use schema for lazy tables, otherwise inspect df directly
    is_char <- if (!is.null(schema)) {
      is.character(schema[[nm]])
    } else {
      is.character(out[[nm]])
    }

    if (is_char && should_normalize) {
      if (is_non_local(out)) {
        # Non-local path: build transformations via dplyr::mutate()
        nm_sym <- dplyr::sym(nm)
        if (trim) {
          if (is_arrow(out)) {
            # Arrow does not support trimws(); use stringr::str_trim() instead
            if (!requireNamespace("stringr", quietly = TRUE)) {
              stop("Package 'stringr' is required for trim on Arrow objects.")
            }
            out <- dplyr::mutate(out, !!nm := stringr::str_trim(!!nm_sym))
          } else {
            # SQL path: trimws() -> SQL TRIM() (translated by dbplyr)
            out <- dplyr::mutate(out, !!nm := trimws(!!nm_sym))
          }
        }
        if (case_insensitive) {
          out <- dplyr::mutate(out, !!nm := tolower(!!nm_sym))
        }
      } else {
        out[[nm]] <- normalize_text(
          out[[nm]],
          case_insensitive = case_insensitive,
          trim = trim
        )
      }
    }
  }
  out
}
