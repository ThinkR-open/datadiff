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
    x <- tolower(x)
  }
  x
}

#' Preprocess dataframe according to column rules
#'
#' Applies preprocessing transformations to dataframe columns based on validation rules,
#' such as text normalization for character columns. Supports both local data.frames
#' and lazy tables (tbl_lazy) via dplyr::mutate().
#'
#' On a local data.frame, every factor column (including key and non-compared
#' columns) is first converted to character: factors are compared as the
#' character values they display, so the text normalization rules apply to
#' them and factors with differing level sets compare cleanly.
#'
#' @param df A dataframe or lazy table to preprocess
#' @param col_rules A list of column-specific rules from derive_column_rules()
#' @param schema Optional local data.frame with 0 rows used to determine column
#'   types when \code{df} is a lazy table. Obtained via
#'   \code{dplyr::collect(utils::head(df, 0L))}.
#' @return Preprocessed dataframe (or lazy table) with transformations applied.
#'   Factor columns of a local data.frame come back as character vectors.
#' @examples
#' df <- data.frame(text_col = c("  HELLO  ", "world"))
#' rules <- list(text_col = list(equal_mode = "normalized", case_insensitive = TRUE, trim = TRUE))
#' preprocess_dataframe(df, rules)
#' @importFrom rlang :=
#' @export
preprocess_dataframe <- function(df, col_rules, schema = NULL) {
  out <- df
  # Factors are classed "character" by detect_column_types() and receive the
  # character rules, but == on factors with differing level sets errors and
  # normalize_text() skips non-character vectors. Compare them as the
  # character values they display. SQL lazy tables convert factors to varchar
  # upstream; Arrow dictionary columns stay lazy and are handled by the
  # is.factor(schema[[nm]]) branch of the is_char predicate below.
  if (!is_non_local(out)) {
    for (nm in names(out)) {
      if (is.factor(out[[nm]])) {
        out[[nm]] <- as.character(out[[nm]])
      }
    }
  }
  lazy_exprs <- list()
  for (nm in names(col_rules)) {
    cr <- col_rules[[nm]]
    normalized <- identical(cr$equal_mode %||% "exact", "normalized")

    # equal_mode "normalized" implies BOTH text normalizations unless the
    # rule sets them explicitly (an explicit FALSE wins over the mode)
    case_insensitive <- if (is.null(cr$case_insensitive)) {
      normalized
    } else {
      isTRUE(cr$case_insensitive)
    }
    trim <- if (is.null(cr$trim)) {
      normalized
    } else {
      isTRUE(cr$trim)
    }

    should_normalize <- case_insensitive || trim

    # Determine column type: use schema for lazy tables, otherwise inspect df
    # directly. A factor in the schema counts as character: the local values
    # were converted above and the character rules apply to it.
    is_char <- if (!is.null(schema)) {
      is.character(schema[[nm]]) || is.factor(schema[[nm]])
    } else {
      is.character(out[[nm]])
    }

    if (is_char && should_normalize) {
      if (is_non_local(out)) {
        # Non-local path: compose ONE expression per column and accumulate;
        # a single mutate() at the end keeps the dbplyr query-construction
        # cost O(1) instead of one or two mutate() layers per column
        expr <- dplyr::sym(nm)
        if (trim) {
          if (is_arrow(out)) {
            # Arrow does not support trimws(); use stringr::str_trim() instead
            if (!requireNamespace("stringr", quietly = TRUE)) {
              stop("Package 'stringr' is required for trim on Arrow objects.")
            }
            expr <- rlang::expr(stringr::str_trim(!!expr))
          } else {
            # SQL path: trimws() -> SQL TRIM() (translated by dbplyr)
            expr <- rlang::expr(trimws(!!expr))
          }
        }
        if (case_insensitive) {
          expr <- rlang::expr(tolower(!!expr))
        }
        lazy_exprs[[nm]] <- expr
      } else {
        out[[nm]] <- normalize_text(
          out[[nm]],
          case_insensitive = case_insensitive,
          trim = trim
        )
      }
    }
  }
  if (length(lazy_exprs) > 0) {
    out <- dplyr::mutate(out, !!!lazy_exprs)
  }
  out
}
