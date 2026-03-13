#' Add tolerance columns for numeric comparisons
#'
#' Creates additional columns in the comparison dataframe to handle numeric tolerance validation.
#' For each numeric column with tolerance rules, adds absolute difference and threshold columns,
#' plus a boolean column indicating if values are within acceptable tolerance.
#'
#' @param cmp Comparison dataframe containing candidate and reference columns
#' @param tol_cols Character vector of column names that have tolerance rules
#' @param col_rules List of column-specific validation rules
#' @param ref_suffix Suffix used for reference columns (default: "__reference")
#' @param na_equal Logical indicating if NA values should be considered equal
#' @return Modified dataframe with added tolerance columns
#' @examples
#' cmp <- data.frame(value = c(1.1, 2.2), value__reference = c(1.0, 2.0))
#' rules <- list(value = list(abs = 0.2, rel = 0.1))
#' add_tolerance_columns(cmp, "value", rules, "__reference", TRUE)
#' @export
add_tolerance_columns <- function(cmp, tol_cols, col_rules, ref_suffix, na_equal) {
  if (is_non_local(cmp)) {
    # Lazy / SQL path: batch ALL column mutations into exactly 2 mutate() calls
    # (one for __absdiff + __thresh, one for __ok) regardless of how many columns
    # there are. Each individual mutate() adds one lazy_query node to the dbplyr
    # plan; with 100+ columns the nesting would exceed R's expression stack limit.
    if (length(tol_cols) == 0) return(cmp)

    exprs_diff <- list()
    exprs_ok   <- list()

    for (c in tol_cols) {
      reference_c <- paste0(c, ref_suffix)
      abs_tol     <- col_rules[[c]][["abs"]] %||% 0
      rel_tol     <- col_rules[[c]][["rel"]] %||% 0
      c_sym       <- dplyr::sym(c)
      rc_sym      <- dplyr::sym(reference_c)
      absdiff_col <- paste0(c, "__absdiff")
      thresh_col  <- paste0(c, "__thresh")
      ok_col      <- paste0(c, "__ok")
      absdiff_sym <- dplyr::sym(absdiff_col)
      thresh_sym  <- dplyr::sym(thresh_col)

      exprs_diff[[absdiff_col]] <- rlang::expr(abs(!!c_sym - !!rc_sym))
      exprs_diff[[thresh_col]]  <- rlang::expr(!!abs_tol + !!rel_tol * abs(!!rc_sym))

      # IEEE 754: floating-point subtraction introduces rounding errors
      # proportional to the magnitude of the operands, not to the threshold.
      # e.g. 100.01 - 100.00 = 0.0100000000000051 > 0.01 in double precision.
      # Adding a few ULPs of the reference magnitude absorbs this error without
      # meaningfully widening the user-specified tolerance.
      fp_eps <- 8 * .Machine$double.eps
      if (na_equal) {
        exprs_ok[[ok_col]] <- rlang::expr(dplyr::case_when(
          is.na(!!c_sym) & is.na(!!rc_sym) ~ TRUE,
          is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
          !!absdiff_sym <= !!thresh_sym + !!fp_eps * abs(!!rc_sym) ~ TRUE,
          .default = FALSE
        ))
      } else {
        exprs_ok[[ok_col]] <- rlang::expr(dplyr::case_when(
          is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
          !!absdiff_sym <= !!thresh_sym + !!fp_eps * abs(!!rc_sym) ~ TRUE,
          .default = FALSE
        ))
      }
    }

    cmp <- dplyr::mutate(cmp, !!!exprs_diff)
    cmp <- dplyr::mutate(cmp, !!!exprs_ok)
    return(cmp)
  }

  # Local data.frame path: full Inf/NaN handling preserved
  for (c in tol_cols) {
    reference_c <- paste0(c, ref_suffix)
    abs_tol <- col_rules[[c]][["abs"]] %||% 0
    rel_tol <- col_rules[[c]][["rel"]] %||% 0

    cand_vals <- cmp[[c]]
    ref_vals <- cmp[[reference_c]]

    # Detect special values: Inf, -Inf, NaN
    both_na <- is.na(cand_vals) & is.na(ref_vals)
    one_na <- is.na(cand_vals) | is.na(ref_vals)
    both_inf <- is.infinite(cand_vals) & is.infinite(ref_vals)
    same_inf <- both_inf & (sign(cand_vals) == sign(ref_vals))
    both_nan <- is.nan(cand_vals) & is.nan(ref_vals)
    one_nan <- is.nan(cand_vals) | is.nan(ref_vals)
    one_inf <- is.infinite(cand_vals) | is.infinite(ref_vals)

    # Calculate absolute difference (handle Inf - Inf = NaN)
    absdiff <- abs(cand_vals - ref_vals)
    # For identical Inf values (same sign), difference is 0
    absdiff[same_inf] <- 0

    cmp[[paste0(c, "__absdiff")]] <- absdiff
    cmp[[paste0(c, "__thresh")]] <- abs_tol + rel_tol * abs(ref_vals)

    ok_col <- paste0(c, "__ok")

    # Base comparison: within tolerance.
    # A few ULPs of the reference magnitude are added to absorb IEEE 754
    # rounding errors in the subtraction (e.g. 100.01 - 100.00 gives
    # 0.0100000000000051 > 0.01 in double precision). The correction is
    # proportional to |ref|, zeroed out for Inf/NaN inputs.
    fp_correction <- 8 * .Machine$double.eps * abs(ref_vals)
    fp_correction[!is.finite(fp_correction)] <- 0
    within_tol <- absdiff <= cmp[[paste0(c, "__thresh")]] + fp_correction

    if (na_equal) {
      cmp[[ok_col]] <- both_na | both_nan | same_inf |
        (!one_na & !one_nan & !one_inf & within_tol)
    } else {
      cmp[[ok_col]] <- same_inf | (!one_na & !one_nan & !one_inf & within_tol)
    }
  }
  cmp
}
