#' Per-column tolerance kernel
#'
#' Computes, for one candidate/reference numeric vector pair, the absolute
#' difference, the tolerance threshold, and the boolean within-tolerance
#' result. Factored out of \code{add_tolerance_columns} so the comparison logic
#' lives in a single tested place and can be applied column-by-column: this
#' keeps the working set cache-resident (unlike a whole-block matrix kernel,
#' which becomes memory-bandwidth bound on tall inputs).
#'
#' Semantics: same-sign infinities compare equal (zero difference); a
#' single-sided \code{NA}/\code{NaN}/\code{Inf} always fails; \code{NA} or
#' \code{NaN} on both sides follows \code{na_equal}; and a few ULPs of the
#' reference magnitude are added to the threshold to absorb IEEE 754 rounding in
#' the subtraction.
#'
#' @param cand_vals Numeric vector of candidate values.
#' @param ref_vals Numeric vector of reference values, same length as
#'   \code{cand_vals}.
#' @param abs_tol Scalar absolute tolerance.
#' @param rel_tol Scalar relative tolerance.
#' @param na_equal Logical; whether NA/NaN on both sides count as equal.
#' @return A list with numeric vectors \code{absdiff}, \code{thresh} and the
#'   logical vector \code{ok}.
#' @noRd
compute_tolerance_col <- function(cand_vals, ref_vals, abs_tol, rel_tol, na_equal) {
  both_na  <- is.na(cand_vals) & is.na(ref_vals)
  one_na   <- is.na(cand_vals) | is.na(ref_vals)
  both_inf <- is.infinite(cand_vals) & is.infinite(ref_vals)
  same_inf <- both_inf & (sign(cand_vals) == sign(ref_vals))
  both_nan <- is.nan(cand_vals) & is.nan(ref_vals)
  one_nan  <- is.nan(cand_vals) | is.nan(ref_vals)
  one_inf  <- is.infinite(cand_vals) | is.infinite(ref_vals)

  absdiff <- abs(cand_vals - ref_vals)
  absdiff[same_inf] <- 0

  thresh <- abs_tol + rel_tol * abs(ref_vals)

  fp_correction <- 8 * .Machine$double.eps * abs(ref_vals)
  fp_correction[!is.finite(fp_correction)] <- 0
  within_tol <- absdiff <= thresh + fp_correction

  if (na_equal) {
    ok <- both_na | both_nan | same_inf |
      (!one_na & !one_nan & !one_inf & within_tol)
  } else {
    ok <- same_inf | (!one_na & !one_nan & !one_inf & within_tol)
  }

  list(absdiff = absdiff, thresh = thresh, ok = ok)
}

#' Within-tolerance boolean for one column (hot path, ok only)
#'
#' Returns ONLY the boolean within-tolerance vector (not absdiff/thresh), with a
#' fast path for the common case of a column with no NA/NaN/Inf: the dozen
#' special-value passes of `compute_tolerance_col()` are skipped and the result
#' reduces to `abs(cand - ref) <= thresh + fp`. Falls back to the full kernel
#' (taking only its `$ok`) when special values are present, so the result is
#' identical to `compute_tolerance_col(...)$ok` in every case.
#'
#' @inheritParams compute_tolerance_col
#' @return Logical vector, the same as `compute_tolerance_col(...)$ok`.
#' @noRd
compute_tolerance_ok <- function(cand_vals, ref_vals, abs_tol, rel_tol, na_equal) {
  if (!anyNA(cand_vals) && !anyNA(ref_vals) &&
      all(is.finite(cand_vals)) && all(is.finite(ref_vals))) {
    thresh <- abs_tol + rel_tol * abs(ref_vals)
    fp     <- 8 * .Machine$double.eps * abs(ref_vals)
    return(abs(cand_vals - ref_vals) <= thresh + fp)
  }
  compute_tolerance_col(cand_vals, ref_vals, abs_tol, rel_tol, na_equal)$ok
}

#' Add only the `<col>__ok` tolerance columns (hot path)
#'
#' Like [add_tolerance_columns()] but materialises only the boolean `<col>__ok`
#' columns - which alone determine the verdict - in a single bind. The
#' `<col>__absdiff` / `<col>__thresh` diagnostic columns are not produced (they
#' are not read by any verdict logic nor surfaced in the failing-row extracts).
#'
#' @inheritParams add_tolerance_columns
#' @return `cmp` with the `<col>__ok` columns appended.
#' @noRd
add_ok_columns <- function(cmp, tol_cols, col_rules, ref_suffix, na_equal) {
  if (length(tol_cols) == 0) {
    return(cmp)
  }
  ok_cols <- vector("list", length(tol_cols))
  for (i in seq_along(tol_cols)) {
    c <- tol_cols[i]
    ok_cols[[i]] <- compute_tolerance_ok(
      cand_vals = cmp[[c]], ref_vals = cmp[[paste0(c, ref_suffix)]],
      abs_tol = col_rules[[c]][["abs"]] %||% 0,
      rel_tol = col_rules[[c]][["rel"]] %||% 0,
      na_equal = na_equal
    )
  }
  names(ok_cols) <- paste0(tol_cols, "__ok")
  cbind(cmp, list2DF(ok_cols))
}

#' Add `<col>__ok` / `<col>__eq` boolean columns to a lazy table via one SQL SELECT
#'
#' On the lazy path, building the per-column tolerance/equality booleans with
#' `dplyr::mutate()` is O(expressions) on the R side (dbplyr query construction
#' and SQL rendering dominate; e.g. ~60 s of 64 s for 300 columns). This builds
#' the same boolean columns in a single templated SQL `SELECT`, leaving DuckDB to
#' execute. The CASE WHEN logic reproduces exactly the `dplyr::case_when` used
#' previously (NULL handling and IEEE 754 fp correction inlined), so the lazy
#' verdict is unchanged.
#'
#' @param cmp A lazy table (the join of candidate and reference).
#' @param tol_cols,eq_cols Tolerance / equality column names.
#' @param col_rules Per-column rules (abs / rel).
#' @param ref_suffix Reference-column suffix.
#' @param na_equal Logical; NA equality semantics.
#' @return A lazy table with the `<col>__ok` / `<col>__eq` columns added.
#' @noRd
add_bool_cols_sql <- function(cmp, tol_cols, eq_cols, col_rules, ref_suffix, na_equal) {
  if (length(tol_cols) == 0 && length(eq_cols) == 0) {
    return(cmp)
  }
  con <- dbplyr::remote_con(cmp)
  sub <- dbplyr::sql_render(cmp)
  q   <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  num <- function(x) sprintf("%.17g", x)
  fp_eps <- 8 * .Machine$double.eps

  case_bool <- function(cc, rc, cond) {
    if (na_equal) {
      sprintf(paste0("CASE WHEN %s IS NULL AND %s IS NULL THEN TRUE ",
                     "WHEN %s IS NULL OR %s IS NULL THEN FALSE ",
                     "WHEN %s THEN TRUE ELSE FALSE END"),
              cc, rc, cc, rc, cond)
    } else {
      sprintf(paste0("CASE WHEN %s IS NULL OR %s IS NULL THEN FALSE ",
                     "WHEN %s THEN TRUE ELSE FALSE END"),
              cc, rc, cond)
    }
  }

  tol_exprs <- vapply(X = tol_cols, FUN = function(c) {
    cc <- q(c)
    rc <- q(paste0(c, ref_suffix))
    at <- col_rules[[c]][["abs"]] %||% 0
    rt <- col_rules[[c]][["rel"]] %||% 0
    within <- sprintf("ABS(%s - %s) <= (%s + %s * ABS(%s)) + %s * ABS(%s)",
                      cc, rc, num(at), num(rt), rc, num(fp_eps), rc)
    sprintf("%s AS %s", case_bool(cc, rc, within), q(paste0(c, "__ok")))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  eq_exprs <- vapply(X = eq_cols, FUN = function(c) {
    cc <- q(c)
    rc <- q(paste0(c, ref_suffix))
    sprintf("%s AS %s",
            case_bool(cc, rc, sprintf("%s = %s", cc, rc)),
            q(paste0(c, "__eq")))
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  exprs <- c(tol_exprs, eq_exprs)

  select_sql <- sprintf("SELECT *, %s FROM (%s) AS %s",
                        paste(exprs, collapse = ", "), sub, q("datadiff_bool"))
  dplyr::tbl(con, dbplyr::sql(select_sql))
}

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

  # Local data.frame path: full Inf/NaN handling preserved.
  # Compute the __absdiff/__thresh/__ok columns per column (cache-friendly
  # vector ops) into pre-allocated lists, then bind them all in a single
  # operation. The original code grew the (already wide) data.frame one column
  # at a time, which copies the whole frame on each assignment and is quadratic
  # in the column count; the single bind removes that without materialising any
  # large intermediate matrices that would be memory-bound on tall inputs.
  if (length(tol_cols) == 0) {
    return(cmp)
  }

  m <- length(tol_cols)
  absdiff_cols <- vector("list", m)
  thresh_cols  <- vector("list", m)
  ok_cols      <- vector("list", m)

  for (i in seq_len(m)) {
    c <- tol_cols[i]
    blocks <- compute_tolerance_col(
      cand_vals = cmp[[c]], ref_vals = cmp[[paste0(c, ref_suffix)]],
      abs_tol = col_rules[[c]][["abs"]] %||% 0,
      rel_tol = col_rules[[c]][["rel"]] %||% 0,
      na_equal = na_equal
    )
    absdiff_cols[[i]] <- blocks$absdiff
    thresh_cols[[i]]  <- blocks$thresh
    ok_cols[[i]]      <- blocks$ok
  }

  names(absdiff_cols) <- paste0(tol_cols, "__absdiff")
  names(thresh_cols)  <- paste0(tol_cols, "__thresh")
  names(ok_cols)      <- paste0(tol_cols, "__ok")

  cbind(cmp, list2DF(c(absdiff_cols, thresh_cols, ok_cols)))
}
