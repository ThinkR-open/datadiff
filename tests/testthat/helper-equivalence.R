# Design-independent capture of the verdict an enc.mco-style caller relies on:
#   - all_passed (the boolean verdict)
#   - the set of failing cells, recovered exactly the way the caller does:
#     pointblank::get_data_extracts() + the validation_set column mapping.
#
# A cell is identified by (underlying data column, key tuple). This is stable
# across any internal re-routing of pointblank steps, so the optimized code
# must reproduce these sets byte-for-byte.

# Underlying data column for a validation step (strip the __ok/__eq markers).
underlying_col <- function(col) {
  sub("__(ok|eq)$", "", col)
}

# Named integer vector: for every failing validation step, the underlying
# data column mapped to its number of failing rows. This is the shape both
# backends can promise: lazy extracts carry the boolean check columns only
# (no key columns, no data values), so per-cell identification stays a
# local-path capability.
failing_row_counts <- function(res) {
  rep <- res$response
  if (is.null(rep)) {
    return(integer(0))
  }
  ex <- pointblank::get_data_extracts(rep)
  if (length(ex) == 0) {
    return(integer(0))
  }
  vs <- rep$validation_set
  cols <- vapply(X = names(ex), FUN = function(nm) {
    i <- as.integer(nm)
    underlying_col(vs$column[[match(i, vs$i)]][1])
  }, FUN.VALUE = character(1))
  counts <- vapply(X = ex, FUN = function(df) {
    nrow(as.data.frame(df))
  }, FUN.VALUE = integer(1))
  names(counts) <- unname(cols)
  counts[order(names(counts))]
}

# Sorted character vector of "<column>@<key1|key2|...>" for every failing cell
# surfaced by get_data_extracts().
failing_cells <- function(res, key) {
  rep <- res$response
  if (is.null(rep)) {
    return(character(0))
  }
  ex <- pointblank::get_data_extracts(rep)
  if (length(ex) == 0) {
    return(character(0))
  }
  vs <- rep$validation_set
  out <- character(0)
  for (nm in names(ex)) {
    i  <- as.integer(nm)
    col <- underlying_col(vs$column[[match(i, vs$i)]][1])
    df  <- as.data.frame(ex[[nm]])
    if (nrow(df) == 0) {
      next
    }
    keyvals <- apply(df[, key, drop = FALSE], 1L, function(r) {
      paste(r, collapse = "|")
    })
    out <- c(out, paste0(col, "@", keyvals))
  }
  sort(unique(out))
}

# Run a comparison with explicit tolerances, returning the captured verdict.
# The same comparison also runs on the lazy backend (DuckDB) and both backends
# must agree on the verdict, the exact failing cells and the column
# bookkeeping; set compare_lazy = FALSE for a case that only makes sense
# locally. The returned verdict is the local one.
run_compare <- function(ref, cand, key,
                        numeric_abs = 0.101, integer_abs = 0L,
                        na_equal_default = TRUE, check_count_default = TRUE,
                        compare_lazy = TRUE) {
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  write_rules_template(
    ref,
    key                 = key,
    numeric_abs         = numeric_abs,
    integer_abs         = integer_abs,
    na_equal_default    = na_equal_default,
    check_count_default = check_count_default,
    path                = tmp
  )
  res <- suppressWarnings(suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = key, path = tmp)
  ))
  out <- list(
    all_passed = res$all_passed,
    cells      = failing_cells(res, key = key),
    missing    = res$missing_in_candidate,
    extra      = res$extra_in_candidate,
    res        = res
  )
  if (isTRUE(compare_lazy) &&
      requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("duckdb", quietly = TRUE) &&
      requireNamespace("dbplyr", quietly = TRUE)) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    duckdb::duckdb_register(con, "ref_equiv", ref)
    duckdb::duckdb_register(con, "cand_equiv", cand)
    res_lazy <- suppressWarnings(suppressMessages(
      compare_datasets_from_yaml(
        dplyr::tbl(con, "ref_equiv"),
        dplyr::tbl(con, "cand_equiv"),
        key = key,
        path = tmp
      )
    ))
    testthat::expect_identical(
      res_lazy$all_passed, out$all_passed,
      info = "lazy backend must agree on the verdict"
    )
    testthat::expect_identical(
      failing_row_counts(res_lazy), failing_row_counts(res),
      info = "lazy backend must agree on the failing columns and their failing-row counts"
    )
    testthat::expect_identical(
      res_lazy$missing_in_candidate, out$missing,
      info = "lazy backend must agree on the missing columns"
    )
    testthat::expect_identical(
      res_lazy$extra_in_candidate, out$extra,
      info = "lazy backend must agree on the extra columns"
    )
  }
  out
}
