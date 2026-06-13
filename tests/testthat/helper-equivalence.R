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

# Sorted character vector of "<column>@<key1|key2|...>" for every failing cell
# surfaced by get_data_extracts().
failing_cells <- function(res, key) {
  rep <- res$reponse
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
    col <- underlying_col(vs$column[[i]])
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
run_compare <- function(ref, cand, key,
                        numeric_abs = 0.101, integer_abs = 0L,
                        na_equal_default = TRUE, check_count_default = TRUE) {
  tmp <- tempfile(fileext = ".yml")
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
  list(
    all_passed = res$all_passed,
    cells      = failing_cells(res, key = key),
    missing    = res$missing_in_candidate,
    extra      = res$extra_in_candidate,
    res        = res
  )
}
