# Benchmark for issue #4: green comparison of a WIDE table, RAM and lazy.
# Run: Rscript dev/bench/perf_issue4.R
#
# Two gabarits:
#   1. issue-4 synthetic (300 cols x 200000 rows, all-finite):
#        before (0.4.7):  RAM ~13.8s ; lazy ~64.5s
#        after  (0.4.8):  RAM ~5.2s  ; lazy ~9.8s
#   2. Lyon worst case (cf. dev/dimensions_tdc_lyon.md): ~500 cols x ~300000
#      rows, ~100% numeric doubles rounded to 2 decimals, NA present.
#      The NA variant matters: compute_tolerance_ok only takes the fast 3-pass
#      branch when a column is fully finite; a single NA in a column falls back
#      to the full compute_tolerance_col path, so NA-heavy tables erode the RAM
#      gain column by column. We bench both finite and NA-laced to bracket it.
# Levers: anyDuplicated key check (RAM), fast-path tolerance arithmetic (RAM),
# templated-SQL booleans bypassing per-column dplyr::mutate (lazy).

suppressMessages(devtools::load_all(".", quiet = TRUE))

make_wide <- function(n_rows = 200000L, n_cols = 300L, seed = 1L, na_frac = 0) {
  set.seed(seed)
  df <- data.frame(id = seq_len(n_rows))
  for (k in seq_len(n_cols)) {
    col <- round(rnorm(n_rows, 100, 10), 2)
    if (na_frac > 0) {
      # Spread NA across every column so the fast-path falls back per column,
      # mirroring Lyon TDC columns that still carry NA before coalesce-to-0.
      idx <- seq.int(k %% max(1L, as.integer(1 / na_frac)) + 1L, n_rows,
                     by = max(1L, as.integer(1 / na_frac)))
      col[idx] <- NA_real_
    }
    df[[paste0("m", k)]] <- col
  }
  df
}

bench_case <- function(label, n_rows, n_cols, na_frac = 0) {
  df    <- make_wide(n_rows = n_rows, n_cols = n_cols, na_frac = na_frac)
  rules <- tempfile(fileext = ".yaml")
  write_rules_template(df, key = "id", numeric_abs = 0.101, integer_abs = 0L,
                       na_equal_default = TRUE, path = rules)

  t_ram <- system.time(
    r1 <- suppressMessages(compare_datasets_from_yaml(df, df, path = rules, key = "id"))
  )[["elapsed"]]
  cat(sprintf("[%s] RAM  %d cols x %d rows (green, na_frac=%.3f): %6.2fs (all_passed=%s)\n",
              label, n_cols, n_rows, na_frac, t_ram, r1$all_passed))

  if (requireNamespace("duckdb", quietly = TRUE) &&
      requireNamespace("dbplyr", quietly = TRUE)) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    duckdb::dbWriteTable(con, "ref", df)
    duckdb::dbWriteTable(con, "cand", df)
    t_lazy <- system.time(
      r2 <- suppressMessages(compare_datasets_from_yaml(
        dplyr::tbl(con, "ref"), dplyr::tbl(con, "cand"), key = "id", path = rules))
    )[["elapsed"]]
    cat(sprintf("[%s] LAZY %d cols x %d rows (green, na_frac=%.3f): %6.2fs (all_passed=%s)\n",
                label, n_cols, n_rows, na_frac, t_lazy, r2$all_passed))
  }
}

# 1. Original issue-4 synthetic case (regression guard).
bench_case("issue4", n_rows = 200000L, n_cols = 300L)

# 2. Lyon worst case, all-finite (best case for the fast-path).
bench_case("lyon-finite", n_rows = 300000L, n_cols = 500L)

# 3. Lyon worst case, NA-laced (~1% NA per column -> fast-path falls back).
bench_case("lyon-na", n_rows = 300000L, n_cols = 500L, na_frac = 0.01)
