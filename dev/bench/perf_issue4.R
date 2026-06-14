# Benchmark for issue #4: green comparison of a WIDE table, RAM and lazy.
# Run: Rscript dev/bench/perf_issue4.R
#
# Measured on 300 numeric columns x 200000 identical rows (all-pass):
#   before (0.4.7):  RAM ~13.8s ; lazy ~64.5s
#   after  (0.4.8):  RAM ~5.2s  ; lazy ~9.8s
# Levers: anyDuplicated key check (RAM), fast-path tolerance arithmetic (RAM),
# templated-SQL booleans bypassing per-column dplyr::mutate (lazy).

suppressMessages(devtools::load_all(".", quiet = TRUE))

make_wide <- function(n_rows = 200000L, n_cols = 300L, seed = 1L) {
  set.seed(seed)
  df <- data.frame(id = seq_len(n_rows))
  for (k in seq_len(n_cols)) {
    df[[paste0("m", k)]] <- round(rnorm(n_rows, 100, 10), 2)
  }
  df
}

bench_issue4 <- function(n_rows = 200000L, n_cols = 300L) {
  df    <- make_wide(n_rows, n_cols)
  rules <- tempfile(fileext = ".yaml")
  write_rules_template(df, key = "id", numeric_abs = 0.101, integer_abs = 0L,
                       path = rules)

  t_ram <- system.time(
    r1 <- suppressMessages(compare_datasets_from_yaml(df, df, path = rules, key = "id"))
  )[["elapsed"]]
  cat(sprintf("RAM  %d cols x %d rows (green): %6.2fs (all_passed=%s)\n",
              n_cols, n_rows, t_ram, r1$all_passed))

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
    cat(sprintf("LAZY %d cols x %d rows (green): %6.2fs (all_passed=%s)\n",
                n_cols, n_rows, t_lazy, r2$all_passed))
  }
}

bench_issue4()
