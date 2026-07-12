# Execute UN scenario du banc dans un processus R frais et ecrit une ligne
# JSON (temps, verdict, memoire R) dans le fichier de sortie.
# Usage : Rscript bench_one.R <libdir> <scenario> <out_jsonl> <data_dir>
# Scenarios : local_green | local_na | local_fail | lazy_green | lazy_fail

args <- commandArgs(trailingOnly = TRUE)
libdir <- args[[1]]
scenario <- args[[2]]
out_file <- args[[3]]
data_dir <- args[[4]]

.libPaths(new = c(libdir, .libPaths()))
suppressPackageStartupMessages({
  library(datadiff, lib.loc = libdir)
  library(dplyr)
})
ver <- as.character(utils::packageVersion("datadiff"))

n_rows_local <- 200000L
n_cols_local <- 300L

make_local_ref <- function() {
  set.seed(4242)
  m <- matrix(
    data = rnorm(n_rows_local * n_cols_local),
    nrow = n_rows_local,
    ncol = n_cols_local
  )
  df <- as.data.frame(m)
  names(df) <- sprintf("c%03d", seq_len(n_cols_local))
  df
}

result <- NULL
con <- NULL

if (scenario %in% c("local_green", "local_na", "local_fail")) {
  ref <- make_local_ref()
  cand <- ref
  if (scenario == "local_na") {
    # NA (sans Inf) sur les 150 premieres colonnes, ~5 % des cellules,
    # aux memes positions des deux cotes : cible le fast-path #23.
    set.seed(777)
    for (j in seq_len(150)) {
      idx <- sample.int(n_rows_local, size = n_rows_local %/% 20)
      ref[[j]][idx] <- NA_real_
    }
    cand <- ref
  }
  if (scenario == "local_fail") {
    set.seed(777)
    for (j in seq_len(5)) {
      idx <- sample.int(n_rows_local, size = 1000)
      cand[[j]][idx] <- cand[[j]][idx] + 1
    }
  }

  gc(reset = TRUE)
  tm <- system.time({
    res <- compare_datasets_from_yaml(
      data_reference = ref,
      data_candidate = cand
    )
  })
} else {
  suppressPackageStartupMessages(library(duckdb))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ref_path <- gsub("'", "''", file.path(data_dir, "ref.parquet"))
  cand_file <- if (scenario == "lazy_green") {
    "ref.parquet"
  } else {
    "cand_fail.parquet"
  }
  cand_path <- gsub("'", "''", file.path(data_dir, cand_file))
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW ref_v AS SELECT * FROM read_parquet('%s')", ref_path
  ))
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW cand_v AS SELECT * FROM read_parquet('%s')", cand_path
  ))
  ref <- tbl(con, "ref_v")
  cand <- tbl(con, "cand_v")

  gc(reset = TRUE)
  tm <- system.time({
    res <- compare_datasets_from_yaml(
      data_reference = ref,
      data_candidate = cand,
      key = "id"
    )
  })
}

g <- gc()
mem_mb <- sum(g[, "max used"] * c(56, 8)) / 1e6

all_passed <- tryCatch(isTRUE(res$all_passed), error = function(e) {
  NA
})

line <- sprintf(
  paste0(
    '{"version":"%s","lib":"%s","scenario":"%s",',
    '"elapsed":%.3f,"user":%.3f,"sys":%.3f,',
    '"all_passed":%s,"r_peak_mb":%.0f}'
  ),
  ver, basename(libdir), scenario,
  tm[["elapsed"]], tm[["user.self"]], tm[["sys.self"]],
  tolower(as.character(all_passed)), mem_mb
)
cat(line, "\n", sep = "", file = out_file, append = TRUE)
cat(line, "\n", sep = "")
