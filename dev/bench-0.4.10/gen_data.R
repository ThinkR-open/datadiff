# Genere les fichiers Parquet des scenarios lazy du banc CRAN 0.5.0 vs PR #58.
# Usage : Rscript gen_data.R <data_dir>
# Tables : 4M lignes x 125 colonnes (id + 74 DOUBLE + 30 INTEGER + 20 VARCHAR),
# le cas cite dans la PR (#22). La table candidate "fail" differe de la
# reference sur 5 colonnes, 40 lignes chacune (id %% 100000 == 7).

args <- commandArgs(trailingOnly = TRUE)
data_dir <- args[[1]]
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

suppressPackageStartupMessages({
  library(duckdb)
})

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

n_rows <- 4e6

dbl_cols <- sprintf("round(random() * 1000, 4)::DOUBLE AS d%03d", 1:74)
int_cols <- sprintf("(random() * 100000)::INTEGER AS i%03d", 1:30)
chr_cols <- sprintf(
  "('code_' || ((range * %d) %% 5000)::VARCHAR) AS s%03d",
  seq(7, by = 2, length.out = 20),
  1:20
)

sql_ref <- sprintf(
  "CREATE TABLE ref AS SELECT range AS id, %s FROM range(%d)",
  paste(c(dbl_cols, int_cols, chr_cols), collapse = ", "),
  n_rows
)

message("Generation table reference 4M x 125...")
DBI::dbExecute(con, sql_ref)

ref_path <- file.path(data_dir, "ref.parquet")
DBI::dbExecute(
  con,
  sprintf("COPY ref TO '%s' (FORMAT PARQUET)", gsub("'", "''", ref_path))
)

# Candidate "fail" : 5 colonnes touchees sur les lignes id %% 100000 == 7
# (40 lignes par colonne, 200 cellules en echec au total).
message("Generation table candidate (fail)...")
cand_path <- file.path(data_dir, "cand_fail.parquet")
DBI::dbExecute(con, sprintf(
  paste(
    "COPY (SELECT * REPLACE (",
    "  CASE WHEN id %% 100000 = 7 THEN d001 + 1 ELSE d001 END AS d001,",
    "  CASE WHEN id %% 100000 = 7 THEN d002 + 1 ELSE d002 END AS d002,",
    "  CASE WHEN id %% 100000 = 7 THEN i001 + 1 ELSE i001 END AS i001,",
    "  CASE WHEN id %% 100000 = 7 THEN s001 || '_X' ELSE s001 END AS s001,",
    "  CASE WHEN id %% 100000 = 7 THEN s002 || '_X' ELSE s002 END AS s002",
    ") FROM ref) TO '%s' (FORMAT PARQUET)"
  ),
  gsub("'", "''", cand_path)
))

message("OK : ", ref_path, " et ", cand_path)
