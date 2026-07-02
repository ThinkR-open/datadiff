#' Operator for default values
#'
#' Returns y if x is NULL, otherwise returns x
#' @name or_operator
#' @param x Value to check
#' @param y Default value to return if x is NULL
#' @return x if not NULL, otherwise y
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

get_col_names <- function(x) {
  nms <- colnames(x)
  if (length(nms) == 0) names(x) else nms
}

is_non_local <- function(x) {
  inherits(x, "tbl_lazy") || inherits(x, c("ArrowObject", "arrow_dplyr_query"))
}

# Package-local mutable state (temp-table counter).
.datadiff_state <- new.env(parent = emptyenv())

# Unique temp-table name: per-process counter + PID, so that two calls in the
# same session never collide (a clock-derived name could repeat within the
# same millisecond) and two R processes sharing a database cannot either.
datadiff_tmp_table_name <- function() {
  counter <- get0("tmp_tbl_counter", envir = .datadiff_state, ifnotfound = 0L) + 1L
  assign("tmp_tbl_counter", value = counter, envir = .datadiff_state)
  sprintf("datadiff_tmp_%d_%d", Sys.getpid(), counter)
}

is_arrow <- function(x) {
  inherits(x, c("ArrowObject", "arrow_dplyr_query"))
}

# Convert an Arrow dataset to a DuckDB tbl_lazy on `con`.
#
# For file-backed Parquet datasets, uses DuckDB's native read_parquet() so
# that all memory is managed by DuckDB's buffer pool (enabling proper disk
# spilling).  Falls back to arrow::to_duckdb() for non-file-backed datasets
# or non-Parquet formats.
arrow_dataset_to_duckdb <- function(ds, con, tbl_name) {
  files <- tryCatch(ds$files, error = function(e) {
    NULL
  })
  if (!is.null(files) && length(files) > 0 &&
      all(grepl("\\.parquet$", files, ignore.case = TRUE))) {
    paths_sql <- paste0("'", gsub("\\\\", "/", files), "'", collapse = ", ")
    DBI::dbExecute(con, paste0(
      "CREATE OR REPLACE TEMP TABLE \"", tbl_name, "\" AS ",
      "SELECT * FROM read_parquet([", paths_sql, "])"
    ))
    return(dplyr::tbl(con, tbl_name))
  }
  arrow::to_duckdb(ds, con = con)
}
