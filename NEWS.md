# datadiff 0.4.4

## CRAN submission preparation

* Replace `\(x)` lambda syntax with `function(x)` throughout to maintain
  compatibility with the declared `R (>= 4.0.0)` dependency (`\()` requires
  R >= 4.1.0).

* All test files: replace hardcoded YAML file paths with `tempfile()` +
  `on.exit(unlink())` to comply with the CRAN policy prohibiting tests from
  writing outside `tempdir()`.

* `compare_datasets_from_yaml()`: change default language from `"fr"` /
  `"fr_FR"` to `"en"` / `"en_US"` for international use.

* `compare_datasets_from_yaml()`: the `error_msg_no_key` parameter is now
  used in the message emitted when row counts differ without keys (was
  previously a dead parameter).

* `write_rules_template()`: fix `@return` documentation (`invisible(path)`,
  not `invisible(NULL)`); fix default label prefix from `"comparaison"` to
  `"comparison"`; fix `@param` documentation for `warn_at` and `stop_at`
  (actual default is `1e-14`, not `0.01`).

* `%||%` is no longer exported to avoid namespace conflicts with `rlang`.

* `DESCRIPTION`: add `URL` and `BugReports` fields; add
  `Config/testthat/edition: 3`.

# datadiff 0.4.3

## Bug fixes

* Fix inconsistent tolerance comparison due to IEEE 754 floating-point
  rounding. When `cand = ref + threshold` mathematically, the subtraction
  `cand - ref` can exceed the threshold by a few ULPs (e.g.
  `100.01 - 100.00 = 0.0100000000000051 > 0.01` in double precision),
  causing a false validation failure. Fixed by adding a correction of
  `8 * .Machine$double.eps * |ref|` to the threshold before comparing — a
  value proportional to the operand magnitude that absorbs floating-point
  representation error without meaningfully widening the user-specified
  tolerance. The correction is applied in both the local data.frame path
  and the lazy SQL path.

## Documentation

* README: add a full `by_name` example showing column-level rule overrides
  (per-column absolute tolerance, mixed abs+rel, case-insensitive character
  comparison) and a table explaining how `by_name` takes precedence over
  `by_type`.

# datadiff 0.4.2

## Bug fixes

* Fix crash (`sign(cand_vals): non-numeric argument to a mathematical function`)
  when a column is numeric in the reference dataset but stored as character in
  the candidate. The tolerance arithmetic (`sign()`, `abs()`, subtraction) was
  applied unconditionally to all numeric reference columns, regardless of the
  candidate column type.

  The fix detects type mismatches between reference and candidate at the start
  of `compare_datasets_from_yaml()` and:
  - Emits a warning listing each mismatched column with its reference and
    candidate types (e.g. `'year' (reference: numeric, candidate: character)`).
  - Excludes mismatched columns from tolerance and equality comparison to avoid
    the crash and prevent silent pass-through due to R's type coercion in `==`.
  - Adds a dedicated failing validation step (labelled `type_mismatch: <column>`)
    to the pointblank report for each mismatched column, so the mismatch is
    clearly surfaced in the validation output.

* `setup_pointblank_agent()` gains a `type_mismatch_cols` parameter (character
  vector) to receive the list of columns with incompatible types between
  reference and candidate.

# datadiff 0.4.1

## Bug fixes

* Fix out-of-memory crash when comparing large Parquet files (e.g. two 4 GB
  files with 125 numeric columns × 4 M rows). Root causes and fixes:

  - **Arrow virtual-scanner memory is untracked by DuckDB.** `arrow::to_duckdb()`
    registers an Arrow-format virtual table whose read buffers are allocated
    *outside* DuckDB's memory manager. Combined Arrow + DuckDB memory could
    exceed physical RAM before DuckDB's spilling threshold was reached, causing
    an `Out of Memory Error: Allocation failure`. Fixed by materialising
    file-backed Parquet datasets with DuckDB's native `read_parquet()` instead
    (`CREATE TEMP TABLE … AS SELECT * FROM read_parquet([…])`), so all memory
    is tracked and disk-spilling works end-to-end. Non-file-backed or
    non-Parquet Arrow inputs continue to use `arrow::to_duckdb()` as a fallback.

  - **DuckDB's default memory cap (80 % of total RAM) left no headroom** for R,
    Arrow, and OS memory. Fixed by adding an explicit `SET memory_limit` after
    opening the DuckDB connection, defaulting to `"8GB"`. This forces
    aggressive spilling well before system RAM is exhausted.

  - **`is_tbl_mssql(logical(0))` crash in pointblank.** After materialisation
    via `dplyr::compute()`, pointblank's reporting code inspected the connection
    metadata of the lazy table and received `character(0)`, which propagated to
    `if(logical(0))`. Fixed by `collect()`-ing the slim boolean table (~125
    logical columns) after `compute()` so that pointblank receives a plain
    `data.frame` — no live DuckDB connection required during interrogation.

## New parameters

* `compare_datasets_from_yaml()` gains `duckdb_memory_limit` (default `"8GB"`):
  the `SET memory_limit` value passed to the user-managed DuckDB connection
  when Arrow datasets are used. Raise it on machines with ample free RAM to
  reduce disk I/O; lower it on memory-constrained machines. Has no effect for
  `data.frame` or `tbl_lazy` inputs.

# datadiff 0.4.0

## New features

* Add Arrow/Parquet support. `compare_datasets_from_yaml()` and
  `write_rules_template()` now accept Arrow Tables
  (`arrow::read_parquet(as_data_frame=FALSE)`) and Arrow Datasets
  (`arrow::open_dataset()`) in addition to data.frames and lazy tables.
  Key design decisions:
  - Arrow objects are converted to DuckDB-backed lazy tables via
    `arrow::to_duckdb()` before pointblank validation, keeping the entire
    pipeline lazy. No full `collect()` is performed — even very large
    Parquet files that don't fit in RAM can be validated.
  - Tolerance columns (`__ok`), equality columns (`__eq`), and text
    preprocessing are computed via `dplyr::mutate()` on the Arrow side
    before the DuckDB handoff.
  - Text trimming on Arrow uses `stringr::str_trim()` (Arrow does not
    support `trimws()`).
  - Positional comparison (no key) still requires `collect()` since
    neither Arrow nor DuckDB guarantee row order.
  - arrow, duckdb, and stringr are added as `Suggests`.

# datadiff 0.3.0

## New features

* Add lazy table support (`tbl_lazy` / dbplyr). `compare_datasets_from_yaml()`
  and `write_rules_template()` now accept SQL-backed lazy tables in addition to
  local data.frames. Key changes:
  - Type detection uses a 0-row `collect(head(x, 0L))` schema (no data fetched).
  - Duplicate key detection uses SQL-native `GROUP BY` + `COUNT`.
  - Tolerance columns (`__absdiff`, `__thresh`, `__ok`) and equality columns
    (`__eq`) are computed via `dplyr::mutate()` + `case_when()` for SQL
    translation.
  - Text preprocessing (`trimws()`, `tolower()`) is translated to SQL `TRIM()`
    and `LOWER()` by dbplyr.
  - Positional comparison (no key) falls back to `collect()` with a user-facing
    message, since SQL does not guarantee row order.
  - DBI, dbplyr, and RSQLite are added as `Suggests`.

* Add `requireNamespace("dbplyr")` guard when lazy tables are passed, with a
  clear error message if the package is missing.

# datadiff 0.2.1

## Bug fixes

* Change default value of `numeric_rel` in `write_rules_template()` from
  `0.000000001` to `0`. Relative tolerance is now disabled by default, which
  avoids unintended failures when comparing values near zero.

# datadiff 0.2.0

## New features

* Validation now fails when columns present in the reference dataset are missing
  from the candidate dataset. Previously, missing columns were silently ignored.
  A dedicated failing step (labelled `col_exists: <column>`) is added to the
  pointblank report for each absent column.

* `setup_pointblank_agent()` gains a `missing_in_candidate` parameter
  (character vector) to receive the list of columns absent from the candidate.

## Bug fixes

* Change default value of `check_count_default` in `write_rules_template()`
  from `FALSE` to `TRUE`. Row-count validation is now enabled by default in
  generated YAML templates.

* Fix assignment of `row_count_ok` when the joined comparison dataframe is
  empty (zero rows), which previously caused an error.

# datadiff 0.1.6

## New features

* Add memory optimization parameters to limit output size for large datasets.
  New parameters in `compare_datasets_from_yaml()`:
  - `extract_failed`: Control whether to collect failed rows (default: TRUE).
    Set to FALSE for lightweight validation without row extraction.
  - `get_first_n`: Limit to first n failed rows per validation step.
  - `sample_n`: Randomly sample n failed rows per validation step.
  - `sample_frac`: Sample a fraction (0-1) of failed rows.
  - `sample_limit`: Maximum rows when using sample_frac (default: 5000).

* Add multilingual support for pointblank reports. New `lang` and `locale`
  parameters in `compare_datasets_from_yaml()` and `setup_pointblank_agent()`.
  French remains the default for backwards compatibility. Supported languages
  include: en, fr, de, es, pt, it, zh, ja, ru.

# datadiff 0.1.5

## New features

* Make YAML rules file optional with automatic default rules (#7602523). The
 `path` parameter in `compare_datasets_from_yaml()` is now optional. When NULL,
  default rules are auto-generated based on the reference dataset structure.

* Make `key` parameter optional in `write_rules_template()`. NULL means
  positional comparison (row by row).

## Bug fixes

* Fix critical edge cases for Inf/NaN handling (#79b9fc6):
  - `Inf == Inf` (same sign) now correctly returns TRUE

  - `NaN == NaN` respects the `na_equal` setting
  - Avoid NaN results from `Inf - Inf` calculations

* Add input validation for `compare_datasets_from_yaml()`:
  - Validate that `data_reference` and `data_candidate` are data.frames
  - Validate that `path` exists and is a single string

* Add duplicate keys detection with detailed warning message showing examples
  of duplicate key values.

* Add reserved suffix conflict detection with warning when column names contain
  the reference suffix (default: `__reference`).

# datadiff 0.1.0

* Initial release with core functionality:
  - YAML-based validation rules configuration
  - Tolerance-based numeric comparisons (absolute and relative)
  - Text normalization (case-insensitive, trim whitespace)
  - Row count validation
  - Column existence checks
  - Integration with pointblank for validation reporting
