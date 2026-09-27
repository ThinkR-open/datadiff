# datadiff (development version)

## Bug fixes

* A tolerance rule (`abs` / `rel`) declared on a column that is not numeric in
  the reference was silently ignored: the column fell back to an exact string
  comparison while `applied_rules` still listed `abs` and `rel`, so the report
  suggested a comparison by value that never happened. datadiff never converts
  data, so the rule cannot be honoured: such a column now gets a dedicated,
  always failing check, `tolerance_on_non_numeric: <column>` (visible in the
  agent and in `coverage`, like `type_mismatch`), a warning is raised when it
  is detected, the column takes part in no other check, and `applied_rules`
  no longer lists the rule that was not applied. `build_coverage()` and
  `setup_pointblank_agent()` gain a `tolerance_non_numeric_cols` argument
  (default empty, behaviour unchanged otherwise) (issue #60).

# datadiff 0.6.0

## New features

* `datadiff_report_html()` gains an `extracts_dir` argument: the failing-row
  extracts are also written as plain CSV files (one per failing step). The
  CSV buttons inside the HTML report are `data:` URI downloads, which some
  viewers block silently (Positron / Posit Workbench webview): files on disk
  are the robust alternative (issue #10).

## Performance

* The lazy verdict no longer loads the boolean table into R: the per-column
  (n, n_failed) counts come from ONE SQL aggregate scan over the computed
  slim table (`SUM(CASE WHEN ok THEN 0 ELSE 1 END)`, counting FALSE and NULL
  alike, exactly the local reducers' semantics), and only the FAILING
  columns' booleans are collected for the pointblank agent. A green lazy
  comparison previously collected N x columns logicals (~2 GB of R memory
  for 4M rows x 125 columns, 32x the figure the code comment promised); it
  now keeps O(columns) in R, and `res$response` carries a constant-size
  placeholder instead of N collected rows (issue #22).

* Four avoidable scans and transfers are gone from the lazy path (issue #24):
  the two systematic `COUNT(*)` full scans are skipped when nothing consumes
  them (keyed comparison with `check_count: false`; `validate_row_counts()`
  gains a `count_rows` parameter, default unchanged); the 0-row schema is
  collected once per table and reused by every consumer including the
  auto-generated template (one DB roundtrip saved when `path = NULL`); the
  keyed join only carries the key and the compared columns, so ignored and
  one-sided columns no longer travel through the join into the agent (the
  failing-row extracts consequently no longer show ignored columns); and the
  lazy duplicate-key detection aggregates in SQL, transferring 2 scalars plus
  at most 3 example groups instead of every duplicated group.

* The verdict, the coverage and the failing-column sets now come from a
  SINGLE pass over the boolean validation columns: `build_coverage()` runs
  first and everything else derives from it (its rows already carry the
  structural checks). Previously the same booleans were scanned twice on a
  green comparison and three times on a red one, and each scan recomputed the
  local equality booleans from scratch. The now-unused scanners
  (`all_validations_pass()`, `failing_columns()`, the `*_col_passes()`
  predicates) are removed. Verdicts are byte-identical (equivalence guard
  green); measured ~6% end to end on a green 200-column x 200k comparison
  (4.35 s to 4.07 s), the join and preprocessing dominating the rest
  (issue #21).

* `compute_tolerance_ok()` gains an intermediate fast path for the most common
  real-data case: a column with NA but no infinity no longer falls back to the
  full special-value kernel. NaN needs no dedicated handling there (`is.na()`
  covers NaN, so NaN follows the NA rules identically on both paths); only
  infinities reroute to the kernel. Bit-identical results, locked by tests;
  measured ~2.6x faster per NA-bearing column (200k rows, 0.008 s vs 0.020 s).
  The full kernel itself drops its redundant NaN masks (`both_nan`/`one_nan`
  are subsets of the NA masks), with unchanged results (issue #23).

## Robustness

* Report construction hardening (all internal to `R/report.R`):
  `datadiff_report_html()` now shares the `print()` memoization (it reads the
  cached report and feeds the cache, instead of rebuilding agent + report on
  every call); a genuine evaluation error on the real agent (`eval_error`,
  where `n_failed` is NA) is propagated to the report instead of being
  silently replaced by the synthetic all-pass branch and forced to FALSE; a
  dead per-row count-injection block was removed (its fields were entirely
  overwritten by the vectorised coverage block); and the `__ok`/`__eq`/
  `__missing_col_`/`__type_mismatch_` naming conventions plus the default
  warn/stop levels are now defined once in `R/constants.R` and shared by the
  producers, the verdict consumers and the report mapping, so a one-sided
  rename can no longer silently break the step mapping (issue #29).

* `arrow_dataset_to_duckdb()` builds its SQL safely: Parquet file paths are
  quoted with `DBI::dbQuoteString()` (a path containing a quote, common in
  French like `l'export/`, broke the query with a raw SQL syntax error), the
  temp table name goes through `dbQuoteIdentifier()`, and `read_parquet()`
  gets `union_by_name = true` so multi-file datasets bind columns by NAME
  like the `arrow::to_duckdb()` fallback always did. `duckdb_memory_limit`
  is validated as a size literal before being interpolated into `SET`, and
  the `SET temp_directory` path is quoted too (issue #30).

* A missing `__ok`/`__eq` boolean validation column now raises an explicit
  internal error instead of silently reading as an all-pass (`all(NULL)` is
  `TRUE`, and a dropped column previously produced a PASS row with `n = 0` in
  the coverage: the worst failure mode for a non-regression tool). The lazy
  slim-table projection uses `all_of()` (loud) instead of `any_of()` (silent
  drop); posing that guard immediately surfaced a latent phantom name in the
  projection (`paste0(character(0), "__ok")` yields `"__ok"`) that `any_of()`
  had been eating since 0.4.8 (issue #17).

## Chores

* Test-suite cleanup (issue #31): the byte-for-byte duplicated
  "uses key parameter over YAML rules" block is gone; every test that wrote
  a fixed-name YAML into the working directory now uses `tempfile()` +
  `on.exit(unlink())` (CRAN policy, and what NEWS 0.4.4 already claimed);
  the orphan committed fixtures `tests/testthat/rules.yaml` and `test.yaml`
  (referenced by no test) are removed; the unrunnable
  `test-huge-parquet.R` (skip-everywhere + hardcoded Windows paths) moves to
  `dev/manual-tests/`; the silent conditional assertions of
  `test-extraction-params.R` (`if (length(extracts) > 0) expect_...`) assert
  their precondition first, so a cap check can no longer pass vacuously; and
  the small internal helpers (`format_key_examples()`, `get_col_names()`,
  `is_non_local()`/`is_arrow()`, the `file = NULL` branch of
  `datadiff_report_html()`) get direct unit tests. The larger redundancy
  compression the issue sketches (IEEE 754 in 7 copies, duplicate-key tests
  in 3 files) is deliberately left for a dedicated pass: it is high-churn
  refactoring of green tests.

* Packaging and code cleanup (issue #34): `dev/` is excluded from the tarball
  (`.Rbuildignore`); the orphan `inst/templates/rules_template.yaml`
  (referenced nowhere) is gone; unused `@importFrom` entries dropped
  (`dplyr::arrange`/`across`, `stats::setNames` replaced by a base
  construction); the `numeric_abs` default reads `1e-9` instead of a
  9-zero decimal literal; `is_non_local()` reuses `is_arrow()` instead of
  duplicating the Arrow class list; `abs(ref_vals)` and the IEEE fp constant
  are hoisted out of the kernels' expressions and loops;
  `tol_col_counts()` counts failures without allocating intermediate
  vectors; `format_key_examples()` truncates to 3 groups before formatting;
  the lazy `preprocess_dataframe()` composes ONE `mutate()` for all
  normalized columns instead of one or two layers per column (the dbplyr
  query-construction anti-pattern eliminated elsewhere in 0.4.8); loop
  variables no longer shadow `base::c`; the tolerance kernel documents the
  integer-overflow caveat. Deliberately not changed: `print()` of a report
  builds the gt object in non-interactive sessions too (printing IS the
  render), and the `DBI` availability guard stays transitive via `duckdb`.

## Documentation and input friction

* Doc-vs-code drift resolved (issue #32): `@return` of
  `compare_datasets_from_yaml()` now lists all 8 elements including
  `all_passed` (the first one) and describes `agent` truthfully (configured,
  NOT interrogated - `response` is); the README Quick Start shows the
  zero-configuration mode first and passes `key` explicitly everywhere; the
  README "Dev part" no longer embeds check/coverage outputs that go stale
  (it froze results from 0.4.2); the vignette documents the local-only
  restriction of the `__absdiff`/`__thresh` extract diagnostics and the
  DuckDB-only NaN caveat of the lazy booleans.

* `read_rules()` speaks to the person who edits the YAML by hand: an
  unsupported `version` gets an explicit error naming the file, the declared
  and the supported versions (was a raw `stopifnot` output), and unknown
  top-level or `defaults` fields (typos like `by_nmae:` or `no_equal:`) get a
  warning naming them and the known fields, instead of being silently
  ignored. `write_rules_template()` rejects a `version` other than 1 upfront
  (it happily wrote version 2 templates that `read_rules()` then refused).

## Bug fixes

* Three NEWS-vs-code contradictions introduced by the "Perf (#1)" commit after
  the 0.4.4 release are resolved in the direction 0.4.4 had announced or the
  docs now tell the truth: `%||%` is internal again (exporting it masked
  rlang's and base R's own operator at load time; the test that locked the
  accidental re-export now locks the opposite), the default rules-template
  label prefix is "comparison" (was still the French "comparaison"), and the
  vignette states the real report language default (`lang = "fr"`, it claimed
  English). The three exported helpers that no user-facing doc mentioned
  (`normalize_text()`, `validate_row_counts()`, `setup_pointblank_agent()`)
  are now documented in the vignette's utility-functions section with real
  use cases (issue #26).

* `setup_pointblank_agent()` cleanup: the `cols_reference` argument was never
  read (verified by grep since its introduction) and is now deprecated
  (warning when supplied, removal planned); the local equality steps validate
  a derived `<col>__eq` boolean with the shared NA semantics instead of
  embedding the whole reference vector in each step (O(rows) per step,
  serialised with the report, and unable to express the one-sided/two-sided
  NA distinction); the roxygen example is now executable and representative
  (it used to target a `__ok` column that did not exist); `get_col_names()`
  is hoisted out of the per-column loop; the dead `cols_reference`/
  `cols_candidate` locals of the caller are gone (issue #25).

* `equal_mode: normalized` now does what the vignette always said: it implies
  `case_insensitive = TRUE` and `trim = TRUE` for character columns unless
  those flags are set explicitly (an explicit value wins over the mode). It
  previously activated a branch that applied the identity transformation: a
  user writing `equal_mode: normalized` alone got nothing, silently, and a
  test even locked that inertia. `write_rules_template()` stops co-writing
  the default FALSE flags next to a "normalized" mode (they neutralised the
  implication). Note: a hand-written YAML using `equal_mode: normalized`
  without explicit flags now normalizes where it previously compared exact,
  so verdicts on such configs can flip from FAIL to PASS; re-run rather than
  assume stability. The `date`/`datetime`/`logical` equal-mode parameters are
  documented as accepted-but-inert for non-character types (text
  normalization only touches character columns) (issue #27).

* Factor columns are now compared as the character values they display:
  preprocessing converts them, so the text normalization rules
  (`case_insensitive`, `trim`) apply to them and the verdict no longer depends
  on `stringsAsFactors` or haven-style imports. This also fixes an opaque
  error when reference and candidate factors carried different level sets
  ("level sets of factors are different"). Factor key columns join correctly
  (issue #28).

* The lazy SQL booleans now reproduce the R tolerance kernel's NaN/Inf
  semantics: same-sign infinities pass, a one-sided NA/NaN/Inf fails, NaN on
  both sides follows `na_equal`. The previous CASE WHEN only handled SQL NULL,
  and NaN is a regular float in DuckDB (`NaN = NaN` is true, NaN sorts above
  everything): Parquet data containing NaN or infinities could silently get
  the opposite verdict on the lazy path (false PASS on one-sided NaN/Inf,
  false FAIL on matching infinities). NaN detection uses `isnan()` on DuckDB;
  backends without NaN storage (SQLite) keep the NULL rules, and infinity
  detection falls back to a `> DBL_MAX` comparison there. Numeric equality
  (non-tolerance) columns get the same NaN-aware NA rules, matching the local
  path. The new equivalence tests use the R kernel as oracle on a full
  NaN/Inf/NA grid, on DuckDB and SQLite (issue #13).

* Failing-row extracts again include the explicit measured deviations
  (issue #11). Since the 0.4.8 performance work, `compare_datasets_from_yaml()`
  only materialised the boolean `<col>__ok` verdict columns, so
  `pointblank::get_data_extracts()`, the HTML report and its CSV download
  showed the candidate and reference values but no longer the measured gap.
  The `<col>__absdiff` (measured absolute deviation) and `<col>__thresh`
  (applied threshold) columns are now recomputed on the failure path for the
  **failing** tolerance columns only, restoring the <= 0.4.7 extract content
  at a cost proportional to the failing columns - the all-pass fast path and
  the passing majority of columns are untouched, so the 0.4.8 speedups are
  preserved. The lazy (SQL) path is unchanged: its extracts are built from the
  slim boolean table and never carried these diagnostics.

* The lazy path no longer leaks its internal temp table on a user-supplied
  connection: the slim boolean table is dropped at the end of each call (the
  Arrow path already closed its private connection). Its name is now derived
  from a per-process counter plus the PID instead of the wall clock, removing
  a collision window for two calls in the same millisecond (issue #19).

* Lazy comparisons now work on tables containing a column named `n`. The
  duplicate-key detection used `dplyr::count()` with its default output name:
  with a key column named `n`, the filter and the aggregates silently read the
  key values instead of the counts, producing a wrong duplicate diagnosis.
  Both lazy helpers now use a reserved count name, and the
  `globalVariables("n")` declaration that masked the pattern is gone
  (issue #18).

* Argument vs YAML resolution is now deterministic and documented: explicit
  argument > YAML `defaults` > built-in default, for both `key` and `label`.
  Previously the `label` argument was silently overwritten by the YAML label
  (or the built-in default) whenever `path` was provided, and the YAML `keys`
  field was only reached through `$` partial matching on a singular `key`
  lookup, an accident waiting to break. `keys` is now read explicitly, with a
  legacy singular `key` field honored as fallback; when both are present,
  `keys` (the canonical field written by `write_rules_template()`) wins
  (issue #20).

* A positional (key-less) comparison of datasets with different row counts now
  raises a single clear error built from `error_msg_no_key` and mentioning both
  row counts. It previously emitted `error_msg_no_key` as an informational
  message and then crashed on an opaque internal column assignment
  ("replacement has X rows, data has Y"). The error is raised before any
  materialisation of non-local tables (the counts are already known), and the
  collect guard now covers both sides: a positional comparison mixing a local
  reference with a lazy candidate (or vice versa) previously skipped the
  candidate collection and failed downstream. On the valid positional path,
  the reference columns are now appended with a single bind instead of a
  per-column assignment loop (issue #15).

## Breaking changes

The baseline for these entries is datadiff 0.5.0, the version on CRAN
(functionally identical to 0.4.9: 0.5.0 was a consolidation release with no
behaviour change).

* The comparison result now exposes the interrogated agent under `response`;
  the historical French name `reponse` is deprecated. The result gains the
  class `datadiff_result`, whose `$` and `[[` accessors keep `reponse`
  readable (with a once-per-session warning) until its removal in a future
  release. Code that iterates over `names(result)` or tests
  `"reponse" %in% names(result)` must switch to `response` now;
  `datadiff_report_html()` still accepts results saved by older versions.

* `%||%` is no longer exported (its export, accidental since 0.4.5, masked
  the operator from base R >= 4.4 and {rlang} at load time). Code calling
  `datadiff::%||%` explicitly must switch to the base R or {rlang} operator;
  code that merely had {datadiff} attached keeps working. See the
  corresponding entry under "Bug fixes" (issue #26).

* The local (data.frame) path now applies the same NA semantics as the lazy
  path and the numeric tolerance kernel to **equality** columns: a one-sided
  NA (a value facing a missing value, including candidate rows with no
  reference match after the key join) is always a difference; a two-sided NA
  follows `na_equal`. Previously, with `na_equal = TRUE` (the default), any NA
  on either side passed on the local path (`na_pass` semantics), while the
  same data failed on the lazy path: identical datasets could get opposite
  verdicts depending on the backend. Comparisons that relied on one-sided NA
  passing locally will now fail; the two-sided NA behavior is unchanged.
  On the local failure path, `pointblank::get_data_extracts()` output for a
  failing equality column now also carries its `<col>__eq` boolean column
  (as the lazy path always did) (issue #14).

* `compare_datasets_from_yaml()` now raises an explicit error when one or more
  key columns are absent from either dataset, naming the missing column(s) and
  the dataset(s) concerned, on every code path (with or without an explicit
  YAML rules file). It previously emitted a vague message
  ("could not find key in both data") and returned a truncated 6-field list
  (no `coverage`, no `summary`) that broke downstream consumers such as
  `datadiff_report_html()` (issue #16).

* `compare_datasets_from_yaml()` rejects an empty (`character(0)`) or
  non-character `key` with a clear error. An empty key previously fell through
  to a keyless cross join (deprecated dplyr behavior producing a cartesian
  product).

# datadiff 0.5.0

* Release consolidating the 0.4.x maintenance series (wide-table performance,
  lazy / Arrow / Parquet support, accurate HTML reports and IEEE 754 tolerance
  handling). No user-facing behaviour changes since 0.4.9; see the entries
  below for the details.

# datadiff 0.4.9

## Bug fixes

* The HTML report of an **all-pass** comparison again shows the per-check
  evaluated row counts (the TBL / EVAL / UNITS / PASS columns). The all-pass
  fast path built a report agent that {pointblank} did not consider interrogated,
  so `get_agent_report()` rendered a bare "no interrogation performed" plan with
  those columns blank, on both the in-memory and the lazy paths. The synthetic
  report agent is now marked as interrogated in constant time (without re-running
  the per-column interrogation) and its validation set is built by replicating a
  single template step, so the report is correct and `build_report_agent()` stays
  fast on wide tables (~0.04 s for 1448 columns). `coverage` / `summary` were
  already correct (issue #6).

# datadiff 0.4.8

## Performance

* `compare_datasets_from_yaml()` is much faster on **wide** tables, including
  all-pass (green) comparisons - the nominal case of non-regression suites.
  Measured on 300 numeric columns x 200 000 identical rows: the in-memory
  (data.frame) path drops from ~13.8 s to ~5.2 s, and the lazy (DuckDB) path
  from ~64.5 s to ~9.8 s. The verdict (`all_passed`) and the extractable failing
  cells are unchanged. Three levers (issue #4):
  - **Duplicate-key check** on local data.frames now uses `anyDuplicated()` /
    `duplicated()` (a single hashed pass) instead of `dplyr::count()` /
    `group_by()`; lazy tables keep the SQL-native count. Same warning message.
  - **Tolerance arithmetic** on the local path computes only the `<col>__ok`
    booleans, with a fast path that skips the special-value handling when a
    column has no `NA`/`NaN`/`Inf`.
  - **Lazy path** builds the `<col>__ok` / `<col>__eq` booleans in a single
    templated SQL `SELECT` instead of one `dplyr::mutate()` expression per
    column, eliminating the dbplyr query-construction cost that dominated wide
    lazy comparisons (verified equivalent on DuckDB and SQLite).

# datadiff 0.4.7

## Bug fixes

* The lazy HTML report now keeps the genuine failure detail. Printing
  `result$reponse` (or calling `datadiff_report_html()`) on a failing
  comparison again shows, for each failing column, the number of failing rows,
  the offending cells, and the downloadable CSV extract - while still listing
  every check performed, with the `col_exists` existence checks and the
  `col_vals_equal` value checks presented as distinct steps. The report is now
  built on top of the real interrogated agent (which holds the extracts) rather
  than a counts-only synthesis, so nothing is lost.

* `build_report_agent()` threads `warn_at` / `stop_at` so the report's
  warn/stop colouring follows the supplied thresholds instead of hard-coded
  values.

## Documentation

* Vignette and README document `coverage` / `summary` and the lazy report, and
  are normalised to ASCII.
* DESCRIPTION quotes software names ('pointblank', 'YAML') per CRAN convention,
  clearing the "Possibly misspelled words" NOTE.
* Internal helpers are marked `@noRd` (documentation kept in source, no `.Rd`).

# datadiff 0.4.6

## New features

* `compare_datasets_from_yaml()` now returns `coverage` and `summary`. The
  `coverage` data.frame is a faithful, instantly computed record of every check
  performed (one row per column with its check type, number of rows, number of
  failures and PASS/FAIL status); `summary` holds the aggregate counts. This
  keeps the verified checks visible even when the all-pass fast path skips the
  per-column {pointblank} agent, so an all-green run no longer looks empty.

* Printing `result$reponse` now lazily renders a full {pointblank}-style report.
  `result$reponse` stays a real interrogated agent (so `pointblank::all_passed()`
  and `pointblank::get_data_extracts()` keep working), but printing it builds the
  per-column report on demand from the pre-computed counts (no re-interrogation)
  and memoizes the result, so the rendering cost is paid only when the report is
  actually displayed.

* New exported `datadiff_report_html()` writes that {pointblank}-style report to
  a standalone HTML file.

# datadiff 0.4.5

## Performance

* `compare_datasets_from_yaml()` is dramatically faster on wide tables. The
  verdict is computed in a single vectorised pass over the precomputed
  comparison booleans; when everything passes, the expensive per-column
  {pointblank} agent (one validation step per column, roughly quadratic in the
  number of columns) is skipped entirely. On a failing comparison, validation
  steps are built only for the columns that actually fail. `all_passed` and the
  extracted failing cells are unchanged.

* `add_tolerance_columns()`: the per-column tolerance computation was rewritten
  to avoid quadratic data.frame growth (the result columns are bound in a single
  operation instead of one assignment per column), with no change to the verdict.

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
  `8 * .Machine$double.eps * |ref|` to the threshold before comparing - a
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
    `data.frame` - no live DuckDB connection required during interrogation.

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
    pipeline lazy. No full `collect()` is performed - even very large
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
