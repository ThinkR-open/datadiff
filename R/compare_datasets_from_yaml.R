#' Create a YAML rules template for data validation
#'
#' Generates a comprehensive YAML configuration file with default validation rules
#' based on the structure and types of the reference dataset. The template includes
#' rules for different data types, column-specific rules, and row validation settings.
#'
#' @param data_reference A dataframe or tibble used as reference for rule generation
#' @param key Character vector specifying column name(s) to use as join key(s) for data
#'   comparison. If NULL, comparison is positional (row by row).
#' @param label Descriptive label for the validation report
#' @param path Character string specifying the output YAML file path (default: "rules.yaml")
#' @param version Numeric version of the rules format (default: 1)
#' @param na_equal_default Logical indicating if NA values should be considered equal by default
#' @param ignore_columns_default Character vector of column names to ignore during comparison by default
#' @param check_count_default Logical indicating if row count validation should be enabled by default
#' @param expected_count_default Numeric value specifying expected row count (NULL uses reference count)
#' @param row_count_tolerance_default Numeric tolerance for row count validation
#' @param numeric_abs Default absolute tolerance for numeric columns
#' @param numeric_rel Default relative tolerance for numeric columns
#' @param integer_abs Default absolute tolerance for integer columns
#' @param character_equal_mode Default comparison mode for character columns ("exact", "normalized")
#' @param character_case_insensitive Logical for case-insensitive character comparison
#' @param character_trim Logical for trimming whitespace in character comparison
#' @param date_equal_mode Default comparison mode for date columns
#' @param datetime_equal_mode Default comparison mode for datetime columns
#' @param logical_equal_mode Default comparison mode for logical columns
#' @return The \code{path} to the written YAML file, returned invisibly.
#' @importFrom yaml write_yaml
#' @importFrom stats setNames
#' @importFrom dplyr collect
#' @export
#' @examples
#' df <- data.frame(id = 1:3, value = c(1.1, 2.2, 3.3), name = c("A", "B", "C"))
#' write_rules_template(df, key = "id", path = tempfile(fileext = ".yaml"))
write_rules_template <- function(data_reference,
                                 key = NULL, label = NULL, path = "rules.yaml", version = 1L, na_equal_default = TRUE,
                                 ignore_columns_default = character(0),
                                 check_count_default = TRUE, expected_count_default = NULL, row_count_tolerance_default = 0,
                                 numeric_abs = 0.000000001, numeric_rel = 0,
                                 integer_abs = 0L,
                                 character_equal_mode = "exact", character_case_insensitive = FALSE, character_trim = FALSE,
                                 date_equal_mode = "exact",
                                 datetime_equal_mode = "exact",
                                 logical_equal_mode = "exact") {

  # Validate key if provided
  # Use a 0-row collect to retrieve column names and types: works for both local
  # data.frames and lazy tables (tbl_lazy) without loading all rows.
  .ref_schema    <- dplyr::collect(utils::head(data_reference, 0L))
  .ref_col_names <- names(.ref_schema)

  if (!is.null(key)) {
    if (!is.character(key) || length(key) == 0) {
      stop("Parameter 'key' must be a non-empty character vector specifying column name(s) to use as join key(s).")
    }
    missing_keys <- setdiff(key, .ref_col_names)
    if (length(missing_keys) > 0) {
      stop(
        sprintf("Key column(s) not found in data: %s. Available columns: %s",
                paste(missing_keys, collapse = ", "),
                paste(.ref_col_names, collapse = ", "))
      )
    }
  }
  validate_label(label)
  if (is.null(label) || label == "") {label <- paste("comparaison", deparse1(substitute(data_reference)))

  }
  types <- detect_column_types(.ref_schema)
  y <- list(
    version = version,
    defaults = list(na_equal = na_equal_default,
                    ignore_columns = ignore_columns_default,
                    keys = key,
                    label = label
    ),
    row_validation = list(check_count = check_count_default, expected_count = expected_count_default, tolerance = row_count_tolerance_default),
    by_type = list(
      numeric = list(abs = numeric_abs, rel = numeric_rel),
      integer = list(abs = integer_abs),
      character = list(equal_mode = character_equal_mode, case_insensitive = character_case_insensitive, trim = character_trim),
      date = list(equal_mode = date_equal_mode),
      datetime = list(equal_mode = datetime_equal_mode),
      logical = list(equal_mode = logical_equal_mode)
    ),
    by_name = setNames(replicate(length(types), list(), simplify = FALSE), names(types))
  )
  write_yaml(x = y, file = path)
  invisible(path)
}

#' Read and validate YAML rules file
#'
#' Loads validation rules from a YAML file and ensures the format is valid.
#' Adds default values for missing configuration sections.
#'
#' @param path Character string specifying the path to the YAML rules file
#' @return A list containing the parsed and validated rules configuration
#' @importFrom yaml read_yaml
#' @examples
#' tmp <- tempfile(fileext = ".yaml")
#' write_rules_template(data.frame(id = 1L, value = 1.0), key = "id", path = tmp)
#' rules <- read_rules(tmp)
#' @export
read_rules <- function(path) {
  r <- read_yaml(path)
  stopifnot(is.list(r), !is.null(r$version), r$version == 1)
  r$defaults <- r$defaults %||% list()
  r$by_type  <- r$by_type  %||% list()
  r$by_name  <- r$by_name  %||% list()
  r$row_validation <- r$row_validation %||% list(check_count = FALSE, expected_count = NULL, tolerance = 0)
  r
}

#' Validate a report label argument
#'
#' A label must be NULL or a single non-NA character string: anything else
#' would crash later on the scalar `if (label == "")` fallback checks.
#'
#' @param label The label value to validate.
#' @return \code{NULL}, invisibly. Called for its side effect (error).
#' @noRd
validate_label <- function(label) {
  if (is.null(label)) {
    return(invisible(NULL))
  }
  if (!is.character(label) || length(label) != 1 || is.na(label)) {
    stop(
      "Parameter 'label' must be a single character string (or NULL).",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Validate a comparison key against both datasets
#'
#' Shared guard for every code path that consumes a key: checks the type and
#' emptiness of the key, then its presence in both datasets, and raises an
#' explicit error naming the missing column(s) and the dataset(s) concerned.
#'
#' @param key Character vector of key column names.
#' @param ref_cols Column names of the reference dataset.
#' @param cand_cols Column names of the candidate dataset.
#' @return \code{NULL}, invisibly. Called for its side effect (error).
#' @noRd
validate_comparison_key <- function(key, ref_cols, cand_cols) {
  if (!is.character(key) || length(key) == 0) {
    stop(
      "Parameter 'key' must be a non-empty character vector specifying column name(s) to use as join key(s).",
      call. = FALSE
    )
  }
  missing_key_ref  <- setdiff(key, ref_cols)
  missing_key_cand <- setdiff(key, cand_cols)
  if (length(missing_key_ref) > 0 || length(missing_key_cand) > 0) {
    describe_missing <- function(cols, dataset_name) {
      if (length(cols) == 0) {
        return(NULL)
      }
      sprintf(
        "%s missing in %s",
        paste(sprintf("'%s'", cols), collapse = ", "),
        dataset_name
      )
    }
    details <- c(
      describe_missing(missing_key_ref, dataset_name = "data_reference"),
      describe_missing(missing_key_cand, dataset_name = "data_candidate")
    )
    stop(sprintf(
      "Key column(s) not found: %s.",
      paste(details, collapse = "; ")
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' Compare datasets using YAML validation rules
#'
#' Main function for comparing reference and candidate datasets using configurable
#' validation rules defined in a YAML file. Supports exact matching, tolerance-based
#' comparisons, text normalization, and row count validation.
#'
#' @section Argument vs YAML precedence:
#' Some settings can come both from an explicit argument and from the YAML
#' rules file. The resolution is always: explicit argument first, then the
#' YAML `defaults` section, then the built-in default.
#'
#' | Setting | Explicit argument | YAML `defaults` field | Built-in default |
#' |---|---|---|---|
#' | join key | `key` | `keys` (legacy alias: `key`) | none (positional) |
#' | report label | `label` | `label` | "Comparing candidate vs reference" |
#'
#' When the YAML contains both `keys` and the legacy singular `key` field,
#' `keys` (the canonical field written by [write_rules_template()]) wins.
#'
#' The `key` argument must be a character vector (a non-character value is an
#' error), while YAML-sourced key values are coerced to character (YAML being
#' stringly typed, `keys: [2024]` designates the column named "2024").
#'
#' Note for `path = NULL`: the auto-generated rules template itself carries a
#' label (`"Comparison with default rules"`, or the explicit `label` argument),
#' so that is the label effectively used without a YAML file; the built-in
#' default above only applies when a YAML file is supplied with an empty or
#' missing `defaults$label`.
#'
#' @param data_reference Reference dataframe, tibble, or lazy table (tbl_lazy)
#' @param data_candidate Candidate dataframe to validate against reference
#' @param key Optional character vector of column names to use as join keys for
#'   ordered comparison. Every key column must exist in both datasets; otherwise
#'   an error is raised naming the missing column(s) and the dataset(s) concerned.
#'   See the "Argument vs YAML precedence" section.
#' @param path Path to YAML file containing validation rules. If NULL, default rules are
#'   generated automatically based on the reference dataset structure.
#' @param warn_at Warning threshold as fraction of failing tests (default: 1e-14)
#' @param stop_at Stop threshold as fraction of failing tests (default: 1e-14)
#' @param ref_suffix Suffix for reference columns in comparison dataframe (default: "__reference")
#' @param label Descriptive label for the validation report. See the
#'   "Argument vs YAML precedence" section.
#' @param error_msg_no_key Text of the error raised when a positional (key-less)
#'   comparison receives datasets with different row counts; the actual row
#'   counts of both datasets are appended to this text.
#' @param lang Language code for pointblank reports. Defaults to the
#'   \code{datadiff.lang} option if set, otherwise \code{"fr"}. Override globally
#'   with \code{options(datadiff.lang = "en")}. Supported values include
#'   "en" (English), "fr" (French), "de" (German), "it" (Italian), "es" (Spanish),
#'   "pt" (Portuguese), "zh" (Chinese), "ja" (Japanese), "ru" (Russian), etc.
#'   See pointblank documentation for full list.
#' @param locale Locale code for number and date formatting. Defaults to the
#'   \code{datadiff.locale} option if set, otherwise \code{"fr_FR"}. Override
#'   globally with \code{options(datadiff.locale = "en_US")}. Examples: "en_US",
#'   "en_GB", "de_DE", "es_ES", "pt_BR", "zh_CN", "ja_JP".
#' @param extract_failed Logical indicating whether to collect rows that failed validation
#'   (default: TRUE). Set to FALSE to reduce memory usage for large datasets with many errors.
#' @param get_first_n Integer specifying the maximum number of failed rows to extract per
#'   validation step (default: NULL, meaning all). Useful to limit memory when many rows fail.
#' @param sample_n Integer specifying a fixed number of failed rows to randomly sample per
#'   validation step (default: NULL). Alternative to get_first_n for random sampling.
#' @param sample_frac Numeric between 0 and 1 specifying the fraction of failed rows to sample
#'   (default: NULL). Used with sample_limit for proportional sampling.
#' @param sample_limit Integer specifying the maximum number of rows when using sample_frac
#'   (default: 5000). Acts as a ceiling for sampled rows.
#' @param duckdb_memory_limit Character string passed to DuckDB's `SET memory_limit`
#'   when Arrow datasets are used (default: `"8GB"`). Controls how much RAM DuckDB
#'   may use before spilling intermediate results to `tempdir()`. The default leaves
#'   headroom for R, Arrow, and the OS alongside DuckDB. Raise it (e.g. `"16GB"`)
#'   on machines with ample free RAM to reduce disk I/O; lower it (e.g. `"4GB"`)
#'   when memory is very constrained. Has no effect when both inputs are plain
#'   `data.frame`s or `tbl_lazy` objects.
#' @return A list containing:
#'   \item{agent}{Configured pointblank agent with validation results}
#'   \item{reponse}{Interrogated pointblank agent (class \code{datadiff_report}):
#'     usable by \code{pointblank::all_passed()} / \code{get_data_extracts()};
#'     printing it lazily renders a full pointblank-style report from
#'     \code{coverage} (built on demand, memoized).}
#'   \item{missing_in_candidate}{Columns missing in candidate data}
#'   \item{extra_in_candidate}{Extra columns in candidate data}
#'   \item{applied_rules}{Final column-specific rules applied}
#'   \item{coverage}{A \code{datadiff_coverage} data.frame: one row per check
#'     actually performed (column, check type, n, n_failed, status), always
#'     produced at negligible cost so the verified checks stay visible even when
#'     the fast path skips the per-column agent.}
#'   \item{summary}{Aggregate counts from \code{coverage} (n_checks, n_pass,
#'     n_fail, n_rows_failed_total, all_passed).}
#' @importFrom dplyr arrange across left_join %>%
#' @importFrom pointblank interrogate
#' @importFrom dplyr collect
#' @export
#' @examples
#' # Create test data
#' ref <- data.frame(id = 1:3, value = c(1.0, 2.0, 3.0))
#' cand <- data.frame(id = 1:3, value = c(1.1, 2.1, 3.1))
#'
#' # Compare datasets without YAML (uses default rules, positional comparison)
#' result <- compare_datasets_from_yaml(ref, cand)
#'
#' # Compare datasets with key but without YAML
#' result <- compare_datasets_from_yaml(ref, cand, key = "id")
#'
#' # Compare datasets with custom YAML rules
#' tmp <- tempfile(fileext = ".yaml")
#' write_rules_template(ref, key = "id", path = tmp)
#' result <- compare_datasets_from_yaml(ref, cand, key = "id", path = tmp)
#' result$reponse
compare_datasets_from_yaml <- function(data_reference,
                                       data_candidate,
                                       key = NULL,
                                       path = NULL,
                                       warn_at = 0.00000000000001, stop_at = 0.00000000000001,
                                       ref_suffix = "__reference",
                                       label = NULL,
                                       error_msg_no_key = "Without keys, both tables must have the same number of rows.",
                                       lang = getOption("datadiff.lang", "fr"),
                                       locale = getOption("datadiff.locale", "fr_FR"),
                                       extract_failed = TRUE,
                                       get_first_n = NULL,
                                       sample_n = NULL,
                                       sample_frac = NULL,
                                       sample_limit = 5000,
                                       duckdb_memory_limit = "8GB"
) {
  # Input validation
  valid_classes <- c("data.frame", "tbl_lazy", "ArrowObject", "arrow_dplyr_query")
  if (!inherits(data_reference, valid_classes)) {
    stop("data_reference must be a data.frame, tibble, lazy table, or Arrow object")
  }
  if (!inherits(data_candidate, valid_classes)) {
    stop("data_candidate must be a data.frame, tibble, lazy table, or Arrow object")
  }
  validate_label(label)

  # Guard: ensure dbplyr is available when lazy tables are used
  if (inherits(data_reference, "tbl_lazy") || inherits(data_candidate, "tbl_lazy")) {
    if (!requireNamespace("dbplyr", quietly = TRUE)) {
      stop("Package 'dbplyr' is required for lazy table support.")
    }
  }

  # Guard: ensure arrow and duckdb are available when Arrow objects are used
  if (is_arrow(data_reference) || is_arrow(data_candidate)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Arrow/Parquet support.")
    }
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("Package 'duckdb' is required for Arrow/Parquet support (used as lazy query engine).")
    }
  }

  # Arrow inputs: convert to DuckDB BEFORE any transformation so that the join
  # and all subsequent mutates become lazy SQL inside DuckDB instead of being
  # evaluated by Arrow's Acero engine (which materialises everything in RAM).
  #
  # Strategy: create one plain duckdb::duckdb() connection, configure it for
  # large-dataset workloads, then materialise each Arrow input as a physical
  # DuckDB temp table via read_parquet() when the dataset is file-backed
  # (Parquet files).  Using DuckDB's native Parquet reader rather than
  # arrow::to_duckdb() ensures that ALL memory is tracked and managed by
  # DuckDB's own buffer pool, so disk-spilling works correctly.
  #
  # With arrow::to_duckdb(), Arrow allocates read buffers OUTSIDE DuckDB's
  # memory manager.  Combined Arrow + DuckDB memory can exceed physical RAM
  # before DuckDB's spilling threshold is reached -> OOM.  Native read_parquet()
  # eliminates this external allocation.
  if (is_arrow(data_reference) || is_arrow(data_candidate)) {
    fresh_con <- duckdb::dbConnect(duckdb::duckdb())
    on.exit(duckdb::dbDisconnect(fresh_con, shutdown = TRUE), add = TRUE)
    # Enable disk-spilling.
    DBI::dbExecute(fresh_con, paste0(
      "SET temp_directory='", gsub("\\\\", "/", tempdir()), "'"
    ))
    # Cap DuckDB's buffer pool so it starts spilling well before exhausting
    # system RAM.  The default (80 % of total RAM) leaves no headroom for
    # R, Arrow, and OS memory.  Configurable via duckdb_memory_limit.
    DBI::dbExecute(fresh_con, paste0("SET memory_limit = '", duckdb_memory_limit, "'"))
    if (is_arrow(data_reference))
      data_reference <- arrow_dataset_to_duckdb(data_reference, fresh_con, "datadiff_ref")
    if (is_arrow(data_candidate))
      data_candidate <- arrow_dataset_to_duckdb(data_candidate, fresh_con, "datadiff_cand")
  }

  # Validate the key argument against BOTH datasets before any other consumer:
  # the auto-generated-template path (path = NULL) would otherwise reach
  # write_rules_template() first, which validates the reference only and with
  # a less specific error.
  if (!is.null(key)) {
    validate_comparison_key(
      key,
      ref_cols = get_col_names(data_reference),
      cand_cols = get_col_names(data_candidate)
    )
  }

  # If no path provided, create a temporary YAML with default rules
  if (is.null(path)) {
    path <- tempfile(fileext = ".yaml")
    write_rules_template(
      data_reference = data_reference,
      key = key,
      label = label %||% "Comparison with default rules",
      path = path
    )
  } else {
    if (!is.character(path) || length(path) != 1) {
      stop("path must be a single character string")
    }
    if (!file.exists(path)) {
      stop(sprintf("YAML rules file not found: %s", path))
    }
  }

  # Check for reserved suffix conflicts in column names
  ref_cols_with_suffix <- grep(ref_suffix, get_col_names(data_reference), fixed = TRUE, value = TRUE)
  cand_cols_with_suffix <- grep(ref_suffix, get_col_names(data_candidate), fixed = TRUE, value = TRUE)
  if (length(ref_cols_with_suffix) > 0 || length(cand_cols_with_suffix) > 0) {
    conflicting <- unique(c(ref_cols_with_suffix, cand_cols_with_suffix))
    warning(sprintf(
      "Column(s) containing reserved suffix '%s' detected: %s. This may cause conflicts.",
      ref_suffix, paste(conflicting, collapse = ", ")
    ))
  }

  rules <- read_rules(path)

  # Collect 0-row schemas for type detection and type-dependent logic.
  schema_ref  <- dplyr::collect(utils::head(data_reference,  0L))
  schema_cand <- dplyr::collect(utils::head(data_candidate, 0L))

  na_equal <- isTRUE(rules$defaults$na_equal)
  ignore_columns <- rules$defaults$ignore_columns %||% character(0)

  # Precedence: explicit argument > YAML defaults > built-in default.
  # An empty string means "no explicit label" (same convention as
  # write_rules_template), so it must not shadow the YAML label.
  if (!is.null(label) && label == "") {
    label <- NULL
  }
  label <- label %||% rules$defaults[["label"]]
  if (is.null(label) || label == "") {
    label <- "Comparing candidate vs reference"
  }

  # Precedence: explicit argument > YAML defaults > none. The canonical YAML
  # field is "keys" (what write_rules_template() writes); a legacy singular
  # "key" field is honored as fallback. [[ avoids $ partial matching.
  if (is.null(key)) {
    key <- rules$defaults[["keys"]] %||% rules$defaults[["key"]]
    if (!is.null(key)) {
      key <- as.character(unlist(key, use.names = FALSE))
    }
  }

  if (is.null(key)) {message("key is missing")}

  # Check for duplicate keys (only if key exists in both datasets)
  if (!is.null(key) && all(key %in% get_col_names(data_reference)) && all(key %in% get_col_names(data_candidate))) {
    # Detect duplicate key values. Local data.frames use a fast
    # anyDuplicated()/duplicated() pass; lazy tables keep the SQL-native count.
    ref_dup_info  <- find_duplicate_keys(data_reference, key)
    cand_dup_info <- find_duplicate_keys(data_candidate, key)

    ref_has_dups  <- !is.null(ref_dup_info)
    cand_has_dups <- !is.null(cand_dup_info)

    if (ref_has_dups || cand_has_dups) {
      # Build detailed warning message
      warning_parts <- c()

      if (ref_has_dups) {
        warning_parts <- c(warning_parts, sprintf(
          "data_reference: %d duplicate key value(s) affecting %d rows (examples: %s)",
          ref_dup_info$n_dup_keys, ref_dup_info$n_dup_rows,
          paste(ref_dup_info$examples, collapse = "; ")
        ))
      }

      if (cand_has_dups) {
        warning_parts <- c(warning_parts, sprintf(
          "data_candidate: %d duplicate key value(s) affecting %d rows (examples: %s)",
          cand_dup_info$n_dup_keys, cand_dup_info$n_dup_rows,
          paste(cand_dup_info$examples, collapse = "; ")
        ))
      }

      warning(sprintf(
        paste0(
          "Duplicate keys detected! The key column(s) [%s] must be unique in both datasets.\n",
          "  - %s\n",
          "Comparison results will be unreliable: the join will produce multiple rows per key, ",
          "leading to incorrect or non-deterministic validation results.\n",
          "Please ensure your key column(s) uniquely identify each row, or choose different key column(s)."
        ),
        paste(key, collapse = ", "),
        paste(warning_parts, collapse = "\n  - ")
      ), call. = FALSE)
    }
  }

  # Analyze columns
  col_analysis <- analyze_columns(data_reference, data_candidate, ignore_columns = ignore_columns)
  cols_reference <- col_analysis$cols_reference
  cols_candidate <- col_analysis$cols_candidate
  missing_in_candidate <- col_analysis$missing_in_candidate
  extra_in_candidate <- col_analysis$extra_in_candidate
  common_cols <- col_analysis$common_cols

  # Remove key columns from comparison columns
  common_cols <- setdiff(common_cols, key)

  col_rules <- derive_column_rules(schema_ref[, common_cols, drop = FALSE], rules)

  # Detect type mismatches between reference and candidate for common columns.
  # Mismatched columns are excluded from validation logic (tolerance or equality)
  # and reported as dedicated failing validation steps.
  type_ref  <- detect_column_types(schema_ref[, common_cols, drop = FALSE])
  cand_common <- intersect(common_cols, names(schema_cand))
  type_cand <- detect_column_types(schema_cand[, cand_common, drop = FALSE])
  # integer and numeric are compatible numeric types: arithmetic and tolerance
  # comparisons work correctly across them, so they are not considered a mismatch.
  numeric_types <- c("integer", "numeric")
  type_mismatch_cols <- common_cols[vapply(common_cols, function(nm) {
    if (!(nm %in% names(type_cand))) return(FALSE)
    t_ref  <- type_ref[[nm]]
    t_cand <- type_cand[[nm]]
    if (t_ref == t_cand) return(FALSE)
    if (t_ref %in% numeric_types && t_cand %in% numeric_types) return(FALSE)
    TRUE
  }, logical(1))]
  if (length(type_mismatch_cols) > 0) {
    mismatch_details <- vapply(type_mismatch_cols, function(nm) {
      sprintf("'%s' (reference: %s, candidate: %s)", nm, type_ref[[nm]], type_cand[[nm]])
    }, character(1))
    warning(sprintf(
      "Type mismatch detected in %d column(s): %s. Each will be reported as a validation error.",
      length(type_mismatch_cols),
      paste(mismatch_details, collapse = ", ")
    ), call. = FALSE)
  }

  data_reference_p <- preprocess_dataframe(data_reference, col_rules, schema = schema_ref)
  data_candidate_p <- preprocess_dataframe(data_candidate, col_rules, schema = schema_cand)

  # Get row validation information
  row_validation_info <- validate_row_counts(data_reference_p, data_candidate_p, rules)

  if (!is.null(key)) {
    # Re-validated here because the key may come from the YAML rules
    # (defaults$keys), which the argument-level check upstream cannot see.
    validate_comparison_key(
      key,
      ref_cols = get_col_names(data_reference_p),
      cand_cols = get_col_names(data_candidate_p)
    )

    # Join candidate to reference on key to handle different row counts
    cmp <- left_join(data_candidate_p, data_reference_p, by = key, suffix = c("", ref_suffix))
  } else {
    # Row-count mismatch is decidable from the counts precomputed by
    # validate_row_counts(): abort before the potentially expensive collect
    # of non-local tables below.
    if (row_validation_info$ref_count != row_validation_info$cand_count) {
      stop(sprintf(
        "%s (data_reference: %s rows, data_candidate: %s rows).",
        error_msg_no_key,
        row_validation_info$ref_count,
        row_validation_info$cand_count
      ), call. = FALSE)
    }
    # For non-keyed comparison, collect non-local tables on BOTH sides
    # (positional binding requires local data on each side).
    if (is_non_local(data_reference_p) || is_non_local(data_candidate_p)) {
      message("Note: positional comparison requires collecting non-local tables into memory.")
      if (is_non_local(data_reference_p)) {
        data_reference_p <- dplyr::collect(data_reference_p)
      }
      if (is_non_local(data_candidate_p)) {
        data_candidate_p <- dplyr::collect(data_candidate_p)
      }
    }
    cmp <- data_candidate_p
    if (length(common_cols) > 0) {
      ref_block <- data_reference_p[, common_cols, drop = FALSE]
      names(ref_block) <- paste0(common_cols, ref_suffix)
      cmp <- dplyr::bind_cols(cmp, ref_block)
    }
  }

  # Detect tolerance columns from schema (works for both local and lazy tables).
  # Type-mismatched columns are excluded: arithmetic on non-numeric candidate
  # values would crash (e.g. sign() on character). They get their own failing
  # validation step via setup_pointblank_agent instead.
  tol_cols <- names(col_rules)
  tol_cols <- tol_cols[vapply(X = tol_cols, FUN = function(nm) is.numeric(schema_ref[[nm]]), FUN.VALUE = logical(1))]
  tol_cols <- tol_cols[vapply(X = tol_cols, FUN = function(nm) {
    cr <- col_rules[[nm]]
    !is.null(cr$abs) || !is.null(cr$rel)
  }, FUN.VALUE = logical(1))]
  tol_cols <- setdiff(tol_cols, type_mismatch_cols)

  # Equality columns the verdict actually checks: common, non-key, non-tolerance
  # AND non-type-mismatched. Derived once and threaded to both the __eq producer
  # and the verdict consumer so the two sets cannot drift. Type-mismatched
  # columns are excluded here because the equality SQL would compare incompatible
  # types and crash the lazy path (e.g. casting a character candidate to the
  # numeric reference's type); they are reported as failing validation steps
  # instead.
  eq_cols <- setdiff(setdiff(common_cols, type_mismatch_cols), tol_cols)

  # Add the per-column within-tolerance (__ok) and, on the lazy path, equality
  # (__eq) booleans - the only columns that drive the verdict.
  #  - Local: materialise only __ok via a vectorised fast path (__eq is
  #    recomputed on the fly where needed).
  #  - Lazy: build __ok AND __eq in a SINGLE templated SQL SELECT. Doing this
  #    with per-column dplyr::mutate() is O(columns) on the R side (dbplyr query
  #    construction + SQL rendering), the dominant cost on wide tables; the
  #    templated SQL is O(1) dbplyr work and lets the database do the rest.
  cmp <- if (is_non_local(cmp)) {
    # Numeric equality columns get NaN-aware NA rules in the SQL (matching
    # the local path, where is.na(NaN) is TRUE); isnan() on a non-numeric
    # column would be a SQL type error.
    eq_num_cols <- eq_cols[vapply(X = eq_cols, FUN = function(nm) {
      is.numeric(schema_ref[[nm]])
    }, FUN.VALUE = logical(1))]
    add_bool_cols_sql(cmp, tol_cols, eq_cols,
                      col_rules, ref_suffix, na_equal,
                      eq_num_cols = eq_num_cols)
  } else {
    add_ok_columns(cmp, tol_cols, col_rules, ref_suffix, na_equal)
  }

  # Add row count validation column if needed
  row_count_ok <- TRUE
  if (row_validation_info$check_count) {
    # Calculate if row count validation passes
    expected <- row_validation_info$expected_count
    if (!is.null(expected)) {
      row_count_ok <- abs(row_validation_info$cand_count - expected) <= row_validation_info$tolerance
    } else {
      row_count_ok <- abs(row_validation_info$cand_count - row_validation_info$ref_count) <= row_validation_info$tolerance
    }
    # Add row_count_ok column via mutate - works for both local and lazy tables,
    # and handles empty dataframes correctly (no nrow() guard needed).
    cmp <- dplyr::mutate(cmp, row_count_ok = !!row_count_ok)
  }

  # For non-local tables (DuckDB / Arrow-backed): materialise only the boolean
  # validation columns to a physical DuckDB temp table before creating the
  # pointblank agent. create_agent() on a 600+ column Arrow-backed lazy query
  # scales poorly (~50x slower than a physical table); a slim table containing
  # only the ~125 __ok/__eq/row_count_ok booleans (~62 MB for 4 M rows) makes
  # agent creation and interrogation fast without loading data into R memory.
  is_lazy <- is_non_local(cmp)
  cmp_for_agent <- cmp
  if (is_lazy) {
    val_cols <- c(
      paste0(tol_cols, "__ok"),
      paste0(eq_cols, "__eq"),
      if (isTRUE(row_validation_info$check_count)) "row_count_ok" else character(0)
    )
    cmp_slim      <- dplyr::select(cmp, dplyr::any_of(val_cols))
    tmp_tbl_name  <- datadiff_tmp_table_name()
    # compute() sends CREATE TEMP TABLE AS SELECT ... to DuckDB: all computation
    # (join, boolean expressions) happens inside DuckDB's process, with disk
    # spilling available for the large join.  We then collect() the slim boolean
    # result into R so that pointblank receives a plain data.frame - avoiding
    # DuckDB connection-state issues (is_tbl_mssql crash) during interrogation.
    cmp_slim_computed <- dplyr::compute(cmp_slim, name = tmp_tbl_name, temporary = TRUE)
    # The slim table only feeds the collect() below. Drop it at exit so that
    # repeated calls on a user-supplied connection do not accumulate temp
    # tables for the lifetime of that connection. after = FALSE runs the drop
    # BEFORE the exit handlers registered earlier, in particular before the
    # dbDisconnect of the private connection on the Arrow path (on.exit
    # add = TRUE fires FIFO by default, which would drop on a closed
    # connection); the try() then only masks genuine failures.
    on.exit(
      try(
        DBI::dbRemoveTable(dbplyr::remote_con(cmp_slim_computed), tmp_tbl_name),
        silent = TRUE
      ),
      add = TRUE, after = FALSE
    )
    cmp_for_agent     <- dplyr::collect(cmp_slim_computed)
  }

  # Fast all-pass short-circuit.
  # The verdict is fully determined by the boolean validation columns already
  # computed above (plus the structural checks). When everything passes there
  # are no cells to extract, so the expensive per-column pointblank agent (one
  # step per column, ~quadratic on wide tables) can be replaced by a constant
  # cost trivially-passing agent. all_passed stays identical and
  # get_data_extracts() is empty either way. Any failure falls through to the
  # full per-column agent so failing cells remain extractable byte-for-byte.
  all_passed_fast <-
    length(missing_in_candidate) == 0 &&
    length(type_mismatch_cols) == 0 &&
    isTRUE(row_count_ok) &&
    all_validations_pass(
      tbl = cmp_for_agent, tol_cols = tol_cols, eq_cols = eq_cols,
      ref_suffix = ref_suffix, na_equal = na_equal
    )

  # Faithful, O(columns) record of every check performed, built from the same
  # booleans the verdict is derived from. Always produced (green and red) so the
  # caller can see what was verified even when the fast path skips the per-column
  # pointblank agent.
  coverage <- build_coverage(
    tbl = cmp_for_agent, tol_cols = tol_cols, eq_cols = eq_cols,
    missing_in_candidate = missing_in_candidate,
    type_mismatch_cols = type_mismatch_cols,
    row_validation_info = row_validation_info, row_count_ok = row_count_ok,
    ref_suffix = ref_suffix, na_equal = na_equal
  )

  if (all_passed_fast) {
    agent <- build_pass_agent(
      tbl = cmp_for_agent, label = label,
      warn_at = warn_at, stop_at = stop_at, lang = lang, locale = locale
    )
  } else {
    # Failure path: build pointblank steps only for the columns that actually
    # fail. Columns that pass produce empty data extracts, so omitting their
    # steps leaves get_data_extracts() byte-for-byte unchanged while avoiding
    # the per-column agent overhead on the passing majority. col_exists steps
    # (which only ever pass) are dropped for the same reason. Structural
    # failures (missing columns, type mismatches, row count) are always kept.
    fail <- failing_columns(
      tbl = cmp_for_agent, tol_cols = tol_cols, eq_cols = eq_cols,
      ref_suffix = ref_suffix, na_equal = na_equal
    )
    # Local path: materialise the __eq boolean for the failing equality
    # columns so the pointblank step validates the exact boolean the verdict
    # used (one-sided NA fails, two-sided NA follows na_equal). The lazy path
    # already carries __eq columns; col_vals_equal(na_pass = ...) alone cannot
    # express these semantics.
    if (!is_lazy && length(fail$eq) > 0) {
      for (c in fail$eq) {
        cmp_for_agent[[paste0(c, "__eq")]] <- eq_col_bool(
          cmp_for_agent, col = c, ref_suffix = ref_suffix, na_equal = na_equal
        )
      }
    }
    # Materialise the measured deviation (<col>__absdiff) and applied threshold
    # (<col>__thresh) for the FAILING tolerance columns only, so the extracts
    # and the report CSV show the explicit gap (as in <= 0.4.7) at a cost
    # proportional to the failing columns, not the table width. The lazy path
    # is excluded: its agent table is the slim boolean collect(), which never
    # carried the original values nor these diagnostics.
    if (!is_lazy && length(fail$tol) > 0) {
      cmp_for_agent <- add_diff_columns(
        cmp_for_agent, fail$tol, col_rules, ref_suffix, na_equal
      )
    }
    agent <- setup_pointblank_agent(
      cmp_for_agent,
      cols_reference,
      fail$eq,
      fail$tol,
      row_validation_info,
      ref_suffix,
      warn_at,
      stop_at,
      label,
      na_equal,
      lang,
      locale,
      missing_in_candidate = missing_in_candidate,
      type_mismatch_cols = type_mismatch_cols,
      add_col_exists_steps = FALSE
    )
  }

  reponse <- interrogate(
    agent,
    extract_failed = extract_failed,
    get_first_n = get_first_n,
    sample_n = sample_n,
    sample_frac = sample_frac,
    sample_limit = sample_limit
  )

  all_passed <- pointblank::all_passed(reponse)

  # Make reponse render the full pointblank report lazily (on print) from the
  # coverage, while remaining a real interrogated agent for all_passed() and
  # get_data_extracts().
  reponse <- as_datadiff_report(
    reponse, coverage = coverage, label = label, lang = lang, locale = locale,
    warn_at = warn_at, stop_at = stop_at
  )

  list(
    all_passed = all_passed,
    agent = agent,
    reponse = reponse,
    missing_in_candidate = missing_in_candidate,
    extra_in_candidate = extra_in_candidate,
    applied_rules = col_rules,
    coverage = coverage,
    summary = summarize_coverage(coverage)
  )
}
