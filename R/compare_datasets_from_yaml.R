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
#' @return Invisible NULL (writes file to disk)
#' @importFrom yaml write_yaml
#' @importFrom stats setNames
#' @importFrom dplyr collect
#' @export
#' @examples
#' df <- data.frame(id = 1:3, value = c(1.1, 2.2, 3.3), name = c("A", "B", "C"))
#' write_rules_template(df, key = "id", "my_rules.yaml")
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
#' # Assuming "rules.yaml" exists
#' rules <- read_rules("rules.yaml")
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

#' Compare datasets using YAML validation rules
#'
#' Main function for comparing reference and candidate datasets using configurable
#' validation rules defined in a YAML file. Supports exact matching, tolerance-based
#' comparisons, text normalization, and row count validation.
#'
#' @param data_reference Reference dataframe, tibble, or lazy table (tbl_lazy)
#' @param data_candidate Candidate dataframe to validate against reference
#' @param key Optional character vector of column names to use as join keys for ordered comparison
#' @param path Path to YAML file containing validation rules. If NULL, default rules are
#'   generated automatically based on the reference dataset structure.
#' @param warn_at Warning threshold as fraction of failing tests (default: 0.01)
#' @param stop_at Stop threshold as fraction of failing tests (default: 0.01)
#' @param ref_suffix Suffix for reference columns in comparison dataframe (default: "__reference")
#' @param label Descriptive label for the validation report
#' @param error_msg_no_key Error message when datasets have different row counts without keys
#' @param lang Language code for pointblank reports (default: "fr"). Supported values include
#'   "en" (English), "fr" (French), "de" (German), "it" (Italian), "es" (Spanish), "pt" (Portuguese),
#'   "zh" (Chinese), "ja" (Japanese), "ru" (Russian), etc. See pointblank documentation for full list.
#' @param locale Locale code for number and date formatting (default: "fr_FR"). Examples: "en_US",
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
#'   \item{reponse}{Interrogation results from pointblank}
#'   \item{missing_in_candidate}{Columns missing in candidate data}
#'   \item{extra_in_candidate}{Extra columns in candidate data}
#'   \item{applied_rules}{Final column-specific rules applied}
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
#' write_rules_template(ref, key = "id", path = "rules.yaml")
#' result <- compare_datasets_from_yaml(ref, cand, key = "id", path = "rules.yaml")
#' result$reponse
compare_datasets_from_yaml <- function(data_reference,
                                       data_candidate,
                                       key = NULL,
                                       path = NULL,
                                       warn_at = 0.00000000000001, stop_at = 0.00000000000001,
                                       ref_suffix = "__reference",
                                       label = NULL,
                                       error_msg_no_key = "Without keys, both tables must have the same number of rows.",
                                       lang = "fr",
                                       locale = "fr_FR",
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
  # before DuckDB's spilling threshold is reached → OOM.  Native read_parquet()
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
  label <- rules$defaults$label
  if (is.null(label) || label == "") {label <- "Comparing candidate vs reference"}

  # Use key parameter if provided, otherwise fall back to rules
  if (is.null(key)) {
    key <- rules$defaults$key
  }

  if (is.null(key)) {message("key is missing")}

  # Check for duplicate keys (only if key exists in both datasets)
  if (!is.null(key) && all(key %in% get_col_names(data_reference)) && all(key %in% get_col_names(data_candidate))) {
    # Use SQL-native GROUP BY + COUNT to find duplicate key values (works for both
    # local data.frames and lazy tables without bracket-subsetting).
    ref_dups <- data_reference %>%
      dplyr::count(dplyr::across(dplyr::all_of(key))) %>%
      dplyr::filter(n > 1L) %>%
      dplyr::collect()

    cand_dups <- data_candidate %>%
      dplyr::count(dplyr::across(dplyr::all_of(key))) %>%
      dplyr::filter(n > 1L) %>%
      dplyr::collect()

    ref_has_dups <- nrow(ref_dups) > 0
    cand_has_dups <- nrow(cand_dups) > 0

    if (ref_has_dups || cand_has_dups) {
      # Build detailed warning message
      warning_parts <- c()

      if (ref_has_dups) {
        n_dup_keys_ref <- nrow(ref_dups)
        n_dup_rows_ref <- sum(ref_dups$n)
        key_cols_ref <- ref_dups[, key, drop = FALSE]

        examples_ref <- if (n_dup_keys_ref <= 3) {
          apply(key_cols_ref, 1, \(r) paste(key, "=", r, collapse = ", "))
        } else {
          c(apply(key_cols_ref[1:3, , drop = FALSE], 1, \(r) paste(key, "=", r, collapse = ", ")), "...")
        }

        warning_parts <- c(warning_parts, sprintf(
          "data_reference: %d duplicate key value(s) affecting %d rows (examples: %s)",
          n_dup_keys_ref, n_dup_rows_ref, paste(examples_ref, collapse = "; ")
        ))
      }

      if (cand_has_dups) {
        n_dup_keys_cand <- nrow(cand_dups)
        n_dup_rows_cand <- sum(cand_dups$n)
        key_cols_cand <- cand_dups[, key, drop = FALSE]

        examples_cand <- if (n_dup_keys_cand <= 3) {
          apply(key_cols_cand, 1, \(r) paste(key, "=", r, collapse = ", "))
        } else {
          c(apply(key_cols_cand[1:3, , drop = FALSE], 1, \(r) paste(key, "=", r, collapse = ", ")), "...")
        }

        warning_parts <- c(warning_parts, sprintf(
          "data_candidate: %d duplicate key value(s) affecting %d rows (examples: %s)",
          n_dup_keys_cand, n_dup_rows_cand, paste(examples_cand, collapse = "; ")
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

  data_reference_p <- preprocess_dataframe(data_reference, col_rules, schema = schema_ref)
  data_candidate_p <- preprocess_dataframe(data_candidate, col_rules, schema = schema_cand)

  # Get row validation information
  row_validation_info <- validate_row_counts(data_reference_p, data_candidate_p, rules)

  if (!is.null(key)) {
    if (isFALSE(all(key %in% get_col_names(data_reference_p))) | isFALSE(all(key %in% get_col_names(data_candidate_p)))) {
      message("could not find key in both data")
      return(
        list(
          all_passed = FALSE,
          agent = NULL,
          reponse = NULL,
          missing_in_candidate = NULL,
          extra_in_candidate = NULL,
          applied_rules = NULL
        )
      )
    }

    # Join candidate to reference on key to handle different row counts
    cmp <- left_join(data_candidate_p, data_reference_p, by = key, suffix = c("", ref_suffix))
  } else {
    # For non-keyed comparison, collect non-local tables (positional join requires local data)
    if (is_non_local(data_reference_p)) {
      message("Note: positional comparison requires collecting non-local tables into memory.")
      data_reference_p <- dplyr::collect(data_reference_p)
      data_candidate_p <- dplyr::collect(data_candidate_p)
    }
    # Use pre-computed counts from validate_row_counts (avoids a second nrow() on lazy)
    if (row_validation_info$ref_count != row_validation_info$cand_count) {
      message("Warning: Row counts differ without keys - validation will fail")
    }
    cmp <- data_candidate_p
    for (c in common_cols) {
      cmp[[paste0(c, ref_suffix)]] <- data_reference_p[[c]]
    }
  }

  # Detect tolerance columns from schema (works for both local and lazy tables)
  tol_cols <- names(col_rules)
  tol_cols <- tol_cols[vapply(X = tol_cols, FUN = function(nm) is.numeric(schema_ref[[nm]]), FUN.VALUE = logical(1))]
  tol_cols <- tol_cols[vapply(X = tol_cols, FUN = function(nm) {
    cr <- col_rules[[nm]]
    !is.null(cr$abs) || !is.null(cr$rel)
  }, FUN.VALUE = logical(1))]

  # Add tolerance columns
  cmp <- add_tolerance_columns(cmp, tol_cols, col_rules, ref_suffix, na_equal)

  # For lazy tables, pre-compute equality columns for non-tolerance columns.
  # pointblank's col_vals_equal(value = cmp[[col]]) cannot access SQL columns via [[,
  # so we compute c__eq as a boolean SQL column and validate that against TRUE.
  # All columns are batched into a single mutate() to prevent O(n) nested
  # lazy_query nodes that would exceed R's expression evaluation stack limit.
  if (is_non_local(cmp)) {
    eq_cols_lazy <- setdiff(common_cols, tol_cols)
    exprs_eq <- list()
    for (c in eq_cols_lazy) {
      c_sym     <- dplyr::sym(c)
      rc_sym    <- dplyr::sym(paste0(c, ref_suffix))
      eq_col_nm <- paste0(c, "__eq")
      if (na_equal) {
        exprs_eq[[eq_col_nm]] <- rlang::expr(dplyr::case_when(
          is.na(!!c_sym) & is.na(!!rc_sym) ~ TRUE,
          is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
          !!c_sym == !!rc_sym              ~ TRUE,
          .default = FALSE
        ))
      } else {
        exprs_eq[[eq_col_nm]] <- rlang::expr(dplyr::case_when(
          is.na(!!c_sym) | is.na(!!rc_sym) ~ FALSE,
          !!c_sym == !!rc_sym              ~ TRUE,
          .default = FALSE
        ))
      }
    }
    if (length(exprs_eq) > 0) {
      cmp <- dplyr::mutate(cmp, !!!exprs_eq)
    }
  }

  # Add row count validation column if needed
  if (row_validation_info$check_count) {
    # Calculate if row count validation passes
    expected <- row_validation_info$expected_count
    if (!is.null(expected)) {
      row_count_ok <- abs(row_validation_info$cand_count - expected) <= row_validation_info$tolerance
    } else {
      row_count_ok <- abs(row_validation_info$cand_count - row_validation_info$ref_count) <= row_validation_info$tolerance
    }
    # Add row_count_ok column via mutate — works for both local and lazy tables,
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
    eq_cols_lazy <- setdiff(common_cols, tol_cols)
    val_cols <- c(
      paste0(tol_cols, "__ok"),
      paste0(eq_cols_lazy, "__eq"),
      if (isTRUE(row_validation_info$check_count)) "row_count_ok" else character(0)
    )
    cmp_slim      <- dplyr::select(cmp, dplyr::any_of(val_cols))
    tmp_tbl_name  <- paste0("datadiff_", gsub("[^0-9]", "", format(Sys.time(), "%H%M%OS3")))
    # compute() sends CREATE TEMP TABLE AS SELECT … to DuckDB: all computation
    # (join, boolean expressions) happens inside DuckDB's process, with disk
    # spilling available for the large join.  We then collect() the slim boolean
    # result into R so that pointblank receives a plain data.frame — avoiding
    # DuckDB connection-state issues (is_tbl_mssql crash) during interrogation.
    cmp_slim_computed <- dplyr::compute(cmp_slim, name = tmp_tbl_name, temporary = TRUE)
    cmp_for_agent     <- dplyr::collect(cmp_slim_computed)
  }

  # Configure pointblank agent
  agent <- setup_pointblank_agent(cmp_for_agent, cols_reference, common_cols, tol_cols, row_validation_info, ref_suffix, warn_at, stop_at, label, na_equal, lang, locale, missing_in_candidate = missing_in_candidate, add_col_exists_steps = !is_lazy)

  reponse <- interrogate(
    agent,
    extract_failed = extract_failed,
    get_first_n = get_first_n,
    sample_n = sample_n,
    sample_frac = sample_frac,
    sample_limit = sample_limit
  )

  list(
    all_passed = pointblank::all_passed(reponse),
    agent = agent,
    reponse = reponse,
    missing_in_candidate = missing_in_candidate,
    extra_in_candidate = extra_in_candidate,
    applied_rules = col_rules
  )
}
