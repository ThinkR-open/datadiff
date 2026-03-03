# datadiff

Data Validation with YAML Rules

## Overview

`datadiff` is a comprehensive R package for data validation that lets you compare datasets using configurable validation rules defined in YAML files.  
It supports exact matching, tolerance-based numeric comparisons, text normalization, and row count validation.

## Installation

```r
install.packages("datadiff", repos = "https://artifactory.intranet.atih.sante.fr/artifactory/R-all")
```

## Quick Start

```r
library(datadiff)

# Create reference and candidate datasets
reference <- data.frame(
  id = 1:3,
  amount = c(100.00, 200.00, 300.00),
  category = c("A", "B", "C")
)

candidate <- data.frame(
  id = 1:3,
  amount = c(100.01, 200.01, 300.001),  # Small differences within tolerance
  category = c("a", "b", "c")  # Different case
)

# Generate a rules template
write_rules_template(reference, key = "id",
                     path = "validation_rules.yaml",
                     numeric_abs = 0.01,
                     character_case_insensitive = TRUE)

# Edit the rules file to configure validation (e.g., set tolerances, case sensitivity)

# Compare datasets
result <- compare_datasets_from_yaml(reference, candidate, path = "validation_rules.yaml")
print(result$reponse)
```

## Key Features

- **YAML Configuration**: Define validation rules in human-readable YAML files
- **Flexible Comparisons**: Support for exact matching and tolerance-based comparisons
- **Text Normalization**: Case-insensitive comparison and whitespace trimming
- **Type-Aware Validation**: Different rules for different data types
- **Comprehensive Reporting**: Detailed validation reports powered by pointblank
- **Row Count Validation**: Ensure datasets have the expected number of rows

## Dependencies

This package is built on top of the excellent [pointblank](https://github.com/rstudio/pointblank) package, which provides the robust data validation engine.  
Pointblank handles all the heavy lifting for validation logic, error reporting, and test execution.

## Configuration

Create a `rules.yaml` file with validation rules:

```yaml
version: 1
defaults:
  na_equal: yes
  ignore_columns: []
  keys: id
  label: reference comparison
row_validation:
  check_count: no
  expected_count: ~
  tolerance: 0.0
by_type:
  numeric:
    abs: 1.0e-09
    rel: 0
  integer:
    abs: 0
  character:
    equal_mode: exact
    case_insensitive: no
    trim: no
  date:
    equal_mode: exact
  datetime:
    equal_mode: exact
  logical:
    equal_mode: exact
by_name:
  id: []
  amount: []
  category: []
```

### Numeric tolerance: formula, edge effects, and best practices

#### Threshold formula

For numeric columns, validation relies on a single **combined threshold**:

```
threshold  = abs + rel × |reference_value|
result = OK  if  |candidate - reference| ≤ threshold
```

The two parameters **add up**—they are not two independent guards.

| Parameter | Role | Default value |
|-----------|------|---------------|
| `abs` | Absolute tolerance: fixed floor, independent of the magnitude of values | `1e-9` |
| `rel` | Relative tolerance: fraction of the reference value added to the threshold | `0` |

#### Pure absolute mode (recommended by default)

With `rel: 0`, the threshold is **constant** regardless of the magnitude of the values being compared:

```yaml
by_type:
  numeric:
    abs: 0.01   # fixed threshold: any difference > 0.01 is detected
    rel: 0
```

| Reference | Candidate | Difference | Threshold | Result |
|-----------|----------:|-----------:|----------:|--------|
| 1.00 | 1.005 | 0.005 | 0.01 | OK |
| 1 000 000.00 | 1 000 000.005 | 0.005 | 0.01 | OK |
| 0.000001 | 0.000001 + 0.005 | 0.005 | 0.01 | OK |
| 1.00 | 1.02 | 0.02 | 0.01 | **ERROR** |
| 1 000 000.00 | 1 000 000.02 | 0.02 | 0.01 | **ERROR** |

The same difference of `0.02` is detected whether you compare thousandths or millions.

#### Pure relative mode

With `abs: 0`, the threshold is **proportional** to the reference value:

```yaml
by_type:
  numeric:
    abs: 0      # no fixed floor
    rel: 0.01   # tolerance of 1% of the reference value
```

| Reference | Candidate | Difference | Threshold (1%) | Result |
|-----------|----------:|-----------:|---------------:|--------|
| 100.00 | 100.50 | 0.50 | 1.00 | OK |
| 1 000 000.00 | 1 005 000.00 | 5 000 | 10 000 | OK |
| 0.00 | 0.001 | 0.001 | **0** | **ERROR** (implicit division by zero) |

> **Warning**: if the reference value is `0`, the relative threshold is `0`—any difference, even tiny, will be detected as an error.  
> This is why `abs` acts as a safety floor.

#### Mixed mode: when and how to use it

Combining both parameters is useful when values can be close to zero **and** very large, and you want a tolerance that adapts in both cases:

```yaml
by_type:
  numeric:
    abs: 0.001   # floor: protects the ref ≈ 0 case
    rel: 0.01    # +1% for large values
```

For `ref = 1 000 000`: `threshold = 0.001 + 0.01 × 1 000 000 = 10 000.001`

> **Pitfall**: with `rel > 0` and large reference values, the threshold can become much wider than you intuitively expect.  
> For example, `rel: 1e-9` on a value of `12 000 000` yields a threshold of `≈ 0.012`, so a difference of `0.000565` would pass undetected, even with `abs: 1e-9`.  
>
> **Rule of thumb**: use `rel: 0` (the default) unless you explicitly need a tolerance proportional to the magnitude of the data.

### Character comparison options

For character columns, you can configure text normalization and comparison behavior:

- **equal_mode**: Comparison mode (`"exact"` or `"normalized"`)
- **case_insensitive**: Whether to ignore case differences (`true`/`false`)
- **trim**: Whether to trim whitespace before comparison (`true`/`false`)

**Example:**
```yaml
by_type:
  character:
    equal_mode: normalized      # Apply normalization before comparison
    case_insensitive: true      # Ignore case differences
    trim: true                  # Remove leading/trailing whitespace
```

For reference value `"Hello World"`:

- Candidate `"hello world"` **passes** with `case_insensitive: true`
- Candidate `"  Hello World  "` **passes** with `trim: true`
- Candidate `"HELLO WORLD"` **passes** with both options enabled
- Candidate `"Hello Universe"` **fails** regardless of normalization

### Row count validation

You can validate that datasets have an expected number of rows using the `row_validation` section:

- **check_count**: Whether to enable row count validation (`true`/`false`)
- **expected_count**: Expected number of rows (if `null`, uses the reference dataset row count)
- **tolerance**: Allowed deviation from the expected count

**Example:**
```yaml
row_validation:
  check_count: true
  expected_count: 1000
  tolerance: 50
```

This validates that the candidate dataset has between 950 and 1050 rows (1000 ± 50).

If `expected_count` is not specified, the reference dataset's row count is used as the expected value.

## Comparing Parquet files (large datasets)

`datadiff` can compare Parquet files that are too large to fit in RAM.
The recommended approach uses `arrow::open_dataset()` — **do not call
`arrow::to_duckdb()` yourself** before passing to `datadiff`; the package
handles the Arrow → DuckDB conversion internally with a single connection.

### Recommended strategy: Arrow Dataset (lazy, out-of-core)

```r
library(datadiff)
library(arrow)

ds_ref  <- arrow::open_dataset("path/to/reference/")
ds_cand <- arrow::open_dataset("path/to/candidate/")

# Generate a rules template from the schema (no data loaded)
write_rules_template(ds_ref, key = "ID", path = "rules.yaml")

# Compare — stays lazy until the final boolean slim table
result <- compare_datasets_from_yaml(
  data_reference = ds_ref,
  data_candidate = ds_cand,
  key = "ID",
  path = "rules.yaml"
)
```

Internally, `compare_datasets_from_yaml()`:

1. Opens a private DuckDB connection (`fresh_con`).
2. Materialises each Parquet dataset as a DuckDB physical temp table via
   `read_parquet()` — all memory is managed by DuckDB's buffer pool, so disk
   spilling works correctly.
3. Runs the full join + 125 boolean expressions as a single lazy SQL query.
4. `dplyr::compute()` materialises only the slim boolean result table
   (~125 logical columns × N rows) — the wide source data is never loaded
   into R.
5. `dplyr::collect()` brings the slim boolean table (~few GB) into R and
   passes a plain `data.frame` to pointblank — no live DuckDB connection
   needed during interrogation.
6. Disconnects and destroys `fresh_con` on exit.

### Memory tuning: `duckdb_memory_limit`

DuckDB's default memory cap (80 % of total RAM) can leave insufficient
headroom when R, Arrow, and the OS are already using significant memory.
The `duckdb_memory_limit` parameter controls how much RAM DuckDB may use
before spilling intermediate results to `tempdir()`:

```r
result <- compare_datasets_from_yaml(
  data_reference    = ds_ref,
  data_candidate    = ds_cand,
  key               = "ID",
  path              = "rules.yaml",
  duckdb_memory_limit = "8GB"   # default — safe for machines with >= 16 GB
)
```

| Machine RAM | Recommended `duckdb_memory_limit` | Notes |
|-------------|-----------------------------------|-------|
| 8 GB | `"3GB"` | Leaves room for R + OS |
| 16 GB | `"6GB"` | Balanced |
| 32 GB | `"8GB"` (default) | Spills when needed |
| 64 GB+ | `"20GB"` | Reduces spilling, faster |

Increasing the limit reduces disk I/O and speeds up the comparison; decreasing
it protects against OOM on memory-constrained machines. The limit only applies
when Arrow datasets are used — it has no effect for `data.frame` or `tbl_lazy`
inputs.

### Strategy comparison

| Strategy | Input type | RAM usage | Requires |
|----------|-----------|-----------|---------|
| **Arrow Dataset** ✅ recommended | `arrow::open_dataset()` | Slim boolean table only (~few GB) | arrow, duckdb |
| Arrow Table | `arrow::read_parquet(as_data_frame=FALSE)` | Same as Arrow Dataset | arrow, duckdb |
| Lazy table (dbplyr) | `tbl(con, "table_name")` | Slim boolean table only | DBI, dbplyr |
| `data.frame` | `read.csv()`, `readr::read_csv()`, etc. | Full data in RAM | — |

### What NOT to do

```r
# ❌ DO NOT convert to DuckDB yourself before passing to datadiff
ds_ref_duckdb  <- arrow::to_duckdb(ds_ref)   # creates Arrow's internal connection
ds_cand_duckdb <- arrow::to_duckdb(ds_cand)  # different connection!
result <- compare_datasets_from_yaml(ds_ref_duckdb, ds_cand_duckdb, ...)
# → cross-connection join fails in pointblank

# ❌ DO NOT collect before passing
ref_df <- dplyr::collect(ds_ref)   # loads full 4 GB into R RAM
```

### Disk spilling location

DuckDB spills to `tempdir()` when the memory limit is reached. On Windows
this is typically `C:\Users\<user>\AppData\Local\Temp`. Ensure that directory
has sufficient free disk space (up to ~2–3× the size of your Parquet files
in the worst case).

## Main Functions

- `write_rules_template()`: Generate a YAML rules template
- `read_rules()`: Load and validate rules from YAML
- `compare_datasets_from_yaml()`: Main validation function
- `detect_column_types()`: Infer column data types
- `preprocess_dataframe()`: Apply text normalization
- `analyze_columns()`: Compare column structures 

## Testing

Run the test suite:

```r
devtools::test()
```

## License

MIT License

## Contributing
