# Internal naming conventions shared by the boolean producers (tolerance.R,
# compare_datasets_from_yaml.R), the verdict consumers (fast_path.R,
# coverage.R) and the report step mapping (report.R, pointblank_setup.R).
# They live in one place because a drift on one side breaks the mapping
# SILENTLY: the report step is simply not found and its extract is lost.

# Suffix of the per-column boolean produced for a tolerance column.
datadiff_suffix_ok <- "__ok"

# Suffix of the per-column boolean produced for an equality column.
datadiff_suffix_eq <- "__eq"

# Prefix of the dummy column carrying a missing-column failure step.
datadiff_prefix_missing_col <- "__missing_col_"

# Prefix of the dummy column carrying a type-mismatch failure step.
datadiff_prefix_type_mismatch <- "__type_mismatch_"

# Column name helpers: the only supported way to build these names.
# Length-guarded: paste0(character(0), suffix) yields the bare suffix
# (recycle0 is FALSE by default), a phantom column name.
datadiff_ok_col <- function(col) {
  if (length(col) == 0) {
    return(character(0))
  }
  paste0(col, datadiff_suffix_ok)
}
datadiff_eq_col <- function(col) {
  if (length(col) == 0) {
    return(character(0))
  }
  paste0(col, datadiff_suffix_eq)
}

# Default pointblank action levels (fractions of failing rows). Used as the
# internal fallback everywhere a threshold is reconstructed from attributes;
# the public compare_datasets_from_yaml() signature documents the same value.
datadiff_default_warn_at <- 1e-14
datadiff_default_stop_at <- 1e-14
