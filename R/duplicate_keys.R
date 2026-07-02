# Detect duplicate key values and summarise them for the user warning.
#
# Returns NULL when the key is unique, otherwise a list with:
#   n_dup_keys : number of distinct key values appearing more than once
#   n_dup_rows : total number of rows carrying a duplicated key
#   examples   : up to 3 formatted key values ("col = val, ...") plus "..."
#
# Local data.frames use anyDuplicated()/duplicated() (a single hashed pass,
# orders of magnitude faster than a dplyr group_by/count on wide tables); lazy
# tables keep the SQL-native count()/group_by (cheap inside the database).

format_key_examples <- function(uniq_keys, key) {
  rows <- unname(apply(uniq_keys, 1, function(r) {
    paste(key, "=", r, collapse = ", ")
  }))
  if (length(rows) <= 3) {
    rows
  } else {
    c(rows[1:3], "...")
  }
}

#' @importFrom rlang .data
#' @noRd
find_duplicate_keys <- function(data, key) {
  if (is_non_local(data)) {
    # Reserved count name: count()'s default "n" collides with a user column
    # named "n" (count() then stores its result in "nn", and a key named "n"
    # would be filtered on its own values instead of the count). The reserved
    # name is extended until it differs from every key column, because
    # count(name = <grouping column>) silently REPLACES that grouping column
    # with the count.
    count_col <- "..datadiff_n"
    while (count_col %in% key) {
      count_col <- paste0(count_col, "_")
    }
    duplicated_groups <- data %>%
      dplyr::count(dplyr::across(dplyr::all_of(key)), name = count_col) %>%
      dplyr::filter(.data[[count_col]] > 1L)
    # Aggregate in SQL: only 2 scalars plus at most 3 example groups cross the
    # wire, instead of every duplicated group (potentially millions).
    agg <- duplicated_groups %>%
      dplyr::summarise(
        ..datadiff_dup_keys = dplyr::n(),
        ..datadiff_dup_rows = sum(.data[[count_col]], na.rm = TRUE)
      ) %>%
      dplyr::collect()
    n_dup_keys <- as.numeric(agg$..datadiff_dup_keys)
    if (n_dup_keys == 0) {
      return(NULL)
    }
    example_groups <- duplicated_groups %>%
      utils::head(3L) %>%
      dplyr::collect()
    examples <- format_key_examples(example_groups[, key, drop = FALSE], key)
    if (n_dup_keys > 3) {
      examples <- c(examples, "...")
    }
    return(list(
      n_dup_keys = n_dup_keys,
      n_dup_rows = as.numeric(agg$..datadiff_dup_rows),
      examples   = examples
    ))
  }

  k <- data[, key, drop = FALSE]
  if (anyDuplicated(k) == 0L) {
    return(NULL)
  }
  is_dup    <- duplicated(k) | duplicated(k, fromLast = TRUE)
  dup_keys  <- k[is_dup, , drop = FALSE]
  uniq_keys <- dup_keys[!duplicated(dup_keys), , drop = FALSE]
  list(
    n_dup_keys = nrow(uniq_keys),
    n_dup_rows = sum(is_dup),
    examples   = format_key_examples(uniq_keys, key)
  )
}
