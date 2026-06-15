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

find_duplicate_keys <- function(data, key) {
  if (is_non_local(data)) {
    dups <- data %>%
      dplyr::count(dplyr::across(dplyr::all_of(key))) %>%
      dplyr::filter(n > 1L) %>%
      dplyr::collect()
    if (nrow(dups) == 0L) {
      return(NULL)
    }
    return(list(
      n_dup_keys = nrow(dups),
      n_dup_rows = sum(dups$n),
      examples   = format_key_examples(dups[, key, drop = FALSE], key)
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
