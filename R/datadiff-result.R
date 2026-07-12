# Accessors for the result of compare_datasets_from_yaml().
#
# The result is a plain named list of class `datadiff_result`. The class only
# exists to keep the historical `reponse` field name readable while user code
# migrates to `response`: `$` and `[[` redirect the old name to the new one
# with a deprecation warning, and behave like plain list access otherwise.

new_datadiff_result <- function(fields) {
  structure(fields, class = "datadiff_result")
}

warn_reponse_deprecated <- function() {
  rlang::warn(
    message = paste(
      "The `reponse` field is deprecated as of {datadiff} 0.6.0:",
      "use `response` instead.",
      "`reponse` will be removed in a future release."
    ),
    .frequency = "once",
    .frequency_id = "datadiff_reponse_deprecated",
    class = "datadiff_deprecated_reponse_warning"
  )
}

#' @export
#' @noRd
`$.datadiff_result` <- function(x, name) {
  if (identical(name, "reponse")) {
    warn_reponse_deprecated()
    name <- "response"
  }
  # exact = FALSE replicates the silent partial matching of `$` on plain lists
  unclass(x)[[name, exact = FALSE]]
}

#' @export
#' @noRd
`[[.datadiff_result` <- function(x, i, ...) {
  if (identical(i, "reponse")) {
    warn_reponse_deprecated()
    i <- "response"
  }
  unclass(x)[[i, ...]]
}

#' @export
#' @noRd
print.datadiff_result <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}
