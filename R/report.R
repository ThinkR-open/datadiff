# Lazy pointblank-style report.
#
# The fast path keeps res$reponse a real interrogated agent (minimal when green,
# targeted when red) so pointblank::all_passed() and get_data_extracts() keep
# working. We prefix the class "datadiff_report" and attach the coverage so that
# PRINTING res$reponse builds, on demand, a full pointblank agent report (one
# step per column) from the pre-computed coverage - without re-running the slow
# per-column interrogation. The build cost is paid only when the report is
# actually displayed, and memoized so repeated prints are instant.

# Build a pointblank agent whose report mirrors the coverage table, populating
# the interrogation result columns directly (no interrogate() scan).
build_report_agent <- function(coverage, label, lang = "fr", locale = "fr_FR",
                                warn_at = 1e-14, stop_at = 1e-14) {
  n <- nrow(coverage)
  dummy_ncol <- max(n, 1L)
  dummy <- as.data.frame(
    matrix(TRUE, nrow = 1L, ncol = dummy_ncol)
  )
  names(dummy) <- paste0("col", seq_len(dummy_ncol))

  agent <- pointblank::create_agent(
    tbl = dummy, label = label,
    actions = pointblank::action_levels(warn_at = warn_at, stop_at = stop_at),
    lang = lang, locale = locale
  )
  if (n == 0L) {
    return(agent)
  }

  agent <- agent %>%
    pointblank::col_vals_equal(columns = tidyselect::everything(), value = TRUE)

  vs <- agent$validation_set
  n_passed <- coverage$n - coverage$n_failed
  vs$column       <- as.list(coverage$column)
  vs$label        <- coverage$check
  vs$eval_error   <- rep(FALSE, n)
  vs$eval_warning <- rep(FALSE, n)
  vs$n            <- as.numeric(coverage$n)
  vs$n_passed     <- as.numeric(n_passed)
  vs$n_failed     <- as.numeric(coverage$n_failed)
  vs$f_passed     <- ifelse(coverage$n > 0, n_passed / coverage$n, 1)
  vs$f_failed     <- ifelse(coverage$n > 0, coverage$n_failed / coverage$n, 0)
  vs$all_passed   <- coverage$n_failed == 0L
  vs$warn         <- coverage$n_failed > 0L & vs$f_failed >= warn_at
  vs$stop         <- coverage$n_failed > 0L & vs$f_failed >= stop_at
  vs$notify       <- rep(FALSE, n)
  agent$validation_set <- vs
  agent
}

# Attach the lazy-report capability to a real interrogated agent: prefix the
# class (print dispatches to print.datadiff_report) and stash what is needed to
# render the full report, plus a (reference-semantics) environment for caching.
as_datadiff_report <- function(reponse, coverage, label, lang, locale,
                               warn_at = 1e-14, stop_at = 1e-14) {
  attr(reponse, "datadiff_coverage") <- coverage
  attr(reponse, "datadiff_label")    <- label
  attr(reponse, "datadiff_lang")     <- lang
  attr(reponse, "datadiff_locale")   <- locale
  attr(reponse, "datadiff_warn_at")  <- warn_at
  attr(reponse, "datadiff_stop_at")  <- stop_at
  attr(reponse, "datadiff_render")   <- new.env(parent = emptyenv())
  class(reponse) <- c("datadiff_report", class(reponse))
  reponse
}

# Build (once) and memoize the pointblank agent report for a datadiff_report.
datadiff_render_report <- function(x) {
  cache <- attr(x, "datadiff_render")
  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
  }
  if (is.null(cache$report)) {
    cache$report <- pointblank::get_agent_report(
      build_report_agent(
        coverage = attr(x, "datadiff_coverage"),
        label    = attr(x, "datadiff_label") %||% "datadiff report",
        lang     = attr(x, "datadiff_lang") %||% "fr",
        locale   = attr(x, "datadiff_locale") %||% "fr_FR",
        warn_at  = attr(x, "datadiff_warn_at") %||% 1e-14,
        stop_at  = attr(x, "datadiff_stop_at") %||% 1e-14
      )
    )
  }
  cache$report
}

#' Print method for a lazy datadiff report
#'
#' Prints an instant textual coverage summary, then renders the full
#' pointblank-style report (HTML in interactive sessions / viewer). The report
#' is built on first print and memoized.
#'
#' @param x A `datadiff_report` (the `reponse` element of a comparison result).
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print datadiff_report
print.datadiff_report <- function(x, ...) {
  coverage <- attr(x, "datadiff_coverage")
  if (!is.null(coverage)) {
    print(coverage)
  }
  print(datadiff_render_report(x))
  invisible(x)
}

#' Render a comparison result as a pointblank HTML report
#'
#' Builds a full pointblank-style report (one validation step per column) from
#' the comparison coverage, populated with the pre-computed pass/fail counts -
#' without re-running the slow per-column interrogation. Optionally writes it to
#' an HTML file.
#'
#' @param res The list returned by [compare_datasets_from_yaml()].
#' @param file Optional path to write the report to as a self-contained HTML
#'   file (via [pointblank::export_report()]). When `NULL`, nothing is written.
#' @return The pointblank agent report object, invisibly.
#' @export
datadiff_report_html <- function(res, file = NULL) {
  coverage <- res$coverage
  if (is.null(coverage)) {
    stop("`res` has no `coverage`; was it produced by compare_datasets_from_yaml()?")
  }
  reponse <- res$reponse
  agent <- build_report_agent(
    coverage = coverage,
    label    = attr(reponse, "datadiff_label") %||% "datadiff report",
    lang     = attr(reponse, "datadiff_lang") %||% "fr",
    locale   = attr(reponse, "datadiff_locale") %||% "fr_FR",
    warn_at  = attr(reponse, "datadiff_warn_at") %||% 1e-14,
    stop_at  = attr(reponse, "datadiff_stop_at") %||% 1e-14
  )
  report <- pointblank::get_agent_report(agent)
  if (!is.null(file)) {
    pointblank::export_report(report, filename = file, quiet = TRUE)
  }
  invisible(report)
}
