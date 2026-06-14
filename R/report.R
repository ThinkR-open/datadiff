# Lazy pointblank-style report.
#
# The fast path keeps res$reponse a real interrogated agent (minimal when green,
# targeted when red) so pointblank::all_passed() and get_data_extracts() keep
# working. We prefix the class "datadiff_report" and attach the coverage so that
# PRINTING res$reponse builds, on demand, a full pointblank agent report (one
# step per column) from the pre-computed coverage - without re-running the slow
# per-column interrogation. The build cost is paid only when the report is
# actually displayed, and memoized so repeated prints are instant.

# Coverage column name that a real validation step maps to (undo the internal
# __ok / __eq / dummy-column naming used when the agent was built).
report_underlying_col <- function(col) {
  if (identical(col, "row_count_ok")) {
    return("<row_count>")
  }
  if (startsWith(col, "__missing_col_")) {
    return(sub("^__missing_col_", "", col))
  }
  if (startsWith(col, "__type_mismatch_")) {
    return(sub("^__type_mismatch_", "", col))
  }
  sub("__(ok|eq)$", "", col)
}

# Build a pointblank agent whose report mirrors the coverage table.
#
# When `real_agent` (the genuinely interrogated agent from the comparison) is
# supplied, the report is built ON TOP of it so that failing columns keep their
# REAL data extract (failing rows + CSV download) and the agent stays marked as
# interrogated. Its validation set is rebuilt to list every coverage row in
# order: the real step is kept (and relabelled) for columns it validated, and a
# synthetic passing row is added for the rest. Without `real_agent`, a purely
# synthetic count-only agent is produced (no extracts).
build_report_agent <- function(coverage, label, lang = "fr", locale = "fr_FR",
                                warn_at = 1e-14, stop_at = 1e-14,
                                real_agent = NULL) {
  n <- nrow(coverage)

  # Augment the real interrogated agent only when it has genuine failures
  # (real extracts to preserve). On an all-pass comparison the real agent is the
  # minimal placeholder (a single col_exists step); cloning its template would
  # mislabel every value check as col_exists, so fall through to the synthetic
  # col_vals_equal build instead.
  if (!is.null(real_agent) &&
      any(real_agent$validation_set$n_failed > 0, na.rm = TRUE)) {
    rvs <- real_agent$validation_set
    real_cols <- vapply(seq_len(nrow(rvs)), function(j) {
      report_underlying_col(rvs$column[[j]][1])
    }, character(1))

    # Extracts (the failing-row data + CSV) live in `agent$extracts`, a list
    # keyed by step index `i`. Reindexing the steps means remapping those keys.
    old_extracts <- real_agent$extracts %||% list()
    template <- rvs[1, , drop = FALSE]
    rows <- vector("list", n)
    new_extracts <- list()
    for (i in seq_len(n)) {
      col <- coverage$column[i]
      # Only value checks (tolerance / equality) map to a real step: that is
      # where the genuine row-level extract matters. col_exists and the
      # structural checks (missing_column, type_mismatch, row_count) are
      # synthesized from coverage - mapping them to a real step would pull in
      # per-row dummy results (e.g. n_failed = nrow) that contradict coverage.
      j <- if (coverage$check[i] %in% c("tolerance", "equality")) {
        match(col, real_cols)
      } else {
        NA_integer_
      }
      if (!is.na(j)) {
        # Genuine interrogated step: keep it, and carry its real extract over to
        # the new step index.
        row <- rvs[j, , drop = FALSE]
        old_key <- as.character(rvs$i[j])
        if (!is.null(old_extracts[[old_key]])) {
          new_extracts[[as.character(i)]] <- old_extracts[[old_key]]
        }
      } else {
        # Column the targeted agent did not validate (it passed): synthesise a
        # passing row from the real-row template.
        row <- template
        row$eval_error   <- FALSE
        row$eval_warning <- FALSE
        row$n        <- as.numeric(coverage$n[i])
        row$n_passed <- as.numeric(coverage$n[i] - coverage$n_failed[i])
        row$n_failed <- as.numeric(coverage$n_failed[i])
        row$f_passed <- if (coverage$n[i] > 0) row$n_passed / coverage$n[i] else 1
        row$f_failed <- if (coverage$n[i] > 0) coverage$n_failed[i] / coverage$n[i] else 0
        row$all_passed <- coverage$n_failed[i] == 0L
        row$warn   <- FALSE
        row$stop   <- FALSE
        row$notify <- FALSE
      }
      row$column <- list(coverage$column[i])
      row$label  <- coverage$check[i]
      rows[[i]] <- row
    }
    new_vs <- dplyr::bind_rows(rows)
    new_vs$i <- seq_len(nrow(new_vs))
    new_vs$assertion_type <- ifelse(coverage$check == "col_exists",
                                    "col_exists", "col_vals_equal")
    # Make every result column authoritative from coverage so the report is
    # consistent with res$coverage / res$summary and warn_at/stop_at apply
    # uniformly. The genuine row-level extracts are preserved separately in
    # new_extracts (keyed by the new step index).
    np <- coverage$n - coverage$n_failed
    new_vs$n            <- as.numeric(coverage$n)
    new_vs$n_passed     <- as.numeric(np)
    new_vs$n_failed     <- as.numeric(coverage$n_failed)
    new_vs$f_passed     <- ifelse(coverage$n > 0, np / coverage$n, 1)
    new_vs$f_failed     <- ifelse(coverage$n > 0, coverage$n_failed / coverage$n, 0)
    new_vs$all_passed   <- coverage$n_failed == 0L
    new_vs$warn         <- coverage$n_failed > 0L & new_vs$f_failed >= warn_at
    new_vs$stop         <- coverage$n_failed > 0L & new_vs$f_failed >= stop_at
    new_vs$notify       <- rep(FALSE, nrow(new_vs))
    new_vs$eval_error   <- rep(FALSE, nrow(new_vs))
    new_vs$eval_warning <- rep(FALSE, nrow(new_vs))
    real_agent$validation_set <- new_vs
    real_agent$extracts <- new_extracts
    return(real_agent)
  }

  # No real agent: synthetic count-only report.
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
  vs$assertion_type <- ifelse(coverage$check == "col_exists",
                              "col_exists", "col_vals_equal")
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
        stop_at  = attr(x, "datadiff_stop_at") %||% 1e-14,
        real_agent = x
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
    stop_at  = attr(reponse, "datadiff_stop_at") %||% 1e-14,
    real_agent = reponse
  )
  report <- pointblank::get_agent_report(agent)
  if (!is.null(file)) {
    pointblank::export_report(report, filename = file, quiet = TRUE)
  }
  invisible(report)
}
