# datadiff_report_html(extracts_dir =): the failing-row extracts are also
# written as plain CSV files. The HTML report's CSV buttons are data-URI
# downloads, which some viewers (Positron / Posit Workbench webview) block:
# real files on disk are the robust alternative.

test_that("extracts_dir writes one CSV per failing step", {
  ref  <- data.frame(id = 1:3, x = c(1, 2, 3), s = c("a", "b", "c"))
  cand <- data.frame(id = 1:3, x = c(1, 2, 9), s = c("a", "ZZ", "c"))
  res <- suppressMessages(compare_datasets_from_yaml(ref, cand, key = "id"))
  expect_false(res$all_passed)

  out_dir <- tempfile(pattern = "datadiff_extracts_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  datadiff_report_html(res, file = NULL, extracts_dir = out_dir)

  csvs <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
  expect_length(csvs, 2L)                       # x (tolerance) + s (equality)
  expect_true(any(grepl("_x\\.csv$", csvs)))
  expect_true(any(grepl("_s\\.csv$", csvs)))

  # Content: the x extract carries the failing row (id 3)
  x_csv <- utils::read.csv(csvs[grepl("_x\\.csv$", csvs)])
  expect_true(3 %in% x_csv$id)
})

test_that("extracts_dir is not created on an all-pass comparison", {
  ref <- data.frame(id = 1:3, x = c(1, 2, 3))
  res <- suppressMessages(compare_datasets_from_yaml(ref, ref, key = "id"))
  expect_true(res$all_passed)

  out_dir <- tempfile(pattern = "datadiff_extracts_")
  datadiff_report_html(res, file = NULL, extracts_dir = out_dir)
  expect_false(dir.exists(out_dir))
})

test_that("column names are sanitized in extract file names", {
  ref  <- data.frame(id = 1:2, x = c(1, 2))
  names(ref)[2] <- "montant (EUR)/annee"
  cand <- ref
  cand[[2]][2] <- 99
  res <- suppressWarnings(suppressMessages(
    compare_datasets_from_yaml(ref, cand, key = "id")
  ))
  expect_false(res$all_passed)

  out_dir <- tempfile(pattern = "datadiff_extracts_")
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  datadiff_report_html(res, file = NULL, extracts_dir = out_dir)

  csvs <- list.files(out_dir, pattern = "\\.csv$")
  expect_length(csvs, 1L)
  expect_false(grepl("[ ()/]", csvs))
})
