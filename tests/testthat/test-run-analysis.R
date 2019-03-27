context("Analysis Script")

tmpdir <- tempdir()
tmpwork <- file.path(tmpdir, "analysis")

test_that("Minimum Viable Analysis script works", {

  # this is expensive to run, skip if the EVALUATOR_TEST_MVA envvar is not set
  skip_if_not(Sys.getenv("EVALUATOR_TEST_MVA") == TRUE,
              "Not running MVA test - EVALUATOR_TEST_MVA not set")

  create_templates(tmpwork)
  base_dir <- tmpwork
  source(system.file("run_analysis.R", package = "evaluator"), local = TRUE)
  expect_equivalent(file.exists(file.path(base_dir, "results", "risk_dashboard.html")), TRUE)
  expect_equivalent(file.exists(file.path(base_dir, "results", "risk_report.docx")), TRUE)
})

unlink(tmpwork, recursive = TRUE)
