context("Analysis Script")

tmpdir <- tempdir()
tmpwork <- file.path(tmpdir, "analysis")

test_that("Minimum Viable Analysis script works", {

  # this is expensive to run, don't run it on CRAN or CI
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  create_templates(tmpwork)
  base_dir <- tmpwork
  expect_success(source(system.file("run_analysis.R", package = "evaluator")))
})

unlink(tmpwork, recursive = TRUE)
