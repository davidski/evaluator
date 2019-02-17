context("Scenario Explorer")
# This file is for testing the applications in the apps/ directory.

library(shinytest)


test_that("explore_scenarios() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # unsure how to pass parameters to a shiny flexdashboard
  skip("Shinytest scaffolding is not complete")

  # prep the test data files
  tmpdir <- tempdir()
  tmpdata <- file.path(tmpdir, "data")
  dir.create(tmpdata)
  tmpinputs <- file.path(tmpdir, "inputs")
  dir.create(tmpinputs, showWarnings = FALSE)

  data("simulation_results", package = "evaluator", envir = environment())
  saveRDS(simulation_results, file = file.path(tmpdata, "simulation_results.Rds"))

  res <- c("risk_tolerances.csv") %>%
    purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                           tmpinputs))

  data("quantitative_scenarios", envir = environment())
  saveRDS(quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.Rds"))

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "evaluator", "explore_scenarios")
  expect_pass(testApp(appdir, compareImages = FALSE))
  unlink(c(tmpdata, tmpinputs), recursive = TRUE)
})

