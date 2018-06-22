context("Scenario Explorer")
# This file is for testing the applications in the apps/ directory.

library(shinytest)

test_that("explore_scenarios() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # shiny testing is not quite working yet
  skip("Shinytest scaffolding is not complete")

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "evaluator", "explore_scenarios")
  expect_pass(testApp(appdir, compareImages = FALSE))
})
