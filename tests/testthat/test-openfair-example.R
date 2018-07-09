context("openfair-example")

library(shinytest)

test_that("openfairExample() works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "evaluator", "openfair_example")
  expect_pass(testApp(appdir, compareImages = FALSE))
})
