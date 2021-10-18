context("OpenFAIR Example Shiny app")

library(shinytest)

test_that("openfairExample() works", {
  skip("Shiny testing is awkward, consider moving to stand alone pkg")

  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  skip_if_not(rmarkdown::pandoc_available(),
              message = "Cannot run shinytest without pandoc available.")

  # Don't run on r-devel, which behaves badly on TravisCI
  skip_if(grepl("unstable", R.version.string),
          message = "Shinytest gives unpredictable results on R-devel")

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "evaluator", "openfair_example")

  expect_pass(testApp(appdir, compareImages = FALSE))
})
