library(testthat)
library(evaluator)

# ensure phantom.js is available on CI platforms
if (Sys.getenv("NOT_CRAN", "") != "" || Sys.getenv("CI", "") != "") {
  if (!shinytest::dependenciesInstalled()) shinytest:::installDependencies()
  message("Using phantom.js from ", shinytest:::find_phantom(), "\n")
}

test_check("evaluator")
