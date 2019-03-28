context("Spreadsheet imports")
test_that("Default scenarios import", {
  data(mc_domains)
  scenarios <- import_scenarios(domains = mc_domains)
  expect_equal(nrow(scenarios), 56)
  expect_equal(length(scenarios), 8)
})
test_that("Scenarios import succeeds when using defaults", {
  data(mc_qualitative_scenarios)
  expect_equal(import_scenarios(), mc_qualitative_scenarios)
})
test_that("Scenarios import fails when given bad input file", {
  expect_error(import_scenarios(survey_file  = "/bad/nonexistant/file", "survey"))
})

test_that("Default capabilities import", {
  data(mc_domains)
  dat <- import_capabilities(domains = mc_domains)
  expect_equal(nrow(dat), 60)
  expect_equal(length(dat), 4)
})
test_that("Capabilities import fails when given bad input file", {
  expect_error(import_capabilities(survey_file  = "/bad/nonexistant/file", "survey"))
})
test_that("Capabilities import succeeds when using defaults", {
  data("mc_capabilities", envir = .GlobalEnv)
  src <- get("mc_capabilities", envir = .GlobalEnv)
  dat <- import_capabilities()
  expect_equal(dat, src)
})

test_that("Higher-level import_spreadsheet functions", {
  dat <- import_spreadsheet(output_dir = tempdir())
  expect_equal(nrow(dat), 2)
})
test_that("Spreadsheet import fails when given bad output directory", {
  expect_error(import_spreadsheet(output_dir  = "/bad/nonexistant/path", "output"))
})
