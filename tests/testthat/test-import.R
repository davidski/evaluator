context("Spreadsheet imports")
test_that("Default scenarios import", {
  data(domains)
  scenarios <- import_scenarios(domains = domains)
  expect_equal(nrow(scenarios), 56)
  expect_equal(length(scenarios), 8)
})

test_that("Default capabilities import", {
  data(domains)
  dat <- import_capabilities(domains = domains)
  expect_equal(nrow(dat), 60)
  expect_equal(length(dat), 4)
})

test_that("Higher-level import_spreadsheet functions", {
  dat <- import_spreadsheet(output_dir = tempdir())
  expect_equal(nrow(dat), 2)
})
