context("Spreadsheet imports")
test_that("Default scenarios import", {
  data(domains)
  scenarios <- import_scenarios(domains = domains)
  expect_equal(nrow(scenarios), 56)
  expect_equal(length(scenarios), 8)
})
