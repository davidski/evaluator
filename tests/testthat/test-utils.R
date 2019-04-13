context("Utilities")
test_that("Missing packages are detected", {
  expect_error(check_availability(packages = c("ggplot2", "Missing"), func = "test"),
              "available: Missing")
})
test_that("Found packages are detected silently", {
  expect_silent(check_availability(packages = c("stats"), func = "test"))
})

test_that("Dollar Millions formats as expected", {
  expect_equal(dollar_millions(1.523 * 10^6), "$1.52M")
})

test_that("Calculate max losses", {
  data("mc_simulation_results")
  dat <- calculate_max_losses(mc_simulation_results, c(1, 10))
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat[dat$outliers == TRUE, ]), 1000)
  expect_equal(nrow(dat), 2000)
})

test_that("calculate_max_losses handles NULL outliers", {
  data("mc_simulation_results")
  dat <- calculate_max_losses(mc_simulation_results)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 1000)
})

test_that("identify_outliers() identifies the expected number of outliers", {
  data("mc_scenario_summary")
  dat <- identify_outliers(mc_scenario_summary)
  expect_equal(sum(dat$outlier), 4)
})
