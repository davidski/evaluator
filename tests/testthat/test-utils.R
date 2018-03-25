context("Utilities")
test_that("Missing packages are detected", {
  expect_error(check_availability(packages = c("Missing"), func = "test"))
})
test_that("Found packages are detected silently", {
  expect_silent(check_availability(packages = c("stats"), func = "test"))
})

test_that("Dollar Millions formats as expected", {
  expect_equal(dollar_millions(1.523 * 10^6), "$1.52M")
})

test_that("Domain TC vs DIFF calculations", {
  data("simulation_results")
  data("domains")
  dat <- calculate_weak_domains(simulation_results, domains)
  expect_equal(nrow(dat), nrow(domains))
})

test_that("Calculate max losses", {
  data("simulation_results")
  dat <- calculate_max_losses(simulation_results, c(1, 10))
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat[dat$outliers == TRUE,]), 1000)
  expect_equal(nrow(dat), 2000)
})

test_that("Calculate max losses", {
  data("simulation_results")
  data("domains")
  dat <- calculate_domain_impact(domain_summary, domains)
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), nrow(domains))
})
