context("Summariziation")
test_that("Simulation summary", {
  data("simulation_results")
  data("scenario_summary")
  dat <- summarize_scenarios(simulation_results)
  expect_identical(dat, scenario_summary)
})

test_that("Domain summary", {
  data("simulation_results")
  data("domains")
  data("domain_summary")
  expect_identical(summarize_domains(simulation_results, domains), domain_summary)
})
