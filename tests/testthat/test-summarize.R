context("Summariziation")
test_that("Simulation summary", {
  data("simulation_results")
  data("scenario_summary")
  dat <- summarize_scenarios(simulation_results)
  expect_equivalent(dat, scenario_summary)
})

test_that("Domain summary", {
  data("simulation_results")
  data("domains")
  data("domain_summary")
  expect_equivalent(summarize_domains(simulation_results, domains), domain_summary)
})

test_that("Domain impact", {
  data("domain_summary")
  data("domains")
  dat <- calculate_domain_impact(domain_summary, domains)
  expect_equal(nrow(dat), nrow(domains))
})