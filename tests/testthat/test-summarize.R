context("Summarization")
test_that("Simulation summary", {
  data("simulation_results")
  data("scenario_summary")
  dat <- summarize_scenarios(simulation_results)
  expect_equivalent(as.data.frame(dat), as.data.frame(scenario_summary))
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

test_that("Summarize to disk", {
  tmpdata <- file.path(tempdir(), "data")
  dir.create(tmpdata)

  result <- summarize_to_disk(evaluator::simulation_results, evaluator::domains,
                              results_dir = tmpdata)
  expect_equal(nrow(result), 2)
  unlink(tmpdata, recursive = TRUE)
})
