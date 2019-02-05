context("Summarization")
test_that("Simulation summary", {
  data("simulation_results")
  data("scenario_summary")
  summarized_scenarios <- summarize_scenarios(simulation_results)
  expect_equivalent(as.data.frame(summarized_scenarios),
                    as.data.frame(scenario_summary),
                    tolerance = .01)
})

test_that("Simulation summary handles NAs for tc/diff exceedance", {
  data("simulation_results")
  simulation_results[1, "mean_tc_exceedance"] <- NA
  simulation_results[simulation_results$scenario_id == 18 &
                       simulation_results$simulation == 1, "mean_diff_exceedance"] <- NA
  dat <- summarize_scenarios(simulation_results)
  expect_gt(dat[[54,"mean_tc_exceedance"]], 0)
})

test_that("Domain summary", {
  data("simulation_results")
  data("domains")
  data("domain_summary")

  summarized_domains <- summarize_domains(simulation_results)

  expect_equivalent(as.data.frame(summarized_domains),
                    as.data.frame(domain_summary), tolerance = 0.01)
})

test_that("Summarize to disk", {
  tmpdata <- file.path(tempdir(), "data")
  dir.create(tmpdata)

  result <- summarize_to_disk(evaluator::simulation_results, evaluator::domains,
                              results_dir = tmpdata)
  expect_equal(nrow(result), 2)
  unlink(tmpdata, recursive = TRUE)
})
