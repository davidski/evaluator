context("Summarization")
test_that("Simulation summary", {
  data("mc_simulation_results")
  data("mc_scenario_summary")
  summarized_scenarios <- summarize_scenarios(mc_simulation_results)
  expect_equivalent(as.data.frame(summarized_scenarios),
                    as.data.frame(mc_scenario_summary),
                    tolerance = .01)
})
test_that("Scenario summary rejects non dataframe inputs", {
  data("mc_simulation_results")
  expect_error(summarize_scenario(mc_simulation_results$results), regexp = "dataframe")
})

test_that("Simulation summary handles NAs for tc/diff exceedance", {
  data("mc_simulation_results")
  mc_simulation_results[[1, "results"]][[1]]$mean_tc_exceedance <- NA
  #simulation_results[1, "mean_tc_exceedance"] <- NA
  mc_simulation_results[[10, "results"]][[1]]$mean_diff_exceedance <- NA
  dat <- mutate(mc_simulation_results,
                result_summary = map(results, summarize_scenario)) %>%
    select(-results)
  summarized_tc_exceedance <- dplyr::filter(dat, scenario_id == "RS-18") %>%
    tidyr::unnest(result_summary) %>%
    dplyr::pull(mean_tc_exceedance)
  expect_gt(summarized_tc_exceedance, 0)
})

test_that("Iteration-level summary", {
  data("mc_simulation_results")
  summarized_iterations <- summarize_iterations(mc_simulation_results$results)
  expect_lte(max(summarized_iterations$mean_tc_exceedance), 1)
  expect_gte(min(summarized_iterations$mean_tc_exceedance), 0)
  expect_lte(max(summarized_iterations$mean_diff_exceedance), 1)
  expect_gte(min(summarized_iterations$mean_diff_exceedance), 0)
})

test_that("Domain summary", {
  data("mc_simulation_results")
  data("mc_domain_summary")

  summarized_domains <- summarize_domains(mc_simulation_results)

  expect_equivalent(as.data.frame(summarized_domains),
                    as.data.frame(mc_domain_summary), tolerance = 0.01)
})
test_that("Domain summary rejects non dataframe inputs", {
  data("mc_simulation_results")
  expect_error(summarize_domains(mc_simulation_results$results), regexp = "dataframe")
})

test_that("Summarize to disk", {
  tmpdata <- file.path(tempdir(), "data")
  dir.create(tmpdata)

  result <- summarize_to_disk(evaluator::mc_simulation_results,
                              results_dir = tmpdata)
  expect_equal(nrow(result), 2)
  unlink(tmpdata, recursive = TRUE)
})

test_that("Summarize to disk - non-existant directory", {
  tmpdata <- file.path(tempdir(), "data")

  result <- summarize_to_disk(evaluator::mc_simulation_results,
                              results_dir = tmpdata)
  expect_equal(nrow(result), 2)
  unlink(tmpdata, recursive = TRUE)
})

