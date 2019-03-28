context("Simulation-Model Interface")

# Simulation-Model Interface ----------------------------------------------

test_that("Simulation fails when given a simulation_count", {
  data("mc_quantitative_scenarios")
  good_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  expect_error(run_simulation(good_scen, simulation_count = 10L), regexp = "iteration")
})

test_that("Simulation fails when not given a scenario object", {
  data("mc_quantitative_scenarios")
  bad_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  class(bad_scen) <- "list"
  expect_error(run_simulation(bad_scen, 10L), regexp = "object")
})

test_that("Simulation respects maximum ALE", {
  data("mc_quantitative_scenarios")
  good_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  results <- run_simulation(good_scen, 10L, ale_maximum = 100)
  expect_lte(max(results$ale), 100)
})

test_that("Missing mandatory OpenFAIR factors are detected", {
  data("mc_quantitative_scenarios")
  bad_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  bad_scen$parameters$tef <- NULL
  expect_error(run_simulation(bad_scen, 10L), regexp = "Missing")
})

test_that("Bad scenario parameters throw an error", {
  data("mc_quantitative_scenarios")
  bad_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  bad_scen$parameters$tef$func <- "stats::rlnorm"
  expect_error(run_simulation(bad_scen, 10L), regexp = "Error")
})

test_that("Simulating multiple scenarios succeeds", {
  data("mc_quantitative_scenarios")
  scenarios <- mc_quantitative_scenarios[1:3, ]$scenario
  results <- run_simulations(scenarios[[1]], scenarios[[2]], scenarios[[3]], iterations = 10L)
  expect_is(results, "list")
})

test_that("Multiple simulations run fails when not given a scenario object", {
  data("mc_quantitative_scenarios")
  bad_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  class(bad_scen) <- "list"
  expect_error(run_simulations(bad_scen, iterations = 10L), regexp = "object")
})

test_that("Multiple simulations deprecates the simulation_count parameters", {
  data("mc_quantitative_scenarios")
  good_scen <- mc_quantitative_scenarios[[1, "scenario"]]
  expect_error(run_simulations(good_scen, simulation_count = 10L), regexp = "iteration")
})
