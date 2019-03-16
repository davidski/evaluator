test_that("Sample TEF", {
  set.seed(1234)
  tef <- sample_tef(n = 10, params = list(1, 10, 100))
  expect_is(tef, "list")
  # ensure that the list has the required elements
  expect_equal(names(tef), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(tef$samples), 10)
  # ensure that TEF values are returned as integers
  expect_is(tef$samples, "integer")
  # ensure that values of samples is correct
  expect_equal(unlist(tef$samples),
               c(7, 30, 2, 34, 36, 13, 14, 14, 9, 15))
})

context("Sample DIFF")
test_that("Sample DIFF", {
  set.seed(1234)
  dat <- sample_diff(n = 10, params = list(50, 70, 75, 3))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equal(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(dat$samples), 10)
  # ensure that values of samples is correct
  expect_equal(signif(unlist(dat$samples), digits = 4),
               signif(c(72.5519454551502, 65.1852603020272, 59.1564180836877,
                        74.5816023178688, 64.1192226440207, 63.561355776164,
                        70.1284833577168, 69.9960887031119, 70.0802721600923,
                        71.4683219144408), digits = 4))
})
test_that("Multi control diff works", {
  set.seed(1234)
  diff_estimates <- list(list(min = 1, mode = 10, max = 20, shape = 1),
                         list(min =  2, mode = 15, max = 100, shape = 3))
  dat <- map(diff_estimates, ~sample_diff(n = 10, params = .x))
  expect_is(dat, "list")
  # ensure that we received two responses back
  expect_equal(length(dat), 2)
  # ensure that each list has the required elements
  expect_equal(names(dat[[1]]), c("type", "samples", "details"))
  expect_equal(names(dat[[2]]), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(map(dat, "samples") %>% purrr::as_vector()), 20)
  # ensure that values of samples is correct
  expect_equal(signif(map(dat, "samples") %>% purrr::as_vector(), digits = 4),
               signif(c(3.90393636733577, 12.0433334741972, 16.4016497257758,
                        1.40316316329966, 12.9556613644064, 13.407051718503,
                        6.90857118330148, 7.06525326308045, 6.96573684805713,
                        5.27395602706608, 19.325236233819, 11.5371092097717,
                        14.4718568402617, 30.9888681331648, 57.8265729409247,
                        26.8078941229072, 18.7219717579054, 12.6241144280687,
                        13.6123515927865, 32.76590164781
               ), digits = 4))
})

context("Sample TC")
test_that("Sample TC", {
  set.seed(1234)
  tc <- sample_tc(n = 10, params = list(50, 75, 100, 4))
  expect_is(tc, "list")
  # ensure that the list has the required elements
  expect_equal(names(tc), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(tc$samples), 10)
  # ensure that values of samples is correct
  expect_equal(signif(unlist(tc$samples), digits = 4),
               signif(c(61.7026564773373, 78.188740471894, 87.0623477417219,
                        53.1987199785052, 79.9184628308895, 80.7889924652588,
                        68.4387021948896, 68.7541469869603, 68.554057026653,
                        64.9764652390671), digits = 4))
})

context("Select Loss Opportunities")
test_that("Mean Difficulty Exceedance works when there are zero losses", {
  threat_strengths <- c(0.2, 0.3, 0.4)
  diff_strengths   <- c(0.3, 0.4, 0.5)
  dat <- select_loss_opportunities(threat_strengths, diff_strengths)
  expect_equal(dat$details$mean_diff_exceedance, 0.1)
})

context("Sample VULN")
test_that("Sample VULN works with binom", {
  set.seed(1234)
  dat <- sample_vuln(n = 10, params = list(1, .5))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equal(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(dat$samples), 10)
  # ensure that values of samples is correct
  expect_equal(sum(dat$samples), 7)
})
test_that("Sample VULN works with TC and DIFF", {
  set.seed(1234)
  tc <- sample_tc(n = 10, params = list(50, 70, 85, 2))$samples
  diff <- sample_diff(n = 10, params = list(50, 70, 85, 2))$samples
  dat <- sample_vuln(n = 10, .func = "evaluator::select_loss_opportunities", params = list(tc = tc, diff = diff))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equivalent(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equivalent(length(dat$samples), 10)
  # ensure that values of samples is correct
  expect_equivalent(sum(dat$samples), 5)
  # ensure that mean_tc_exceedance is set correctly
  expect_equivalent(floor(dat$details$mean_tc_exceedance), 7)
  # ensure that mean_diff_exceedance is set correctly
  expect_equivalent(floor(dat$details$mean_diff_exceedance), 8)
})
test_that("TC and DIFF exceedance handles NA threat events", {
  set.seed(1234)
  tc <- c(NA)
  diff <- sample_diff(n = 2, params = list(50, 70, 85, 2))$samples
  dat <- sample_vuln(n = 2, .func = "evaluator::select_loss_opportunities", params = list(tc = tc, diff = diff))
  expect_is(dat, "list")
  # ensure that mean_tc_exceedance is set correctly
  expect_equivalent(dat$details$mean_tc_exceedance, NA)
  # ensure that mean_diff_exceedance is set correctly
  expect_equivalent(dat$details$mean_diff_exceedance, NA)
})

context("Sample LM")
test_that("Sample LM", {
  set.seed(1234)
  lm <- sample_lm(n = 10, params = list(min = 1*10^4, mode = 5*10^4,
                                max = 1*10^7, shape = 3))
  expect_is(lm, "list")
  # ensure that the list has the required elements
  expect_equal(names(lm), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(lm$samples), 10)
  # ensure that values of samples is correct
  expect_equal(signif(unlist(lm$samples), digits = 4),
               signif(c(332422.727880636, 2831751.79415706, 35602.2608120876,
                        3349352.73654269, 3632631.71769846, 927503.010814968,
                        966756.805719722, 941718.366417413, 569057.598433507,
                        1069488.76293628), digits = 4))
})
test_that("Non-standard distributions work as expected", {
  set.seed(1234)
  lm <- sample_lm(.func = "EnvStats::rlnormTrunc", n = 10,
                  params = list(meanlog = 1, sdlog = 2, min = 1, max = 2))
  expect_is(lm, "list")
  # ensure that the list has the required elements
  expect_equal(names(lm), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(lm$samples), 10)
  # ensure that values of samples is correct
  expect_equal(signif(unlist(lm$samples), digits = 4),
               signif(c(1.087017, 1.552746, 1.539039, 1.553887, 1.823434,
                        1.571874, 1.007058, 1.184094, 1.599599, 1.442124),
                      digits = 4))
})

context("Sample LEF")
test_that("Sample LEF works with composition function", {
  set.seed(1234)
  tef <- sample_tef(n = 10, params = list(1, 10, 20))
  vuln <- sample_vuln(n = 10, params = list(1, .6))
  dat <- sample_lef(n = 10, .func = "evaluator::compare_tef_vuln",
             params = list(tef = tef$samples, vuln = vuln$samples))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equal(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(dat$samples), 10)
  # ensure that LEF samples are always integers
  expect_is(dat$samples, "integer")
  # ensure that values of samples is correct
  expect_equal(dat$samples, c(5, 11, 15, 2, 12, 0, 8, 0, 0, 6))
})

context("Standard simulation model")
test_that("Default simulation model returns expected results", {
  scen <- tidyrisk_scenario(
    tef_params = list(func = "mc2d::rpert", min = 1, mode = 10, max = 100, shape = 4),
    tc_params = list(func = "mc2d::rpert", min = 1, mode = 10, max = 75, shape = 100),
    lm_params = list(func = "mc2d::rpert", min = 1, mode = 100, max = 10000, shape = 54),
    diff_params = list(list(func = "mc2d::rpert", min = 1, mode = 10, max = 50, shape = 4)))
  sim <- openfair_tef_tc_diff_lm(scen, n = 100)
  expect_s3_class(sim, "tbl_df")
  expect_equal(nrow(sim), 100)
  expect_equal(length(sim), 11)
  expect_equal(sum(sim$threat_events), 2287)
  expect_equal(sum(sim$loss_events), 786)
})

context("Main simulation")
test_that("Full wrapped scenario works as expected", {
  scenario <- structure(list(scenario_id = "1",
                            scenario = "Inadequate human resources are available to execute the informaton security strategic security plan.",
                            tcomm = "Organizational Leadership", domain_id = "ORG",
                            controls = "1, 5, 7, 32, 14, 15, 16",
                            diff_params = list(list(list(func = "mc2d::rpert", min = 70L, mode = 85L, max = 98L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 50L, mode = 70L, max = 84L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 0L,  mode = 10L, max = 30L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 50L, mode = 70L, max = 84L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 20L, mode = 30L, max = 50L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 20L, mode = 30L, max = 50L, shape = 4L),
                                                    list(func = "mc2d::rpert", min = 50L, mode = 70L, max = 84L, shape = 4L))),
                            tef_params = list(list(func = "mc2d::rpert", min  = 10L, mode = 24, max = 52L, shape = 4L)),
                            tc_params = list(list(func = "mc2d::rpert", min = 33L, mode = 50, max = 60L, shape = 3L)),
                            lm_params = list(list(func = "mc2d::rpert", min = 10000L, mode = 20000, max = 500000L, shape = 4L)),
                            model = "openfair_tef_tc_diff_lm"), row.names = c(NA, -1L),
                       class = c("tbl_df", "tbl", "data.frame"))
  scenario <- scenario %>% mutate(scenario = pmap(list(tef_params, tc_params, diff_params, lm_params, model), tidyrisk_scenario))
  results <- evaluate_promise(run_simulations(scenario[[1, "scenario"]], 100L))
  expect_s3_class(results$result, "tbl_df")
  expect_equal(nrow(results$result), 100)
  expect_equal(length(results$result), 11)
  expect_equal(sum(results$result$threat_events), 2686)
  #$expect_equal(sum(results$result$loss_events), 764)
  expect_equal(sum(results$result$loss_events), 772)
})

test_that("Simulation fails when given a simulation_count", {
  data("quantitative_scenarios")
  bad_scen <- quantitative_scenarios[[1, "scenario"]]
  class(bad_scen) <- "list"
  expect_error(run_simulations(bad_scen, simulation_count = 10L), regexp = "iteration")
})

test_that("Simulation fails when not given a scenario object", {
  data("quantitative_scenarios")
  bad_scen <- quantitative_scenarios[[1, "scenario"]]
  class(bad_scen) <- "list"
  expect_error(run_simulations(bad_scen, 10L), regexp = "object")
})

test_that("Simulation respects maximum ALE", {
  data("quantitative_scenarios")
  good_scen <- quantitative_scenarios[[1, "scenario"]]
  results <- run_simulations(good_scen, 10L, ale_maximum = 100)
  expect_lte(max(results$ale), 100)
})
