context("Sample TEF")
test_that("Sample TEF", {
  set.seed(1234)
  tef <- sample_tef(params = list(10, 1, 10, 100))
  expect_is(tef, "list")
  # ensure that the list has the required elements
  expect_equal(names(tef), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(tef$samples), 10)
  # ensure that values of samples is correct
  expect_equal(unlist(tef$samples), c(6.58476034295649, 30.043491618616,
                                      1.78473018566469, 34.1131016153908,
                                      36.3088944002633, 13.3732140003123,
                                      13.7715514319784, 13.5179511213919,
                                      9.49877054112384, 14.7953439798899))
})

context("Sample DIFF")
test_that("Sample DIFF", {
  set.seed(1234)
  dat <- sample_diff(params = list(10, 50, 70, 75, 3))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equal(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(dat$samples), 10)
  # ensure that values of samples is correct
  expect_equal(unlist(dat$samples), c(72.5519454551502, 65.1852603020272,
                                      59.1564180836877, 74.5816023178688,
                                      64.1192226440207, 63.561355776164,
                                      70.1284833577168, 69.9960887031119,
                                      70.0802721600923, 71.4683219144408))
})
test_that("Multi control diff works", {
  diff_estimates <- data_frame(l=c(1, 2), ml=c(10, 15), h=c(20, 100), conf=c(1, 3))
})

context("Sample TC")
test_that("Sample TC", {
  set.seed(1234)
  tc <- sample_tc(params = list(10, 50, 75, 100, 4))
  expect_is(tc, "list")
  # ensure that the list has the required elements
  expect_equal(names(tc), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(tc$samples), 10)
  # ensure that values of samples is correct
  expect_equal(unlist(tc$samples), c(61.7026564773373, 78.188740471894,
                                     87.0623477417219, 53.1987199785052,
                                     79.9184628308895, 80.7889924652588,
                                     68.4387021948896, 68.7541469869603,
                                     68.554057026653, 64.9764652390671))
})

context("Sample VULN")
test_that("Sample VULN works with binom", {
  set.seed(1234)
  dat <- sample_vuln(params = list(10, 1, .5))
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
  tc <- sample_tc(params = list(10, 50, 70, 85, 2))$samples
  diff <- sample_diff(params = list(10, 50, 70, 85, 2))$samples
  dat <- sample_vuln(func = select_loss_opportunities, params = list(tc = tc, diff = diff))
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

context("Sample LM")
test_that("Sample LM", {
  set.seed(1234)
  lm <- sample_lm(params = list(10, 1*10^4, 5*10^4, 1*10^7, 3))
  expect_that(lm, is_a("list"))
  # ensure that the list has the required elements
  expect_that(names(lm), equals(c("type", "samples", "details")))
  # ensure that the samples matches the number requested
  expect_that(length(lm$samples), equals(10))
  # ensure that values of samples is correct
  expect_that(unlist(lm$samples), equals(c(332422.727880636, 2831751.79415706,
                                           35602.2608120876, 3349352.73654269,
                                           3632631.71769846, 927503.010814968,
                                           966756.805719722, 941718.366417413,
                                           569057.598433507, 1069488.76293628)))
})

context("Sample LEF")
test_that("Sample LEF works with composition function", {
  set.seed(1234)
  tef <- sample_tef(params = list(10, 1, 10, 20))
  vuln <- sample_vuln(params = list(10, 1, .6))
  dat <- sample_lef(func = compare_tef_vuln,
             params = list(tef = tef$samples, vuln = vuln$samples))
  expect_is(dat, "list")
  # ensure that the list has the required elements
  expect_equal(names(dat), c("type", "samples", "details"))
  # ensure that the samples matches the number requested
  expect_equal(length(dat$samples), 10)
  # ensure that values of samples is correct
  expect_equal(dat$samples, c(5.20876621226071, 11.3834536928631,
                              14.8272191228827, 2.13539043294175,
                              12.0480617647494, 0, 7.69701064151479, 0, 0,
                              6.41214819406418))
})

context("Standard simulation")
test_that("Stanard simulation returns expected results", {
  sim <- calculate_ale(list(tef_l = 1, tef_ml=10, tef_h=100, tef_conf=4,
                            tc_l = 1, tc_ml = 10, tc_h =75, tc_conf=100,
                            lm_l=1, lm_ml=100, lm_h = 10000, lm_conf=54),
                       diff_estimates = data_frame(l=1, ml=10, h = 50, conf =4),
                       n = 100)
  expect_s3_class(sim, "tbl_df")
  expect_equal(nrow(sim), 100)
  expect_equal(length(sim), 12)
  expect_equal(sum(sim$threat_events), 2287)
  expect_equal(sum(sim$loss_events), 786)
})

context("Main simulation")
test_that("Full wrapped scenario works as expected", {
  scenario <- structure(list(scenario_id = 1L, scenario = "Inadequate human resources are available to execute the informaton security strategic security plan.",
                             tcomm = "Organizational Leadership", domain_id = "ORG", controls = "1, 5, 7, 32, 14, 15, 16",
                             diff_params = list(structure(list(control_id = c("1", "5",
                                                                              "7", "32", "14", "15", "16"), label = c("5 - Optimized",
                                                                                                                      "4 - Managed", "1 - Initial", "4 - Managed", "4 - Managed",
                                                                                                                      "2 - Repeatable", "2 - Repeatable"), type = c("diff", "diff",
                                                                                                                                                                    "diff", "diff", "diff", "diff", "diff"), l = c(70L, 50L,
                                                                                                                                                                                                                   0L, 50L, 50L, 20L, 20L), ml = c(85, 70, 10, 70, 70, 30, 30
                                                                                                                                                                                                                   ), h = c(98L, 84L, 30L, 84L, 84L, 50L, 50L), conf = c(4L,
                                                                                                                                                                                                                                                                         4L, 4L, 4L, 4L, 4L, 4L)), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                                                                         ), row.names = c(NA, -7L), .Names = c("control_id", "label",
                                                                                                                                                                                                                                                                                                               "type", "l", "ml", "h", "conf"))), tef_l = 10L, tef_ml = 24,
                             tef_h = 52L, tef_conf = 4L, tc_l = 33L, tc_ml = 50, tc_h = 60L,
                             tc_conf = 3L, lm_l = 10000L, lm_ml = 20000, lm_h = 500000L,
                             lm_conf = 4L), .Names = c("scenario_id", "scenario", "tcomm",
                                                       "domain_id", "controls", "diff_params", "tef_l", "tef_ml", "tef_h",
                                                       "tef_conf", "tc_l", "tc_ml", "tc_h", "tc_conf", "lm_l", "lm_ml",
                                                       "lm_h", "lm_conf"), row.names = c(NA, -1L), class = c("tbl_df",
                                                                                                             "tbl", "data.frame"))

  sim <- run_simulations(scenario, 100L)
  expect_s3_class(sim, "tbl_df")
  expect_equal(nrow(sim), 100)
  expect_equal(length(sim), 13)
  expect_equal(sum(sim$threat_events), 2686)
  expect_equal(sum(sim$loss_events), 764)
})
