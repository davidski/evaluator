context("OpenFAIR simulation functions")
test_that("Sample TEF", {
  set.seed(1234)
  tef <- sample_tef(n = 10, params = list(1, 1, 10, 100))
  expect_that(tef, is_a("list"))
  # ensure that the list has the required elements
  expect_that(names(tef), equals(c("type", "samples", "details")))
  # ensure that the samples matches the number requested
  expect_that(length(tef$samples), equals(10))
  # ensure that values of samples is correct
  expect_that(unlist(tef$samples), equals(c(6.58476034295649, 30.043491618616,
                                            1.78473018566469, 34.1131016153908,
                                            36.3088944002633, 13.3732140003123,
                                            13.7715514319784, 13.5179511213919,
                                            9.49877054112384, 14.7953439798899))
              )
})

test_that("Sample TC", {
  set.seed(1234)
  tc <- sample_tef(n = 10, params = list(1, 50, 75, 100, 4))
  expect_that(tc, is_a("list"))
  # ensure that the list has the required elements
  expect_that(names(tc), equals(c("type", "samples", "details")))
  # ensure that the samples matches the number requested
  expect_that(length(tc$samples), equals(10))
  # ensure that values of samples is correct
  expect_that(unlist(tc$samples), equals(c(61.7026564773373, 78.188740471894,
                                           87.0623477417219, 53.1987199785052,
                                           79.9184628308895, 80.7889924652588,
                                           68.4387021948896, 68.7541469869603,
                                           68.554057026653, 64.9764652390671))
              )
})

context("Main simulation")
test_that("Simulation", {
  sim <- calculate_ale(list(tef_l = 1, tef_ml=10, tef_h=100, tef_conf=4,
                            tc_l = 1, tc_ml = 10, tc_h =75, tc_conf=100,
                            lm_l=1, lm_ml=100, lm_h = 10000, lm_conf=54),
                       diff_estimates = data_frame(l=1, ml=10, h = 50, conf =4),
                       n = 100)
  expect_that(sim, is_a("tbl_df"))
  expect_that(nrow(sim), equals(100))
  expect_that(length(sim), equals(11))
})


