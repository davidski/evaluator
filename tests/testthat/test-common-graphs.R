test_that("Basefont selection works", {
  dat <- get_base_fontfamily()
  expect_type(dat, "character")
  expect_gt(nchar(dat), 1)
})
test_that("Basefont returns sans when no fonts available", {
  mockery::stub(get_base_fontfamily, 'extrafont::choose_font', "")
  expect_equal(get_base_fontfamily(), "sans")
})
test_that("Basefont returns Benton when available", {
  mockery::stub(get_base_fontfamily, 'extrafont::choose_font', "BentonSansRE")
  expect_equal(get_base_fontfamily(), "BentonSansRE")
})


test_that("Theme functions", {
  gg <- theme_evaluator()
  expect_s3_class(gg, "gg")
  expect_s3_class(gg, "theme")
})

test_that("Domain VaR heatmap", {
  data(mc_domain_summary)
  gg <- generate_heatmap(mc_domain_summary)
  expect_s3_class(gg, "gg")
})

test_that("generate_scatterplot() throws a warning", {
  data(mc_simulation_results)
  expect_warning(generate_scatterplot(mc_simulation_results, scenario_id = "RS-50"))
})

test_that("loss_scatterplot() functions", {
  data(mc_simulation_results)
  mc_simulation_results %>% filter(scenario_id == "RS-50") %>%
    unnest(.data$results) -> my_results
  gg <- loss_scatterplot(my_results)
  expect_s3_class(gg, "gg")
})

test_that("Exposure histogram", {
  data(mc_simulation_results)
  gg <- mc_simulation_results %>% filter(scenario_id == "RS-50") %>%
    unnest(.data$results) %>% exposure_histogram()
  expect_s3_class(gg, "gg")
})

test_that("Exposure histogram with VaR line", {
  data(mc_simulation_results)
  gg <- mc_simulation_results %>% filter(scenario_id == "RS-50") %>%
    unnest(.data$results) %>% exposure_histogram(show_var_95 = TRUE)
  expect_s3_class(gg, "gg")
})

test_that("Domain-level outcomes", {
  data(mc_domain_summary)
  gg <- generate_event_outcomes_plot(mc_domain_summary)
  expect_s3_class(gg, "gg")
})
