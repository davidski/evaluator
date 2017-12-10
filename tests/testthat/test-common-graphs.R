context("Graphics")
test_that("Basefont selection works", {
  expect_type(get_base_fontfamily(), "character")
})

test_that("Theme functions", {
  gg <- theme_evaluator()
  expect_s3_class(gg, "gg")
  expect_s3_class(gg, "theme")
})

test_that("Domain VaR heatmap", {
  dat <- calculate_domain_impact(domain_summary, domains)
  gg <- generate_heatmap(dat)
  expect_s3_class(gg, "gg")
})

test_that("Scatterplot", {
  data(simulation_results)
  gg <- generate_scatterplot(simulation_results, scenario_id = 50)
  expect_s3_class(gg, "gg")
})

test_that("Domain-level outcomes", {
  data(domains)
  dat <- calculate_weak_domains(simulation_results, domains)
  gg <- generate_event_outcomes_plot(dat)
  expect_s3_class(gg, "gg")
})
