context("Data Loads")
tmpdir <- tempdir()
tmpdata <- file.path(tmpdir, "data")
dir.create(tmpdata)
tmpinputs <- file.path(tmpdir, "inputs")
dir.create(tmpinputs, showWarnings = FALSE)

data("mc_simulation_results", package = "evaluator", envir = environment())
saveRDS(mc_simulation_results, file = file.path(tmpdata, "simulation_results.rds"))
data("mc_scenario_summary", package = "evaluator", envir = environment())
saveRDS(mc_scenario_summary, file = file.path(tmpdata, "scenario_summary.rds"))
data("mc_domain_summary", package = "evaluator", envir = environment())
saveRDS(mc_domain_summary, file = file.path(tmpdata, "domain_summary.rds"))

data("mc_capabilities", package = "evaluator", envir = environment())
readr::write_csv(mc_capabilities, file.path(tmpinputs, "capabilities.csv"))


res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))
data("mc_qualitative_scenarios", envir = environment())
saveRDS(mc_qualitative_scenarios, file = file.path(tmpinputs, "qualitative_scenarios.rds"))
readr::write_csv(mc_qualitative_scenarios, file.path(tmpinputs, "qualitative_scenarios.csv"))
data("mc_quantitative_scenarios", envir = environment())
saveRDS(mc_quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.rds"))

test_that("Template files can be copied", {
  tmpdata <- file.path(tempdir(), "templates")
  dir.create(tmpdata, showWarnings = FALSE)
  res <- create_templates(tmpdata)
  expect_equal(sum(res$copied), 5)
  unlink(tmpdata, recursive = TRUE)
})

test_that("Deprecated load data function works", {
  expect_warning(load_data(tmpinputs, tmpdata))
})

test_that("Qualitative inputs can be loaded",{
  dat <- read_qualitative_inputs(tmpinputs)
  expect_is(dat, "list")
})

test_that("Quantitative inputs can be loaded", {
  dat <- read_quantitative_inputs(tmpinputs)
  expect_is(dat, "list")
})

unlink(c(tmpdata, tmpinputs), recursive = TRUE)
