context("Data Loads")
tmpdir <- tempdir()
tmpdata <- file.path(tmpdir, "data")
dir.create(tmpdata)
tmpinputs <- file.path(tmpdir, "inputs")
dir.create(tmpinputs, showWarnings = FALSE)

data("simulation_results", package = "evaluator", envir = environment())
save(simulation_results, file = file.path(tmpdata, "simulation_results.rda"))
saveRDS(simulation_results, file = file.path(tmpdata, "simulation_results.Rds"))
data("scenario_summary", package = "evaluator", envir = environment())
save(scenario_summary, file = file.path(tmpdata, "scenario_summary.rda"))
data("domain_summary", package = "evaluator", envir = environment())
save(domain_summary, file = file.path(tmpdata, "domain_summary.rda"))

data("capabilities", package = "evaluator", envir = environment())
readr::write_csv(capabilities, file.path(tmpinputs, "capabilities.csv"))


res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))
data("qualitative_scenarios", envir = environment())
save(qualitative_scenarios, file = file.path(tmpinputs, "qualitative_scenarios.rda"))
readr::write_csv(qualitative_scenarios, file.path(tmpinputs, "qualitative_scenarios.csv"))
data("quantitative_scenarios", envir = environment())
saveRDS(quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.Rds"))

test_that("Template files can be copied", {
  tmpdata <- file.path(tempdir(check = TRUE), "templates")
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

