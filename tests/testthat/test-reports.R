context("Reports")
tmpdir <- tempdir()
tmpdata <- file.path(tmpdir, "data")
dir.create(tmpdata)
tmpinputs <- file.path(tmpdir, "inputs")
dir.create(tmpinputs, showWarnings = FALSE)

data("simulation_results", package = "evaluator", envir = environment())
saveRDS(simulation_results, file = file.path(tmpdata, "simulation_results.Rds"))
data("scenario_summary", package = "evaluator", envir = environment())
save(scenario_summary, file = file.path(tmpdata, "scenario_summary.rda"))
data("domain_summary", package = "evaluator", envir = environment())
save(domain_summary, file = file.path(tmpdata, "domain_summary.rda"))

res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))
data("capabilities", envir = environment())
readr::write_csv(capabilities, file.path(tmpinputs, "capabilities.csv"))
data("qualitative_scenarios", envir = environment())
readr::write_csv(qualitative_scenarios, file.path(tmpinputs, "qualitative_scenarios.csv"))
data("quantitative_scenarios", envir = environment())
saveRDS(quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.Rds"))


test_that("Analyze report renders", {

  skip_if_not(rmarkdown::pandoc_available(),
              message = "Cannot test report generation without pandoc available.")
  purrr::walk(c("psych", "pander", "purrrlyr", "ggalt", "rmarkdown"),
              ~ skip_if_not_installed(.))

  file <- tempfile(fileext = ".html")

  result <- evaluate_promise(generate_report(input_directory = tmpinputs,
                                             results_directory = tmpdata,
                                             output_file = file,
                                             quiet = FALSE))
  expect_equivalent(normalizePath(result$result), normalizePath(file))
  unlink(file)
})


# test_that("Scenario Explorer launches", {
#   expect_is(explore_scenarios(input_directory = tmpinputs,
#                               results_directory = tmpdata,
#                               shiny_args = list(launch.browser = FALSE)),
#                               "shiny.appobj")
# })

# test_that("OpenFAIR Example launches", {
#   expect_is(openfair_example(shiny_args = list(launch.browser = FALSE)),
#             "shiny.appobj")
# })

test_that("Risk Dashboard renders", {

  skip_if_not(rmarkdown::pandoc_available(),
              message = "Cannot test dashboard generation without pandoc available.")
  purrr::walk(c("rmarkdown", "shiny", "flexdashboard", "forcats"),
              ~ skip_if_not_installed(.))

  file <- tempfile(fileext = ".html")

  result <- evaluate_promise(risk_dashboard(input_directory = tmpinputs,
                                            results_directory = tmpdata,
                                            output_file = file,
                                            quiet = FALSE))
  expect_equivalent(normalizePath(result$result), normalizePath(file))
  # there should be no warnings
  expect_condition(result$warnings, regexp = NA)
  unlink(file)
})

unlink(c(tmpdata, tmpinputs), recursive = TRUE)
