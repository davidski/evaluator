context("Reports")
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

res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))
data("mc_qualitative_scenarios", envir = environment())
write.csv(mc_qualitative_scenarios, file = file.path(tmpinputs, "qualitative_scenarios.csv"),
          row.names = FALSE)
data("mc_quantitative_scenarios", envir = environment())
saveRDS(mc_quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.rds"))


test_that("Analyze report renders", {

  skip_if_not(rmarkdown::pandoc_available(),
              message = "Cannot test report generation without pandoc available.")
  purrr::walk(c("psych", "pander", "ggalt", "rmarkdown"),
              ~ skip_if_not_installed(.))

  file <- tempfile(fileext = ".html")

  result <- evaluate_promise(generate_report(input_directory = tmpinputs,
                                             results_directory = tmpdata,
                                             output_file = file,
                                             quiet = FALSE))
  expect_equivalent(normalizePath(result$result), normalizePath(file))
  unlink(file)
})

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
