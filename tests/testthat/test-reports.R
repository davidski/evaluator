context("Reports")
test_that("Analyze report renders", {

  skip_on_cran()

  tmpdir <- tempdir()
  tmpdata <- file.path(tmpdir, "data")
  tmpinputs <- file.path(tmpdir, "inputs")
  dir.create(tmpdata)
  dir.create(tmpinputs)

  data("simulation_results", package = "evaluator", envir = environment())
  save(simulation_results, file = file.path(tmpdata, "simulation_results.rda"))
  data("scenario_summary", package = "evaluator", envir = environment())
  save(scenario_summary, file = file.path(tmpdata, "scenario_summary.rda"))
  data("domain_summary", package = "evaluator", envir = environment())
  save(domain_summary, file = file.path(tmpdata, "domain_summary.rda"))

  res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
    purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                           tmpinputs))
  data("capabilities", envir = environment())
  readr::write_csv(domains, file.path(tmpinputs, "capabilities.csv"))
  data("qualitative_scenarios", envir = environment())
  readr::write_csv(qualitative_scenarios, file.path(tmpinputs, "qualitative_scenarios.csv"))

  file <- tempfile(fileext = ".html")
  # expect_success(generate_report(input_directory = tmpinputs,
  #                                results_directory = tmpdata,
  #                                output_file = file))
})
