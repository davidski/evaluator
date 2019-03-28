#!/usr/bin/env r

# Regenerate sample reports
devtools::load_all()

devtools::install_cran("here")

tmpdir <- tempdir()
tmpdata <- file.path(tmpdir, "data")
dir.create(tmpdata)
tmpinputs <- file.path(tmpdir, "inputs")
dir.create(tmpinputs, showWarnings = FALSE)

output_dir <- here::here("reports")
dir.create(output_dir, showWarnings = FALSE)

data("mc_simulation_results", package = "evaluator", envir = environment())
saveRDS(simulation_results, file = file.path(tmpdata, "simulation_results.rds"))
data("mc_scenario_summary", package = "evaluator", envir = environment())
saveRDS(scenario_summary, file = file.path(tmpdata, "scenario_summary.rds"))
data("mc_domain_summary", package = "evaluator", envir = environment())
saveRDS(domain_summary, file = file.path(tmpdata, "domain_summary.rds"))

res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))
data("mc_qualitative_scenarios", envir = environment())
readr::write_csv(qualitative_scenarios, file.path(tmpinputs, "qualitative_scenarios.csv"))
data("mc_quantitative_scenarios", envir = environment())
saveRDS(quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.rds"))

file <- file.path(output_dir, "evaluator_risk_analysis.html")

generate_report(input_directory = tmpinputs,
                results_directory = tmpdata,
                output_file = file)

file <- file.path(output_dir, "evaluator_risk_dashboard.html")

risk_dashboard(input_directory = tmpinputs,
               results_directory = tmpdata,
               output_file = file)

unlink(c(tmpdata, tmpinputs), recursive = TRUE)
