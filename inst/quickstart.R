# Quick Start script for Evaluator workflow
# Process documented at https://evaluator.severski.net/articles/usage.html

library(evaluator)

# load and validate
domains <-  readr::read_csv(file.path(inputs_dir, "domains.csv"))
import_spreadsheet(file.path(inputs_dir, "survey.xlsx"), domains, inputs_dir)

qualitative_scenarios <- readr::read_csv(file.path(inputs_dir, 
                                                   "qualitative_scenarios.csv"))
mappings <- readr::read_csv(file.path(inputs_dir, "qualitative_mappings.csv"))
capabilities <- readr::read_csv(file.path(inputs_dir, "capabilities.csv"))
validate_scenarios(qualitative_scenarios, capabilities, domains, mappings)

# encode
quantitative_scenarios <- encode_scenarios(qualitative_scenarios, capabilities, 
                                           mappings)

# simulate
simulation_results <- run_simulations(quantitative_scenarios, 
                                      simulation_count = 100L)
save(simulation_results, file = file.path(results_dir, "simulation_results.rda"))

# summarize
summarize_to_disk(simulation_results = simulation_results, 
                  domains = domains, results_dir)

# generate sample reports

## Risk Dashboard
risk_dashboard(inputs_dir, results_dir, 
               file.path(results_dir, "risk_dashboard.html"))

## Sample Report
generate_report(inputs_dir, results_dir, 
                file.path(results_dir, "risk_report.html"))
