## ----setup, include=FALSE------------------------------------------------
library(tibble)
library(dplyr)
library(pander)

## ----quick_start, eval=FALSE---------------------------------------------
#  base_dir <- "~/evaluator"
#  source("~/evaluator/run_analysis.R")

## ----import, eval=FALSE--------------------------------------------------
#  domains <- readr::read_csv("~/evaluator/inputs/domains.csv")
#  import_spreadsheet("~/evaluator/inputs/survey.xlsx", domains, output_dir = "~/evaluator/inputs")

## ----validate, eval=FALSE------------------------------------------------
#  qualitative_scenarios <- readr::read_csv("~/evaluator/inputs/qualitative_scenarios.csv")
#  mappings <- readr::read_csv("~/evaluator/inputs/qualitative_mappings.csv")
#  capabilities <- readr::read_csv("~/evaluator/inputs/capabilities.csv")
#  validate_scenarios(qualitative_scenarios, capabilities, domains, mappings)

## ----encode, eval = FALSE------------------------------------------------
#  quantitative_scenarios <- encode_scenarios(qualitative_scenarios,
#                                             capabilities, mappings)

## ----simulate, eval = FALSE----------------------------------------------
#  simulation_results <- run_simulations(quantitative_scenarios,
#                                        simulation_count = 100L)
#  save(simulation_results, file = "~/evaluator/results/simulation_results.rda")

## ----summarize, eval=FALSE-----------------------------------------------
#  summarize_to_disk(simulation_results = simulation_results, domains = domains,
#                    results_dir = "~/evaluator/results")

## ----data_files, eval = TRUE, echo=FALSE---------------------------------
tibble::tribble(
    ~"Data File", ~Purpose,
    "simulation_results.rda", "Full details of each simulated scenario",
    "scenario_summary.rda", "Simulation results, summarized at the scenario level",
    "domain_summary.rda", "Simulation results, summarized at the domain level"
) %>% pander::pander(., justify = "left")

## ----analyze, eval=FALSE-------------------------------------------------
#  # Explorer
#  explore_scenarios("~/evaluator/inputs", "~/evaluator/results")
#  
#  # Risk Dashboard
#  risk_dashboard("~/evaluator/inputs", "~/evaluator/results",
#                 "~/evaluator/risk_dashboard.html")
#  
#  # Sample Report
#  generate_report("~/evaluator/inputs", "~/evaluator/results",
#                  "~/evaluator/risk_report.html") %>% rstudioapi::viewer()

