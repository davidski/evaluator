## ----prepare_sample_environement, eval=FALSE-----------------------------
#  library(dplyr)     # piping
#  library(readr)     # better CSV handling
#  library(evaluator) # core evaluator toolkit
#  
#  # create default directories
#  input_directory <- "~/evaluator/data"
#  results_directory <- "~/evaluator/results"
#  if (!dir.exists(input_directory)) dir.create(input_directory, recursive = TRUE)
#  if (!dir.exists(results_directory)) dir.create(results_directory, recursive = TRUE)
#  
#  # copy sample files
#  file.copy(system.file("extdata", "domains.csv", package = "evaluator"),
#            input_directory)
#  file.copy(system.file("extdata", "qualitative_mappings.csv", package="evaluator"),
#            input_directory)
#  file.copy(system.file("extdata", "risk_tolerances.csv", package = "evaluator"),
#            input_directory)
#  file.copy(system.file("survey", "survey.xlsx", package = "evaluator"),
#            input_directory)

## ----import, eval=FALSE--------------------------------------------------
#  domains <-  readr::read_csv(file.path(input_directory, "domains.csv"))
#  system.file("survey", "survey.xlsx", package = "evaluator") %>%
#    import_spreadsheet(., domains, output_dir = input_directory)

## ----validate, eval=FALSE------------------------------------------------
#  mappings <-  readr::read_csv(file.path(input_directory, "qualitative_mappings.csv"))
#  qualitative_scenarios <- readr::read_csv(file.path(input_directory, "qualitative_scenarios.csv"))
#  capabilities <- readr::read_csv(file.path(input_directory, "capabilities.csv"))
#  validate_scenarios(qualitative_scenarios, capabilities, domains, mappings)

## ----encode, eval = FALSE------------------------------------------------
#  quantitative_scenarios <- encode_scenarios(qualitative_scenarios,
#                                             capabilities, mappings)

## ----simulate, eval = FALSE----------------------------------------------
#  simulation_results <- run_simulations(quantitative_scenarios,
#                                        simulation_count = 10000L)
#  save(simulation_results, file = file.path(results_directory, "simulation_results.rda"))

## ----data_files, eval = FALSE, echo=FALSE--------------------------------
#  tibble::tribble(
#      ~"Data File", ~Purpose,
#      "simulation_results.rda", "Full details of each simulated scenario",
#      "scenario_summary.rda", "Simulation results, summarized at the scenario level",
#      "domain_summary.rda", "Simulation results, summarized at the domain level"
#  ) %>% pander::pander(., justify = "left")

## ----analyze, eval=FALSE-------------------------------------------------
#  # summarize
#  scenario_summary <- summarize_scenarios(simulation_results)
#  domain_summary <- summarize_domains(simulation_results, domains)
#  
#  # or to save the summary files directly to disk
#  summarize_to_disk(simulation_results = simulation_results, domains = domains, results_directory)
#  
#  # define risk tolerances
#  risk_tolerances <- system.file("extdata", "risk_tolerances.csv",
#                                 package="evaluator") %>% read_csv()
#  
#  # Explorer
#  explore_scenarios(input_directory, results_directory)
#  
#  # Sample Report
#  generate_report(input_directory, results_directory) %>% rstudioapi::viewer()

