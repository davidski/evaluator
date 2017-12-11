## Regenerate sample data sets


# read in and save domain mappings
domains <- readr::read_csv("./inst/extdata/domains.csv")
devtools::use_data(domains, overwrite = TRUE)

# read in capabilities
capabilities <- import_capabilities(domains = domains)
devtools::use_data(capabilities, overwrite = TRUE)

# read in mappings
mappings <- readr::read_csv("./inst/extdata/qualitative_mappings.csv")

# read in and save qualitative scenarios
qualitative_scenarios <- import_scenarios(domains = domains)
devtools::use_data(qualitative_scenarios, overwrite = TRUE)

# generate and save quantitative scenarios
quantitative_scenarios <- encode_scenarios(qualitative_scenarios,
                                           capabilities,
                                           mappings)
devtools::use_data(quantitative_scenarios, overwrite = TRUE)

# run simulations and save results
simulation_results <- run_simulations(quantitative_scenarios, 10000L)
devtools::use_data(simulation_results, overwrite = TRUE)

# calculate and save domain summary
domain_summary <- summarize_domains(simulation_results, domains)
devtools::use_data(domain_summary, overwrite = TRUE)

# calculate and save scenario summary
scenario_summary <- summarize_scenarios(simulation_results)
devtools::use_data(scenario_summary, overwrite = TRUE)
