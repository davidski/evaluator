# read in quant scenarios
quantitative_scenarios <- data("quantitative_scenarios")

# run simulations and save results
simulation_results <- run_simulations(quantitative_scenarios, 10000L)
save(simulation_results, file = "./data/simulation_results.rda")

# read in and save domain mappings
domains <- readr::read_csv("./inst/extdata/domains.csv")
save(domains, file = "./data/simulation_results.rda")

# calculate and save domain summary
domain_summary <- summarize_domains(scenario_summary, domains)
save(domain_summary, file = "./data/domain_summary.rda")

# calculate and save scenario summary
scenario_summary <- summarize_scenarios(simulation_results)
save(scenario_summary, file = "./data/scenario_summary.rda")
