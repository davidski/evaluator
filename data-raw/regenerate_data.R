## Regenerate sample data sets
library(evaluator)
library(future)
plan(multiprocess)

# read in and save domain mappings
domains <- readr::read_csv(here::here("inst/extdata/domains.csv"))
usethis::use_data(domains, overwrite = TRUE)

# read in capabilities
capabilities <- import_capabilities(domains = domains)
usethis::use_data(capabilities, overwrite = TRUE)

# read in mappings
mappings <- readr::read_csv(here::here("inst/extdata/qualitative_mappings.csv"))
usethis::use_data(mappings, overwrite = TRUE)

# read in and save qualitative scenarios
qualitative_scenarios <- import_scenarios(domains = domains)
usethis::use_data(qualitative_scenarios, overwrite = TRUE)

# generate and save quantitative scenarios
quantitative_scenarios <- encode_scenarios(qualitative_scenarios,
                                           capabilities,
                                           mappings)
usethis::use_data(quantitative_scenarios, overwrite = TRUE)

# run simulations and save results
simulation_results <- quantitative_scenarios %>%
  mutate(results = furrr::future_map(scenario, run_simulations, simulation_count = 1000)) %>%
  select(-c(scenario, controls, tcomm, scenario_text), scenario_id, domain_id, results) %>%
  unnest(results)
simulation_results <- run_simulations(quantitative_scenarios, 1000L)
usethis::use_data(simulation_results, overwrite = TRUE)

# calculate and save domain summary
domain_summary <- summarize_domains(simulation_results)
usethis::use_data(domain_summary, overwrite = TRUE)

# calculate and save scenario summary
scenario_summary <- summarize_scenarios(simulation_results)
usethis::use_data(scenario_summary, overwrite = TRUE)

