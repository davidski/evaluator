## Regenerate sample data sets
library(evaluator)
library(purrr)
library(dplyr)
library(future)
plan(multiprocess)

# read in and save domain mappings
domains <- readr::read_csv(here::here("inst/extdata/domains.csv"),
                           col_types = readr::cols(
                             domain_id = readr::col_character(),
                             domain = readr::col_character()
                           ))
usethis::use_data(domains, overwrite = TRUE)

# read in capabilities
capabilities <- import_capabilities(domains = domains)
usethis::use_data(capabilities, overwrite = TRUE)

# read in mappings
mappings <- readr::read_csv(here::here("inst/extdata/qualitative_mappings.csv"),
                            col_types = readr::cols(
                              type = readr::col_character(),
                              label = readr::col_character(),
                              l = readr::col_double(),
                              ml = readr::col_double(),
                              h = readr::col_double(),
                              conf = readr::col_double()))
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
  mutate(results = furrr::future_map(scenario, run_simulations, iterations = 1000, .progress = TRUE)) %>%
  select(-c(scenario, tcomm, scenario_description), scenario_id, domain_id, results)

#simulation_results <- run_simulations(quantitative_scenarios, 1000L)
usethis::use_data(simulation_results, overwrite = TRUE)

# calculate and save domain summary
domain_summary <- summarize_domains(simulation_results)
usethis::use_data(domain_summary, overwrite = TRUE)

# calculate and save scenario summary
scenario_summary <- mutate(simulation_results, summary = map(results, summarize_scenario))
usethis::use_data(scenario_summary, overwrite = TRUE)
