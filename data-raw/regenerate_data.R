## Regenerate sample data sets
#library(evaluator)
library(purrr)
library(dplyr)
library(future)
plan(multiprocess)

# read in and save domain mappings
mc_domains <- readr::read_csv(here::here("inst/extdata/domains.csv"),
                           col_types = readr::cols(
                             domain_id = readr::col_character(),
                             domain = readr::col_character()
                           ))
usethis::use_data(mc_domains, overwrite = TRUE)

# read in capabilities
mc_capabilities <- import_capabilities(domains = mc_domains)
usethis::use_data(mc_capabilities, overwrite = TRUE)

# read in mappings
mc_mappings <- readr::read_csv(here::here("inst/extdata/qualitative_mappings.csv"),
                            col_types = readr::cols(
                              type = readr::col_character(),
                              label = readr::col_character(),
                              l = readr::col_double(),
                              ml = readr::col_double(),
                              h = readr::col_double(),
                              conf = readr::col_double()))
usethis::use_data(mc_mappings, overwrite = TRUE)

# read in and save qualitative scenarios
mc_qualitative_scenarios <- import_scenarios(domains = mc_domains)
usethis::use_data(mc_qualitative_scenarios, overwrite = TRUE)

# generate and save quantitative scenarios
mc_quantitative_scenarios <- encode_scenarios(
  mc_qualitative_scenarios, mc_capabilities, mc_mappings)
usethis::use_data(mc_quantitative_scenarios, overwrite = TRUE)

# run simulations and save results
mc_simulation_results <- mc_quantitative_scenarios %>%
  mutate(results = furrr::future_map(scenario, run_simulation, iterations = 1000, .progress = TRUE)) %>%
  select(-c(scenario, tcomm, scenario_description), scenario_id, domain_id, results)

usethis::use_data(mc_simulation_results, overwrite = TRUE)

# calculate and save domain summary
mc_domain_summary <- summarize_domains(mc_simulation_results)
usethis::use_data(mc_domain_summary, overwrite = TRUE)

# calculate and save scenario summary
#scenario_summary <- mutate(simulation_results, summary = map(results, summarize_scenario))
mc_scenario_summary <- summarize_scenarios(mc_simulation_results)
usethis::use_data(mc_scenario_summary, overwrite = TRUE)
