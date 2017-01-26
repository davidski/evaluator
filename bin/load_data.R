
# ---- load_data ----

# Simulations and pre-computed summary views are created via the 
# `strat_risk.Rmd` notebook.
results <- readRDS(file = "./data/simulation_results.Rds") # full results
scenario_summary <- readRDS(file = "./data/scenario_summary.Rds")  # scenario level summary
domain_summary <- readRDS(file = "./data/domain_summary.Rds")    # domain level summary

# details on our scenarios
domains <- read_csv("./data/domains.csv") # domain catalog
mappings <- read_csv("./data/qualitative_mappings.csv") # qualitative translations
capabilities <- read_csv("data/capabilities.csv") # i.e. objectives & controls
risk_tolerances <- read_csv("data/risk_tolerances.csv") # i.e. risk tolerances
scenarios <- read_csv("./data/scenarios.csv") %>% mutate(tef = tolower(tef), 
                                                         lm = tolower(lm), 
                                                         tc = tolower(tc))

# ---- calculate_domain_impact ----
domain_impact <- domain_summary %>%
  group_by(domain_id) %>%
  select(domain_id, ale) %>%
  summarize_each(funs(min, mean, max, sd, quantile(., probs = c(0.95)))) %>%
  rename(var = quantile) %>% 
  arrange(desc(var)) %>% 
  ungroup %>% 
  left_join(domains, by = c("domain_id" = "domain_id")) %>% 
  mutate(domain = paste0(domain, " (", domain_id, ")"))