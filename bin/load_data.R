
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


# Precalculate the standard order of scenarios (domain, then ID of the scenario)
scenario_order <- results %>% group_by(domain_id, scenario_id) %>% summarise()
# store the `scenario_id` of outliers 
scenario_outliers <- scenario_summary[which(scenario_summary$outlier == TRUE), 
                                      "scenario_id"] %>% unlist %>% unname
# a text vector of numbers to english words
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", 
             "nine")
risk_tolerance <- risk_tolerances$amount
names(risk_tolerance) <- risk_tolerances$level %>% tolower

# enhance scenario_summary
# assign loss tolerance to ALE VaR size
scenario_summary %<>% 
  mutate(annual_tolerance = ifelse(ale_var >= risk_tolerance["high"], 
                                   "High", 
                                   ifelse(ale_var >= risk_tolerance["medium"], "Medium", "Low"))) %>% 
  mutate(annual_tolerance = factor(annual_tolerance, 
                                   levels = c("High", "Medium", "Low"), 
                                   ordered = TRUE)) 
scenario_outliers <- scenario_summary[which(scenario_summary$outlier == TRUE), 
                                      "scenario_id"] %>% unlist %>% unname

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


# ---- calculate_max_losses ----
max_loss <- results %>% filter(!scenario_id %in% scenario_outliers) %>%  
  group_by(simulation) %>% 
  summarise(biggest_single_scenario_loss = max(ale),
            min_loss = min(ale),
            max_loss = sum(ale),
            outliers = FALSE) %>%
  ungroup
max_loss_w_outliers <- results %>% 
  group_by(simulation) %>% 
  summarise(biggest_single_scenario_loss = max(ale),
            min_loss = min(ale),
            max_loss = sum(ale),
            outliers = TRUE) %>%
  ungroup
max_loss <- union(max_loss, max_loss_w_outliers)
rm(max_loss_w_outliers)