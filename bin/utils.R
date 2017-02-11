# ---- common_functions ----

# format dollar amounts in terms of millions of USD
dollar_millions <- function(x) {
  #paste0("$", x / 10 ^ 6,  "M")
  x <- (x / 10 ^ 6) %>% round(digits=2)
  paste0("$", x, "M")
}

# determine fonts we can/should use
get_base_fontfamily <- function() {
  if ("BentonSansRE" %in% extrafont::fonts()) {
    "BentonSansRE" 
  } else 
    "Arial Narrow"
}

#default theme
theme_evaluator <- function(base_family="BentonSansRE") {
  theme_minimal(base_family = base_family) %+replace% 
    theme(
      panel.border = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 9, hjust = 1)
    )
}

# ---- calculate_weak_domains ----

# Can't calc mean of means this way!
# control_weakness <- scenario_summary %>% group_by(domain_id) %>% 
#   summarize(vuln = round(mean(mean_vuln), 2)) %>% 
#   arrange(desc(vuln)) %>% 
#   mutate(vuln = percent(vuln)) %>% 
#   ungroup()

control_weakness <- results %>% group_by(domain_id) %>% 
  summarize(loss_events = sum(loss_events), 
            threat_events = sum(threat_events)) %>% 
  mutate(vuln = loss_events / threat_events) %>% 
  arrange(desc(vuln)) %>% 
  mutate(vuln = percent(vuln))
# recalc domain_level tc_exceedance
tc_exceedance <- results %>% group_by(domain_id) %>% 
  mutate(avoided_events = threat_events - loss_events) %>% 
  summarize(tc_exceedance = sum(mean_tc_exceedance * loss_events), 
            diff_exceedance = sum(mean_diff_exceedance * avoided_events),
            avoided_events = sum(avoided_events),
            loss_events = sum(loss_events)) %>% 
  mutate(tc_exceedance = tc_exceedance / loss_events,
         diff_exceedance = diff_exceedance / avoided_events ) %>% 
  mutate(tc_exceedance = ifelse(is.na(tc_exceedance), NA, 
                                percent(tc_exceedance / 100 )),
         diff_exceedance = ifelse(is.na(diff_exceedance), NA, 
                                  percent(diff_exceedance / 100))) %>% 
  select(tc_exceedance, diff_exceedance, domain_id)
left_join(control_weakness, tc_exceedance, by = c("domain_id" = "domain_id")) %>% 
  left_join(domains, by = c("domain_id" = "domain_id")) %>% 
  mutate(domain = paste0(domain, " (", domain_id, ")")) -> control_weakness
