#' Format dollar amounts in terms of millions of USD
#'
#' @importFrom dplyr %>%
#' @param x A number.
#' @return String in the format of $xM.
#' @export
#' @example dollar_millions(1.523 * 10^6)
dollar_millions <- function(x) {
  # paste0('$', x / 10 ^ 6, 'M')
  x <- (x/10^6) %>% round(digits = 2)
  paste0("$", x, "M")
}

#' Calculate control weaknesses on a domain level
#'
#' @import dplyr
#' @param simulation_results Results of running the risk simulations.
#' @param domains Domain titles and IDs as a dataframe.
#' @return Control weaknesses summarized by domain.
#' @export
calculate_weak_domains <- function(simulation_results, domains) {
  control_weakness <- simulation_results %>% group_by_("domain_id") %>%
    summarize_(loss_events = ~ sum(loss_events), threat_events = ~ sum(threat_events)) %>%
    mutate_(vuln = ~ loss_events/threat_events) %>%
    arrange_(~ desc(vuln)) %>% mutate_(vuln = ~ scales::percent(vuln))
  # recalc domain_level tc_exceedance
  tc_exceedance <- simulation_results %>% group_by_("domain_id") %>%
    mutate_(avoided_events = ~ threat_events - loss_events) %>%
    summarize_(tc_exceedance = ~ sum(mean_tc_exceedance * loss_events),
              diff_exceedance = ~ sum(mean_diff_exceedance * avoided_events),
              avoided_events = ~ sum(avoided_events),
              loss_events = ~ sum(loss_events)) %>%
    mutate_(tc_exceedance = ~ tc_exceedance / loss_events,
            diff_exceedance = ~diff_exceedance / avoided_events) %>%
    mutate_(tc_exceedance = ~ ifelse(is.na(tc_exceedance), NA,
                                     scales::percent(tc_exceedance/100)),
            diff_exceedance = ~ ifelse(is.na(diff_exceedance), NA,
                                       scales::percent(diff_exceedance/100))) %>%
    select_("tc_exceedance", "diff_exceedance", "domain_id")
  control_weakness <- left_join(control_weakness, tc_exceedance,
                                by = c(domain_id = "domain_id")) %>%
    left_join(domains, by = c(domain_id = "domain_id")) %>%
      mutate_(domain = ~ paste0(domain, " (", domain_id, ")"))
  control_weakness
}

#' Calculate quantified impact at a domain level
#'
#' @import dplyr
#' @importFrom stats sd quantile
#' @param domain_summary Domain-level summary of `run_simulation` results.
#' @param domains Dataframe of all domains in scope.
#' @return A dataframe
#' @export
calculate_domain_impact <- function(domain_summary, domains) {
  domain_summary %>% group_by_(~domain_id) %>% select_("domain_id", "ale") %>%
    summarize_at(vars("ale"), funs(min, mean, max, sd,
                                   var = quantile(., probs = 0.95))) %>%
    arrange_("desc(var)") %>% ungroup %>%
    left_join(domains, by = c(domain_id = "domain_id")) %>%
    mutate_(domain = quote(paste0(domain, " (", domain_id, ")")))
}

#' Calculate maximum losses with and without outliers
#'
#' @import dplyr
#' @param simulation_results Data.
#' @param scenario_outliers Vector of scenario_ids which are outliers
#' @return A dataframe.
#' @export
calculate_max_losses <- function(simulation_results, scenario_outliers) {
  max_loss <- simulation_results %>% filter_(~ !scenario_id %in% scenario_outliers) %>%
    group_by_("simulation") %>%
    summarise_(biggest_single_scenario_loss = ~ max(ale),
               min_loss = ~ min(ale), max_loss = ~ sum(ale),
               outliers = FALSE) %>%
    ungroup
  max_loss_w_outliers <- simulation_results %>% group_by_("simulation") %>%
    summarise_(biggest_single_scenario_loss = ~ max(ale), min_loss = ~ min(ale),
               max_loss = ~ sum(ale), outliers = TRUE) %>%
    ungroup
  union(max_loss, max_loss_w_outliers)
}
