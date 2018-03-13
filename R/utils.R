#' Format dollar amounts in terms of millions of USD
#'
#' Given a number, return a string formatted in terms of millions of dollars.
#'
#' @importFrom dplyr %>%
#' @param x A number.
#' @return String in the format of $xM.
#' @export
#' @examples
#' dollar_millions(1.523 * 10^6)
dollar_millions <- function(x) {
  # paste0('$', x / 10 ^ 6, 'M')
  x <- (x/10^6) %>% round(digits = 2)
  paste0("$", x, "M")
}

#' Calculate control weaknesses on a domain level
#'
#' For each domain, calculate the total number of threat events, loss
#' events, percentage of vulnerability, mean threat capability exceedance,
#' and mean difficult exceedance.
#'
#' @importFrom dplyr summarize_ mutate_ arrange_ group_by_ select left_join
#' @param simulation_results Results of running the risk simulations.
#' @param domains Domain titles and IDs as a dataframe.
#' @return Dataframe.
#' @export
#' @examples
#' data(simulation_results)
#' data(domains)
#' calculate_weak_domains(simulation_results, domains)
calculate_weak_domains <- function(simulation_results, domains) {
  control_weakness <- simulation_results %>% dplyr::group_by_("domain_id") %>%
    dplyr::summarize_(loss_events = ~ sum(loss_events), threat_events = ~ sum(threat_events)) %>%
    dplyr::mutate_(vuln = ~ loss_events/threat_events) %>%
    dplyr::arrange_(~ desc(vuln)) %>% dplyr::mutate_(vuln = ~ scales::percent(vuln))
  # recalc domain_level tc_exceedance
  tc_exceedance <- simulation_results %>% dplyr::group_by_("domain_id") %>%
    dplyr::mutate_(avoided_events = ~ threat_events - loss_events) %>%
    dplyr::summarize_(tc_exceedance = ~ sum(mean_tc_exceedance * loss_events),
                      diff_exceedance = ~ sum(mean_diff_exceedance * avoided_events),
                      avoided_events = ~ sum(avoided_events),
                      loss_events = ~ sum(loss_events)) %>%
    dplyr::mutate_(tc_exceedance = ~ tc_exceedance / loss_events,
                   diff_exceedance = ~diff_exceedance / avoided_events) %>%
    dplyr::mutate_(tc_exceedance = ~ ifelse(is.na(tc_exceedance), NA,
                                            scales::percent(tc_exceedance/100)),
                   diff_exceedance = ~ ifelse(is.na(diff_exceedance), NA,
                                              scales::percent(diff_exceedance/100))) %>%
    dplyr::select_("tc_exceedance", "diff_exceedance", "domain_id")
  control_weakness <- dplyr::left_join(control_weakness, tc_exceedance,
                                by = c(domain_id = "domain_id")) %>%
    dplyr::left_join(domains, by = c(domain_id = "domain_id")) %>%
      dplyr::mutate_(domain = ~ paste0(domain, " (", domain_id, ")"))
  control_weakness
}

#' Calculate quantified impact at a domain level
#'
#' Given a dataframe of simulation results summarized at the domain level,
#' create a summarization of the annual loss expected (ALE) with descriptors
#' at the minimum, mean, maximum, standard deviation, and 95% value at risk
#' levels.
#'
#' @importFrom dplyr select_ summarize_at funs arrange_ ungroup left_join mutate_
#' @importFrom stats sd quantile
#' @param domain_summary Domain-level summary of simulation results as returned by \code{\link{summarize_domains}}.
#' @param domains Dataframe of all domains in scope.
#' @return Dataframe.
#' @export
#' @examples
#' data(domain_summary)
#' data(domains)
#' calculate_domain_impact(domain_summary, domains)
calculate_domain_impact <- function(domain_summary, domains) {
  domain_summary %>% dplyr::group_by_(~domain_id) %>%
    dplyr::select_("domain_id", "ale") %>%
    dplyr::summarize_at(vars("ale"), dplyr::funs(min, mean, max, sd,
                                                 var = quantile(., probs = 0.95))) %>%
    dplyr::arrange_("desc(var)") %>% dplyr::ungroup() %>%
    dplyr::left_join(domains, by = c(domain_id = "domain_id")) %>%
    dplyr::mutate_(domain = quote(paste0(domain, " (", domain_id, ")")))
}

#' Calculate maximum losses
#'
#' Calculate the biggest single annual loss for each scenario, as well as
#' the minimum and maximum ALE across all simulations. Calculations both
#' with and without outliers (if passed) are returned.
#'
#' @importFrom dplyr filter_ group_by_ summarize_ ungroup union
#' @param simulation_results Simulation results dataframe.
#' @param scenario_outliers Optional vector of IDs of outlier scenarios.
#' @return Dataframe.
#' @export
#' @examples
#' data(simulation_results)
#' calculate_max_losses(simulation_results)
calculate_max_losses <- function(simulation_results, scenario_outliers = NULL) {
  max_loss <- simulation_results %>%
    dplyr::filter_(~ !scenario_id %in% scenario_outliers) %>%
    dplyr::group_by_("simulation") %>%
    dplyr::summarize_(biggest_single_scenario_loss = ~ max(ale),
                      min_loss = ~ min(ale), max_loss = ~ sum(ale),
                      outliers = FALSE) %>%
    dplyr::ungroup()
  max_loss_w_outliers <- simulation_results %>%
    dplyr::group_by_("simulation") %>%
    dplyr::summarize_(biggest_single_scenario_loss = ~ max(ale),
                      min_loss = ~ min(ale),
                      max_loss = ~ sum(ale), outliers = TRUE) %>%
    dplyr::ungroup()
  dplyr::union(max_loss, max_loss_w_outliers)
}
