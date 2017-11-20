#' Domain mappings
#'
#' A dataset of domains and domain IDs.
#'
#' \describe{
#'   \item{domain_id}{abbreviated name of the domain}
#'   \item{domain}{full title of the domain}
#' }
"domains"

#' Domain-level risk summary
#'
#' A dataset of quantified information security risk, summarized at the
#' domain level.
#'
#' \describe{
#'   \item{ale}{annual loss expected, in US dollars}
#'   \item{domain}{full title of the domain}
#'   \item{domain_id}{abbreviated name of the domain}
#'   \item{simulation}{simulation id number}
#' }
"domain_summary"

#' Scenario-level risk summary
#'
#' A dataset of quantified information security risk, summarized at the
#' scenario level.
#'
#' \describe{
#'   \item{ale_max}{maximum annual loss expected}
#'   \item{ale_median}{median annual loss expected}
#'   \item{ale_var}{value at risk, ale}
#'   \item{ale_var_zscore}{Z-score of ale VaR}
#'   \item{domain_id}{domain id}
#'   \item{loss_events_max}{maximum number of loss events}
#'   \item{loss_events_mean}{mean number of loss events}
#'   \item{loss_events_median}{median number of loss events}
#'   \item{loss_events_min}{minimum number of loss events}
#'   \item{mean_diff_exceedance}{mean difficulty exceedance}
#'   \item{mean_tc_exceedance}{mean threat capability exceedance}
#'   \item{mean_vuln}{mean vulnerability of the scenario}
#'   \item{outlier}{is this scenario an outlier}
#'   \item{scenario_id}{ID of the scenario}
#'   \item{sle_max}{single loss expectance max}
#'   \item{sle_mean}{mean single loss expectance}
#'   \item{sle_median}{median single loss expectance}
#'   \item{sle_min}{minimum single loss expectance}
#' }
"scenario_summary"

#' Information security risk scenarios
#'
#' A dataset of quantified information security risk scenarios, with parameters
#' describing the distribution of each input.
#'
#' \describe{
#'   \item{scenario}{full text description of the risk scenario}
#'   \item{scenario_id}{id of the scenario, primary key}
#'   \item{tcomm}{full text name of threat community}
#'   \item{domain_id}{domain abbreviation}
#'   \item{controls}{comma separated list of control ids that apply to this scenario}
#'   \item{lm_l}{loss magnitude - low}
#'   \item{lm_ml}{loss magnitude - most likely}
#'   \item{lm_h}{loss magnitude - high}
#'   \item{lm_conf}{loss magnitude - confidence}
#'   \item{tc_l}{threat capability - low}
#'   \item{tc_ml}{threat capability - most likely}
#'   \item{tc_h}{threat capability - high}
#'   \item{tc_conf}{threat capability - confidence}
#'   \item{tef_l}{threat event frequency - low}
#'   \item{tef_ml}{threat event frequency - most likely}
#'   \item{tef_h}{threat event frequency - high}
#'   \item{tef_conf}{threat event frequency - confidence}
#' }
"quantitative_scenarios"

#' Information security risk simulation results
#'
#' A dataset containing the full results of sample Monte Carlo
#' simulations of information security risk scenarios.
#'
#' \describe{
#'   \item{domain_id}{domain abbreviation}
#'   \item{scenario_id}{id of the scenario}
#'   \item{simulation}{id of the simulation}
#'   \item{threat_events}{number of threat events}
#'   \item{loss_events}{number of loss events occurring in the simulation}
#'   \item{vuln}{percentage of threat events that result in losses}
#'   \item{mean_tc_exceedance}{mean amount of TC > DIFF}
#'   \item{mean_diff_exceedance}{mean amount of DIFF > TC}
#'   \item{ale}{annual loss expectancy}
#'   \item{sle_min}{single loss expectancy - minimum}
#'   \item{sle_mean}{single loss expectancy - mean}
#'   \item{sle_median}{single loss expectancy - mean}
#'   \item{sle_max}{single loss expectancy - maximum}
#' }
"simulation_results"
