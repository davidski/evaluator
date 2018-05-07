#' Capabilities
#'
#' A qualitative dataset of sample capabilities and qualitative level of effectiveness.
#'
#' \describe{
#'   \item{id}{unique id of the capability}
#'   \item{domain_id}{domain id to which the capability applies}
#'   \item{capability}{full text summary of the capability}
#'   \item{diff}{qualitative label of control effectiveness}
#' }
"capabilities"

#' Qualitative to quantitative mappings
#'
#' A dataset of sample mappings from qualitative labels to quantitative
#' distribution parameters.
#'
#' \describe{
#'   \item{type}{OpenFAIR taxonomy to which this mapping applies}
#'   \item{label}{Qualitative label}
#'   \item{l}{BetaPERT low value}
#'   \item{ml}{BetaPERT most likely value}
#'   \item{h}{BetaPERT high value}
#'   \item{conf}{BetaPERT confidence value}
#' }
"mappings"

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
#'   \item{domain_id}{abbreviated name of the domain}
#'   \item{domain}{full title of the domain}
#'   \item{ale}{annual loss expected, in US dollars}
#'   \item{simulation}{simulation id number}
#' }
"domain_summary"

#' Scenario-level risk summary
#'
#' A dataset of quantified information security risk, summarized at the
#' scenario level.
#'
#' \describe{
#'   \item{domain_id}{domain id}
#'   \item{scenario_id}{ID of the scenario}
#'   \item{loss_events_mean}{mean number of loss events}
#'   \item{loss_events_min}{minimum number of loss events}
#'   \item{loss_events_max}{maximum number of loss events}
#'   \item{loss_events_median}{median number of loss events}
#'   \item{ale_median}{median annual loss expected}
#'   \item{ale_max}{maximum annual loss expected}
#'   \item{ale_var}{value at risk, ale}
#'   \item{sle_mean}{mean single loss expectancy}
#'   \item{sle_median}{median single loss expectancy}
#'   \item{sle_max}{maximum single loss expectancy}
#'   \item{sle_min}{minimum single loss expectancy}
#'   \item{mean_tc_exceedance}{mean threat capability exceedance}
#'   \item{mean_diff_exceedance}{mean difficulty exceedance}
#'   \item{mean_vuln}{mean vulnerability of the scenario}
#'   \item{ale_var_zscore}{Z-score of ale VaR}
#'   \item{outlier}{boolean - is this scenario an outlier}
#' }
"scenario_summary"

#' Quantified information risk scenarios
#'
#' A dataset of quantified risk scenarios, with parameters
#' describing the distribution of each input.
#'
#' \describe{
#'   \item{scenario}{full text description of the risk scenario}
#'   \item{scenario_id}{id of the scenario, primary key}
#'   \item{tcomm}{full text name of threat community}
#'   \item{domain_id}{domain abbreviation}
#'   \item{controls}{comma separated list of control ids that apply to this scenario}
#'   \item{diff_params}{nested dataframe of the controls and difficulty parameters associated with the scenario}
#'   \item{tef_params}{list of the threat expected frequency parameters}
#'   \item{tc_params}{list of the threat capability parameters}
#'   \item{lm_params}{list of the loss magniture parameters}
#' }
"quantitative_scenarios"

#' Qualitative information security risk scenarios
#'
#' A dataset of qualified information security risk scenarios.
#'
#' \describe{
#'   \item{scenario_id}{id of the scenario, primary key}
#'   \item{scenario}{full text description of the risk scenario}
#'   \item{tcomm}{full text name of threat community}
#'   \item{tef}{qualitative label of the frequency of threat events}
#'   \item{tc}{qualitative label of the threat capability}
#'   \item{lm}{qualitative label of the single loss magnitude}
#'   \item{domain_id}{domain id to which the scenario applies}
#'   \item{controls}{comma separate string of the controls for the scenario}
#' }
"qualitative_scenarios"

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
