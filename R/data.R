#' Capabilities
#'
#' A qualitative dataset of sample capabilities and qualitative level of effectiveness.
#'
#' \describe{
#'   \item{capability_id}{unique id of the capability}
#'   \item{domain_id}{domain id to which the capability applies}
#'   \item{capability}{full text summary of the capability}
#'   \item{diff}{qualitative label of control effectiveness}
#' }
"capabilities"

#' Qualitative to quantitative mappings
#'
#' A dataset of sample mappings from qualitative labels to quantitative
#'   distribution parameters.
#'
#' \describe{
#'   \item{type}{The element in the OpenFAIR ontology to which this mapping applies}
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
#'   domain level.
#'
#' \describe{
#'   \item{domain_id}{abbreviated name of the domain}
#'   \item{loss_events_mean}{mean number of loss events}
#'   \item{loss_events_min}{minimum number of loss events}
#'   \item{loss_events_max}{maximum number of loss events}
#'   \item{loss_events_median}{median number of loss events}
#'   \item{ale_max}{minimum annual loss expected}
#'   \item{ale_median}{median annual loss expected}
#'   \item{ale_mean}{mean annual loss expected}
#'   \item{ale_max}{maximum annual loss expected}
#'   \item{ale_sd}{standard deviation annual loss expected}
#'   \item{ale_var}{value at risk, ale}
#'   \item{mean_threat_events}{mean threat events}
#'   \item{mean_avoided_events}{mean avoided events}
#'   \item{mean_tc_exceedance}{mean threat capability exceedance}
#'   \item{mean_diff_exceedance}{mean difficulty exceedance}
#'   \item{mean_vuln}{mean vulnerability of the scenario}
#' }
"domain_summary"

#' Scenario-level risk summary
#'
#' A dataset of quantified information security risk, summarized at the
#'   scenario level.
#'
#' \describe{
#'   \item{scenario_id}{ID of the scenario}
#'   \item{domain_id}{domain id}
#'   \item{control_description}{control description}
#'   \item{results}{nested data frame of simulation results for the scenario}
#'   \item{loss_events_mean}{mean number of loss events}
#'   \item{loss_events_median}{median number of loss events}
#'   \item{loss_events_min}{minimum number of loss events}
#'   \item{loss_events_max}{maximum number of loss events}
#'   \item{ale_median}{median annual loss expected}
#'   \item{ale_max}{maximum annual loss expected}
#'   \item{ale_var}{value at risk, ale}
#'   \item{sle_min}{minimum single loss expectancy}
#'   \item{sle_max}{maximum single loss expectancy}
#'   \item{sle_mean}{mean single loss expectancy}
#'   \item{sle_median}{median single loss expectancy}
#'   \item{mean_tc_exceedance}{mean threat capability exceedance}
#'   \item{mean_diff_exceedance}{mean difficulty exceedance}
#'   \item{mean_vuln}{mean vulnerability of the scenario}
#' }
"scenario_summary"

#' Quantified information risk scenarios
#'
#' A dataset of quantified risk scenarios, with parameters
#'   describing the distribution of each input.
#'
#' \describe{
#'   \item{scenario_id}{id of the scenario, primary key}
#'   \item{scenario_description}{full text description of the risk scenario}
#'   \item{tcomm}{description of the threat community}
#'   \item{domain_id}{domain id}
#'   \item{control_descriptons}{named list of the text description of controls involved}
#'   \item{scenario}{\code{\link{tidyrisk_scenario}} objects}

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
#'   \item{tef}{qualitative label of threat frequency}
#'   \item{tc}{qualitative label of threat capability}
#'   \item{lm}{qualitative label of loss magnitude}
#'   \item{domain_id}{domain id}
#'   \item{controls}{comma delimited list of controls ids}
#' }
"qualitative_scenarios"

#' Information security risk simulation results
#'
#' A dataset containing the full results of sample Monte Carlo
#'   simulations of information security risk scenarios.
#'
#' \describe{
#'   \item{scenario_id}{id of the scenario}
#'   \item{domain_id}{domain id}
#'   \item{results}{nested data frame of simulation results for the scenario}
#' }
"simulation_results"
