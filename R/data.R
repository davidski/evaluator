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
#'   \item{summary}{nested dataframe with summary statistics}
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
#'   \item{results}{nested data frame of simulation results for the scenario}
#'   \item{summary}{nested data frame of summary statistics for the scenario}
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
