#' Create scenario-level summary of simulation results
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#' the individual results at a per-scenario level. This is generally the most
#' granular level of data for reporting and analysis (full simulation results
#' are rarely directly helpful).
#'
#' Summary stats created include:
#' * Mean/Min/Max/Median are calculated for loss events
#' * Median/Max/VaR are calculated for annual loss expected (ALE)
#' * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#' * Mean percentage of threat capability exceeding difficulty on successful threat events
#' * Mean percentage of difficulty exceeding threat capability on defended events
#' * Vulnerability percentage
#' * Z-score of ALE (outliers flagged as 2 >= z-score)
#'
#' @import dplyr
#' @param simulation_results Simulation results dataframe.
#' @export
#' @return Dataframe.
#' @examples
#' data(simulation_results)
#' summarize_scenarios(simulation_results)
summarize_scenarios <- function(simulation_results) {
  scenario_summary <- simulation_results %>%
    dplyr::group_by_("domain_id", "scenario_id") %>%
    dplyr::summarize_(
    loss_events_mean = ~ mean(loss_events, na.rm = TRUE),
    loss_events_min = ~ min(loss_events, na.rm = TRUE),
    loss_events_max = ~ max(loss_events, na.rm = TRUE),
    loss_events_median = ~ median(loss_events, na.rm = TRUE),
    ale_median = ~ median(ale, na.rm = TRUE),
    ale_max = ~ max(ale, na.rm = TRUE),
    ale_var = ~ quantile(ale, 0.95),
    sle_mean = ~ mean(sle_median, na.rm = TRUE),
    sle_median = ~ median(sle_median, na.rm = TRUE),
    sle_max = ~ max(sle_max, na.rm = TRUE),
    sle_min = ~ min(sle_min, na.rm = TRUE),
    mean_tc_exceedance = ~ (sum(mean_tc_exceedance * loss_events) /
      sum(loss_events)) %>% ifelse(is.finite(.), ., 0),
    mean_diff_exceedance = ~(sum(mean_diff_exceedance *
                                  (threat_events - loss_events)) /
      sum(threat_events - loss_events)) %>% ifelse(is.finite(.), ., 0),
    mean_vuln = ~ mean(vuln, na.rm = TRUE)) %>%
  dplyr::mutate_(sle_median = ~ ifelse(is.nan(sle_median), NA, sle_median)) %>%
  dplyr::ungroup()

  # calculate z-score for ALE VaR and assign outliers as >= 2 SD
  dplyr::mutate_(scenario_summary, ale_var_zscore = ~ scale(ale_var),
                 outlier = ~ ale_var_zscore >= 2)
}

#' Create domain-level summary of simulation results
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#' the individual results at a per-domain level. This domain-level summary
#' is a useful data structure for aggregate reporting.
#'
#' Summary stats created include:
#' * Mean/Min/Max/Median are calculated for loss events
#' * Median/Max/VaR are calculated for annual loss expected (ALE)
#' * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#' * Mean percentage of threat capability exceeding difficulty on successful threat events
#' * Mean percentage of difficulty exceeding threat capability on defended events
#' * Vulnerability percentage
#' * Z-score of ALE (outliers flagged as 2 >= z-score)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @param simulation_results Simulation results dataframe.
#' @param domains Domain mappings dataframe.
#' @export
#' @return Simulation results summarized by domain
#' @examples
#' data(simulation_results)
#' data(domains)
#' summarize_domains(simulation_results, domains)
summarize_domains <- function(simulation_results, domains) {
  simulation_results %>% group_by_("domain_id", "simulation") %>%
    dplyr::summarize(ale = sum(.data$ale)) %>%
    dplyr::left_join(domains, by = c("domain_id" = "domain_id")) %>%
    dplyr::select(.data$domain_id, .data$domain, .data$simulation, .data$ale, everything())
}

#' Create all summary files and write to disk
#'
#' This is a wrapper around \code{\link{summarize_scenarios}} and
#' \code{\link{summarize_domains}}, calling both functions and writing the
#' dataframes to a location on disk.
#'
#' @importFrom dplyr "%>%"
#' @importFrom tibble rownames_to_column as_tibble
#' @param simulation_results Simulation results dataframe.
#' @param domains Domain mappings dataframe.
#' @param results_dir Directory to place simulation files.
#' @export
#' @return Tibble with paths to the created data files.
#' @examples
#' \dontrun{
#' data(simulation_results)
#' data(domains)
#' summarize_to_disk(simulation_results, domains)
#' }
summarize_to_disk <- function(simulation_results, domains,
                              results_dir = "~/results") {
  if (!dir.exists(results_dir)) dir.create(results_dir)

  scenario_summary <- summarize_scenarios(simulation_results)
  save(scenario_summary, file = file.path(results_dir, "scenario_summary.rda"))

  domain_summary <- summarize_domains(simulation_results, domains)
  save(domain_summary, file = file.path(results_dir, "domain_summary.rda"))

  file.info(c(file.path(results_dir, "scenario_summary.rda"),
              file.path(results_dir, "domain_summary.rda"))) %>%
    tibble::rownames_to_column("filename") %>% tibble::as_tibble()
}
