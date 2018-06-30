#' Create simulation-level summary of simulation results
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#'   the individual results at a per-simulation level.
#'
#'   Summary stats created include:
#'     * Mean/Min/Max/Median are calculated for loss events
#'     * Median/Max/VaR are calculated for annual loss expected (ALE)
#'     * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#'     * Mean percentage of threat capability exceeding difficulty on successful threat events
#'     * Mean percentage of difficulty exceeding threat capability on defended events
#'     * Vulnerability percentage
#'     * Z-score of ALE (outliers flagged as 2 >= z-score)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats median
#' @param simulation_results Simulation results dataframe.
#' @export
#' @return Dataframe.
#' @examples
#' data(simulation_results)
#' summarize_scenarios(simulation_results)
summarize_simulations <- function(simulation_results) {
  simulation_results %>%
    dplyr::group_by(.data$simulation) %>%
    dplyr::summarize(biggest_single_scenario_loss = max(.data$ale),
                     min_loss = min(.data$ale), max_loss = sum(.data$ale),
                     max_loss = sum(.data$ale), outliers = TRUE)
}

#' Create scenario-level summary of simulation results
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#'   the individual results at a per-scenario level. This is generally the most
#'   granular level of data for reporting and analysis (full simulation results
#'   are rarely directly helpful).
#'
#'   Summary stats created include:
#'     * Mean/Min/Max/Median are calculated for loss events
#'     * Median/Max/VaR are calculated for annual loss expected (ALE)
#'     * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#'     * Mean percentage of threat capability exceeding difficulty on successful threat events
#'     * Mean percentage of difficulty exceeding threat capability on defended events
#'     * Vulnerability percentage
#'     * Z-score of ALE (outliers flagged as 2 >= z-score)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats median quantile
#' @param simulation_results Simulation results dataframe.
#' @export
#' @return Dataframe.
#' @examples
#' data(simulation_results)
#' summarize_scenarios(simulation_results)
summarize_scenarios <- function(simulation_results) {
  scenario_summary <- simulation_results %>%
    dplyr::group_by_at(.vars = c("domain_id", "scenario_id")) %>%
    dplyr::summarize(
    loss_events_mean = mean(.data$loss_events, na.rm = TRUE),
    loss_events_min = min(.data$loss_events, na.rm = TRUE),
    loss_events_max = max(.data$loss_events, na.rm = TRUE),
    loss_events_median = stats::median(.data$loss_events, na.rm = TRUE),
    ale_median = median(.data$ale, na.rm = TRUE),
    ale_max = max(.data$ale, na.rm = TRUE),
    ale_var = stats::quantile(.data$ale, 0.95),
    sle_mean = mean(.data$sle_median, na.rm = TRUE),
    sle_median = median(.data$sle_median, na.rm = TRUE),
    sle_max = max(.data$sle_max, na.rm = TRUE),
    sle_min = min(.data$sle_min, na.rm = TRUE),
    mean_tc_exceedance = (sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
      sum(.data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
    mean_diff_exceedance = (sum(.data$mean_diff_exceedance *
                                  (.data$threat_events - .data$loss_events), na.rm = TRUE) /
      sum(.data$threat_events - .data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
    mean_vuln = mean(.data$vuln, na.rm = TRUE)) %>%
  dplyr::mutate(sle_median = ifelse(is.nan(.data$sle_median), NA, .data$sle_median)) %>%
  dplyr::ungroup()

  # calculate z-score for ALE VaR and assign outliers as >= 2 SD
  dplyr::mutate(scenario_summary,
                ale_var_zscore = scale(.data$ale_var),
                outlier = .data$ale_var_zscore >= 2)
}

#' Create domain-level summary of simulation results
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#'   the individual results at a per-domain level. This domain-level summary
#'   is a useful data structure for aggregate reporting.
#'
#'   Summary stats created include:
#'   * Mean/Min/Max/Median are calculated for loss events
#'   * Median/Max/VaR are calculated for annual loss expected (ALE)
#'   * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#'   * Mean percentage of threat capability exceeding difficulty on successful threat events
#'   * Mean percentage of difficulty exceeding threat capability on defended events
#'   * Vulnerability percentage
#'   * Z-score of ALE (outliers flagged as 2 >= z-score)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats sd quantile
#' @param simulation_results Simulation results dataframe.
#' @export
#' @return Simulation results summarized by domain
#' @examples
#' data(simulation_results)
#' summarize_domains(simulation_results)
summarize_domains <- function(simulation_results) {
  simulation_results %>%
    group_by(.data$domain_id, .data$simulation) %>%
    dplyr::mutate(avoided_events = .data$threat_events - .data$loss_events) %>%
    dplyr::summarize(ale_sum = sum(.data$ale),
                     loss_events = sum(.data$loss_events, na.rm = TRUE),
                     threat_events = sum(.data$threat_events, na.rm = TRUE),
                     avoided_events = sum(.data$avoided_events, na.rm = TRUE),
                     loss_events = sum(.data$loss_events, na.rm = TRUE),
                     mean_tc_exceedance = (sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
                                        sum(.data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
                     mean_diff_exceedance = (sum(.data$mean_diff_exceedance * (.data$threat_events - .data$loss_events), na.rm = TRUE) /
                                               sum(.data$threat_events - .data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0)) %>%
    dplyr::summarize(
      ale_min = min(.data$ale_sum, na.rm = TRUE),
      ale_median = median(.data$ale_sum, na.rm = TRUE),
      ale_mean = mean(.data$ale_sum, na.rm = TRUE),
      ale_max = max(.data$ale_sum, na.rm = TRUE),
      ale_sd = stats::sd(.data$ale_sum, na.rm = TRUE),
      ale_var = stats::quantile(.data$ale_sum, probs = 0.95, na.rm = TRUE),
      mean_threat_events = mean(.data$threat_events, na.rm = TRUE),
      mean_loss_events = mean(.data$loss_events, na.rm = TRUE),
      mean_avoided_events = mean(.data$avoided_events, na.rm = TRUE),
      mean_tc_exceedance = (sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
                              sum(.data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
      mean_diff_exceedance = (sum(.data$mean_diff_exceedance * (.data$threat_events - .data$loss_events), na.rm = TRUE) /
                                sum(.data$threat_events - .data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
      mean_vuln = (.data$mean_loss_events / .data$mean_threat_events)

    ) %>%
    dplyr::select(.data$domain_id, .data$ale, dplyr::everything())
}

#' Create all summary files and write to disk
#'
#' This is a wrapper around \code{\link{summarize_scenarios}} and
#'   \code{\link{summarize_domains}}, calling both functions and writing the
#'   dataframes to a location on disk.
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

  domain_summary <- summarize_domains(simulation_results)
  save(domain_summary, file = file.path(results_dir, "domain_summary.rda"))

  file.info(c(file.path(results_dir, "scenario_summary.rda"),
              file.path(results_dir, "domain_summary.rda"))) %>%
    tibble::rownames_to_column("filename") %>% tibble::as_tibble()
}
