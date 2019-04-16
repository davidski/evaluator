#' Create a summary of the simulation results for a single scenario
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, create
#'   summary statistics for the scenario. This is generally the most granular
#'   level of useful data for reporting and analysis (full simulation results
#'   are rarely directly helpful).
#'
#'   Summary stats created include:
#'     * Mean/Min/Max/Median are calculated for loss events
#'     * Median/Max/VaR are calculated for annual loss expected (ALE)
#'     * Mean/Median/Max/Min are calculated for single loss expected (SLE)
#'     * Mean percentage of threat capability exceeding difficulty on successful threat events
#'     * Mean percentage of difficulty exceeding threat capability on defended events
#'     * Vulnerability percentage
#'
#' @importFrom dplyr mutate summarize
#' @importFrom rlang .data
#' @importFrom stats median quantile
#' @param simulation_result Results object for a single scenario.
#' @export
#' @return Dataframe of summary statistics.
#' @examples
#' data(mc_simulation_results)
#' # summarize a single scenario
#' summarize_scenario(mc_simulation_results[[1, "results"]])
#'
summarize_scenario <- function(simulation_result) {
  if (!is.data.frame(simulation_result) ||
      length(grepl("iteration", names(simulation_result))) == 0) {
    stop("simluation_result must be a bare result dataframe. Have you supplied a multi-scenario results object?",
         call. = FALSE)
  }
  scenario_summary <- simulation_result %>%
    dplyr::mutate(avoided_events = .data$threat_events - .data$loss_events) %>%
    dplyr::summarize(
      loss_events_mean = mean(.data$loss_events, na.rm = TRUE),
      loss_events_median = stats::median(.data$loss_events, na.rm = TRUE),
      loss_events_min = min(.data$loss_events, na.rm = TRUE),
      loss_events_max = max(.data$loss_events, na.rm = TRUE),
      ale_median = median(.data$ale, na.rm = TRUE),
      ale_max = max(.data$ale, na.rm = TRUE),
      ale_var = stats::quantile(.data$ale, 0.95),
      sle_mean = mean(.data$sle_median, na.rm = TRUE),
      sle_median = median(.data$sle_median, na.rm = TRUE),
      sle_min = min(.data$sle_min, na.rm = TRUE),
      sle_max = max(.data$sle_max, na.rm = TRUE),
      mean_tc_exceedance =
        (sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
        sum(.data$loss_events, na.rm = TRUE)) %>%
        ifelse(is.finite(.), ., 0),
      mean_diff_exceedance =
        (sum(.data$mean_diff_exceedance * (.data$threat_events - .data$loss_events), na.rm = TRUE) /
        sum(.data$threat_events - .data$loss_events, na.rm = TRUE)) %>%
        ifelse(is.finite(.), ., 0),
      mean_vuln = mean(.data$vuln, na.rm = TRUE)) %>%
    dplyr::mutate(sle_median = ifelse(is.nan(.data$sle_median), NA, .data$sle_median))
  scenario_summary
}

#' @export
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @param simulation_results Simulation results dataframe.
#' @rdname summarize_scenario
#' @examples
#' # summarize all scenarios in a data frame
#' data(mc_simulation_results)
#' summarize_scenarios(mc_simulation_results)
summarize_scenarios <- function(simulation_results) {
  dplyr::mutate(simulation_results, summary = purrr::map(.data$results, summarize_scenario)) %>%
    tidyr::unnest(summary)
}

#' Create a summary of outcomes across all scenarios
#'
#' Given a dataframe of raw results from \code{\link{run_simulations}}, summarize
#'   the individual results at a per-iteration level.
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
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom purrr every
#' @importFrom rlang .data ensym
#' @importFrom stats median
#' @param simulation_result Results object for a single scenario.
#' @param ... Additional simulation result objects to summarize.
#' @param .key Iteration ID field
#' @export
#' @return Dataframe.
#' @examples
#' data(mc_simulation_results)
#' summarize_iterations(mc_simulation_results$results)
summarize_iterations <- function(simulation_result, ..., .key = "iteration") {

  key <- rlang::ensym(.key)

  dots <- list(...)
  if (!purrr::every(dots, is.data.frame)) stop("All inputs must be data frames.")

  result_list <- dplyr::bind_rows(simulation_result, ...)

  result_list %>%
    dplyr::group_by(!!key) %>%
    dplyr::summarize(largest_single_scenario_loss = max(.data$ale),
                     min_loss = min(.data$ale),
                     max_loss = sum(.data$ale),
                     outliers = TRUE,
                     ale_sum = sum(.data$ale),
                     mean_tc_exceedance = (sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
                                             sum(.data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
                     mean_diff_exceedance = (sum(.data$mean_diff_exceedance * (.data$threat_events - .data$loss_events), na.rm = TRUE) /
                                               sum(.data$threat_events - .data$loss_events, na.rm = TRUE)) %>% ifelse(is.finite(.), ., 0),
                     loss_events = sum(.data$loss_events, na.rm = TRUE),
                     threat_events = sum(.data$threat_events, na.rm = TRUE),
                     avoided_events = .data$threat_events - .data$loss_events)

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
#'
#' @importFrom dplyr group_by mutate summarize select
#' @importFrom purrr map
#' @importFrom rlang .data ensym
#' @importFrom stats sd quantile
#' @importFrom tidyr unnest
#' @param simulation_results Simulation results dataframe.
#' @param domain_variable Variable by which individual simulations should be grouped.
#' @export
#' @return Simulation results summarized across domains.
#' @examples
#' \dontrun{
#' data(mc_simulation_results)
#' summarize_domains(mc_simulation_results)
#' }
summarize_domains <- function(simulation_results, domain_variable = "domain_id") {
  domain_variable <- rlang::ensym(domain_variable)
  if (!is.data.frame(simulation_results) ||
      length(grepl("results", names(simulation_results))) == 0) {
    stop("simluation_results must be a dataframe containing a results column.",
         call. = FALSE)
  }
  simulation_domain_sum <- simulation_results %>%
    dplyr::group_by(!!domain_variable) %>%
    dplyr::summarize(simulation_summary = list(summarize_iterations(.data$results)))
  simulation_domain_sum <- dplyr::mutate(
    simulation_domain_sum, summary = purrr::map(.data$simulation_summary, ~ {
      .x %>%
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
          mean_tc_exceedance =
            sum(.data$mean_tc_exceedance * .data$loss_events, na.rm = TRUE) /
            sum(.data$loss_events, na.rm = TRUE) %>%
            ifelse(is.finite(.), ., 0),
          mean_diff_exceedance =
            sum(.data$mean_diff_exceedance * (.data$threat_events - .data$loss_events), na.rm = TRUE) /
            sum(.data$threat_events - .data$loss_events, na.rm = TRUE) %>%
            ifelse(is.finite(.), ., 0),
          mean_vuln = (.data$mean_loss_events / .data$mean_threat_events)
        )
    }))
  dplyr::select(simulation_domain_sum, -.data$simulation_summary) %>%
    tidyr::unnest(summary)
}

#' Create all summary files and write to disk
#'
#' This is a wrapper around \code{\link{summarize_scenario}} and
#'   \code{\link{summarize_domains}}, calling both functions and writing the
#'   dataframes to a location on disk.
#'
#' @importFrom rlang .data
#' @importFrom tibble rownames_to_column as_tibble
#' @param simulation_results Simulation results dataframe.
#' @param results_dir Directory to place simulation files.
#'
#' @return Tibble with paths to the created data files.
#' @export
#' @examples
#' data(mc_simulation_results)
#' summarize_to_disk(mc_simulation_results, results_dir = tempdir())
summarize_to_disk <- function(simulation_results, results_dir) {
  if (!dir.exists(results_dir)) dir.create(results_dir)

  scenario_summary <- summarize_scenarios(simulation_results)
  saveRDS(scenario_summary, file = file.path(results_dir, "scenario_summary.rds"))

  domain_summary <- summarize_domains(simulation_results)
  saveRDS(domain_summary, file = file.path(results_dir, "domain_summary.rds"))

  file.info(c(file.path(results_dir, "scenario_summary.rds"),
              file.path(results_dir, "domain_summary.rds"))) %>%
    tibble::rownames_to_column("filename") %>% tibble::as_tibble()
}
