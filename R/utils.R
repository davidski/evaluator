#' Helper function to verify package availability
#'
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @param packages Packages to verify are available.
#' @param func Calling function.
#'
#' @return Invisible
#' @noRd
#' @keywords internal
#' \dontrun{
#' check_availability(packages = c("ggplot2", "dplyr"), func = "my_function")
#' }
check_availability <- function(packages, func) {
  res <- purrr::map_df(packages,
                       ~ tibble::tibble(package = .x,
                                        available = requireNamespace(.x, quietly=TRUE)))
  if (sum(res$available) != length(packages)) {
    stop(func, " requires the following packages which are not available: ",
         paste0(res[, ]$package, collapse = ", "), "\n",
         "Please install and try again.", call. = FALSE)
  }

  invisible()

}

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

#' Unnest a summarized results dataframe, adding outlier information
#'
#' Given a summarized results dataframe, unnest the summary results
#' column and use the value at risk (VaR) column to identify all the
#' elements that are outliers (having a VaR >= two standard deviations)
#'
#' @param results Scenario summary results
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
#' @return The supplied dataframe with the following additional columns:
#'   - `ale_var_zscore` - Annual loss z-score
#'   - `outlier` - Logical flag when the z-score is greater than or equal to two
#' @examples
#' data(mc_scenario_summary)
#' identify_outliers(mc_scenario_summary)
identify_outliers <- function(results) {
 results %>% #tidyr::unnest(summary) %>%
    dplyr::mutate(ale_var_zscore = as.vector(scale(.data$ale_var)),
                  outlier = .data$ale_var_zscore >= 2)
}


#' Calculate maximum losses
#'
#' Calculate the biggest single annual loss for each scenario, as well as
#'   the minimum and maximum ALE across all iterations. Calculations both
#'   with and without outliers (if passed) are returned.
#'
#' @importFrom dplyr filter group_by summarize ungroup union
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @param simulation_results Simulation results dataframe.
#' @param scenario_outliers Optional vector of IDs of outlier scenarios.
#' @return A dataframe with the following columns:
#'   - `iteration` - index of the iteration
#'   - `biggest_single_scenario_loss` - the biggest annual loss in that iteration,
#'   - `min_loss` - the smallest annual loss in that iteration,
#'   - `max_loss` - the total annual losses in that iteration
#'   - `outliers` - logical of whether or not outliers are included
#' @export
#' @examples
#' data(mc_simulation_results)
#' calculate_max_losses(mc_simulation_results)
calculate_max_losses <- function(simulation_results, scenario_outliers = NULL) {
  max_loss <- tidyr::unnest(simulation_results, .data$results) %>%
    dplyr::filter(!.data$scenario_id %in% scenario_outliers) %>%
    dplyr::group_by(.data$iteration) %>%
    dplyr::summarize(biggest_single_scenario_loss = max(.data$ale),
                      min_loss = min(.data$ale), max_loss = sum(.data$ale),
                      outliers = FALSE) %>%
    dplyr::ungroup()
  # if we're not passed any outliers, don't bother calculating outliers
  if (is.null(scenario_outliers)) return(max_loss)

  max_loss_w_outliers <- tidyr::unnest(simulation_results, .data$results) %>%
    dplyr::group_by(.data$iteration) %>%
    dplyr::summarize(biggest_single_scenario_loss = max(.data$ale),
                      min_loss = min(.data$ale),
                      max_loss = sum(.data$ale), outliers = TRUE) %>%
    dplyr::ungroup()
  dplyr::union(max_loss, max_loss_w_outliers)
}
