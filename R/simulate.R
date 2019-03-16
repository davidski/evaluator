#' Run simulations for a scenario
#'
#' Given a quantitative scenario object, run an OpenFAIR Monte Carlo simulation
#' returning a dataframe of results.
#'
#' @import dplyr
#' @importFrom dplyr progress_estimated bind_rows %>% mutate row_number
#' @importFrom furrr future_map
#' @importFrom purrr safely is_null map_lgl transpose simplify keep
#' @importFrom tidyr nest
#' @importFrom rlang .data
#' @param scenario An \link{evaluator_scen} object.
#' @param iterations Number of iterations to run on each scenario.
#' @param simulation_count **DEPRECATED** Number of simulations to perform.
#' @param ale_maximum Maximum practical annual losses.
#' @param verbose Whether verbose console output is requested.
#' @export
#' @return Dataframe of raw results.
#' @examples
#' data(quantitative_scenarios)
#' # run a single scenario through a trivial number (10) of trials
#' run_simulations(quantitative_scenarios[[1, "scenario"]], 10)
run_simulations <- function(scenario, iterations = 10000L,
                            ale_maximum = NULL,
                            verbose = FALSE, simulation_count = NULL) {

  if (!is.null(simulation_count)) stop("simulation_count is deprecated. use `iterations` instead.", call. = FALSE)
  if (!class(scenario) %in% "evaluator_scen") {
    stop("Scenario must be an evaluator_scen object", call. = FALSE)
  }

  #model <- rlang::sym(model) # convert characters to symbol
  ## ----run_simulations-----------------------------------------------------
  wrapped_calc <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) & inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

    func <- x$model
    safe_calculate <- purrr::safely(eval(rlang::parse_expr(func)))
    safe_calculate(scenario = x,
                   n = iterations,
                   verbose = verbose)
  }

  #pb <- dplyr::progress_estimated(length(scenarios))
  # dat <- scenario %>% dplyr::mutate(id = row_number()) %>% tidyr::nest(-id)
  #pb$print()
  #simulation_results <- scenarios %>% furrr::future_map(~ {library(evaluator); wrapped_calc(.x, .pb = pb)}, .progress = TRUE)
  simulation_results <- wrapped_calc(scenario)
  #simulation_results <- safe_calculate(scenario = scenario,
  #                                     n = simulation_count,
  #                                     verbose = verbose)

  #y <- simulation_results %>% purrr::transpose() %>% purrr::simplify()
  #is_ok <- y$error %>% purrr::map_lgl(purrr::is_null)
  #errors <- y$error %>% purrr::keep(!is_ok)

  #if (sum(is_ok) != length(scenarios)) {
  if (!is.null(simulation_results$error)) {
    stop("Errors encountered with scenarios:\n",
         scenario,
         paste0("Error: ", simulation_results$error,
               collapse = "\n"))
         # paste(scenario[!is_ok,]$scenario_id, errors, sep = " - Error: ",
         #       collapse = "\n"))
  }

  simulation_results <- simulation_results$result

  # apply a maximum per year ALE, if requested
  if (!(is.null(ale_maximum))) simulation_results$ale <- pmin(simulation_results$ale, ale_maximum)

  # ## ----tidy_results--------------------------------------------------------
  # # convert title back to scenario_id
  # simulation_results <- rename(simulation_results, scenario_id = .data$title)
  #
  # # add the domain_id column
  # simulation_results <- simulation_results %>%
  #   left_join(select(scenario, c(.data$scenario_id, .data$domain_id)),
  #             by = "scenario_id") %>%
  #   select(.data$domain_id, .data$scenario_id, .data$simulation, .data$threat_events,
  #           .data$loss_events, .data$vuln, dplyr::everything())

  # store the date on which this simulation was generated
  attr(simulation_results, "generated_on") <- Sys.time()
  # store the number of iterations performed
  attr(simulation_results, "iterations") <- iterations

  simulation_results

}
