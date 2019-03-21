#' Run simulations for a scenario
#'
#' Given a quantitative scenario object of type `tidyrisk_scenario`, run an
#' OpenFAIR Monte Carlo simulation.
#'
#' @import dplyr
#' @importFrom dplyr progress_estimated bind_rows %>% mutate row_number
#' @importFrom purrr safely is_null map_lgl transpose simplify keep some
#' @importFrom tidyr nest
#' @importFrom rlang .data exec
#' @param scenario A \link{tidyrisk_scenario} object.
#' @param iterations Number of iterations to run on each scenario.
#' @param simulation_count **DEPRECATED** Number of simulations to perform.
#' @param ale_maximum Maximum practical annual losses.
#' @param verbose Whether verbose console output is requested.
#' @export
#' @return Dataframe of results.
#' @examples
#' data(quantitative_scenarios)
#' run_simulation(quantitative_scenarios[[1, "scenario"]], 10)
run_simulation <- function(scenario, iterations = 10000L,
                            ale_maximum = NULL,
                            verbose = FALSE, simulation_count = NULL) {

  if (!is.null(simulation_count)) stop("simulation_count is deprecated. use `iterations` instead.", call. = FALSE)

  if (!is_tidyrisk_scenario(scenario)) {
    stop("Scenario must be a tidyrisk_scenario object", call. = FALSE)
  }

  # check for required elements
  required_elements <- formals(scenario$model) %>% names %>%
    setdiff(c("n", "verbose"))
  if (purrr::some(scenario$parameters[required_elements], is.null)) {
    stop("Missing one or more required elements.", call. = FALSE)
  }

  #model <- rlang::sym(model) # convert characters to symbol
  func <- scenario$model
  params <- c(scenario$parameters, list(n = iterations, verbose = verbose))
  wrapped_calc <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) & inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

    safe_calculate <- purrr::safely(eval(rlang::parse_expr(func)))
    rlang::exec(safe_calculate, !!!params)
  }

  simulation_results <- wrapped_calc(scenario)

  if (!is.null(simulation_results$error)) {
    stop("Errors encountered with scenarios:\n",
         scenario,
         paste0("Error: ", simulation_results$error,
               collapse = "\n"))
  }

  simulation_results <- simulation_results$result

  # apply a maximum per year ALE, if requested
  if (!(is.null(ale_maximum))) simulation_results$ale <- pmin(simulation_results$ale, ale_maximum)

  # store the date on which this simulation was generated
  attr(simulation_results, "generated_on") <- Sys.time()
  # store the number of iterations performed
  attr(simulation_results, "iterations") <- iterations

  simulation_results

}



#' Run simulations for a list of scenarios
#'
#' Given a list of quantitative scenario objects of type `tidyrisk_scenario`,
#' run a OpenFAIR Monte Carlo simulation for each scenario.
#'
#' @inheritParams run_simulation
#' @param ... Additional `tidyrisk_scenario` objects to simulate
#' @importFrom purrr map every
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @return A list of one dataframe of results for each scenario
#' @export
#' @examples
#' # fetch three scenarios for this example
#' data(quantitative_scenarios)
#' scenario_a <- quantitative_scenarios[[1, "scenario"]]
#' scenario_b <- quantitative_scenarios[[2, "scenario"]]
#' scenario_c <- quantitative_scenarios[[3, "scenario"]}
#' run_simulations(scenario_a, scenario_b, scenario_c, iterations = 10)
#'
run_simulations <- function(scenario, ..., iterations = 10000L,
                            ale_maximum = NULL,
                            verbose = FALSE, simulation_count = NULL) {

  scenarios <- list(scenario, ...)
  if (!purrr::every(scenarios, is_tidyrisk_scenario)) {
    stop("All scenarios must be tidyrisk_scenario objects", call. = FALSE)
  }

  if (!is.null(simulation_count)) stop("simulation_count is deprecated. use `iterations` instead.", call. = FALSE)

  map(scenarios, run_simulation, iterations, ale_maximum, verbose)

}
