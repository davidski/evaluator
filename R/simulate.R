#' Run simulations for all scenarios
#'
#' Given a dataframe of quantitative scenarios, run an OpenFAIR Monte
#' Carlo simulation for each scenario, returning a combined dataframe of all
#' results.
#'
#' @import dplyr
#' @importFrom dplyr progress_estimated bind_rows %>% mutate row_number
#' @importFrom purrr safely is_null map_lgl transpose simplify keep
#' @importFrom tidyr nest
#' @importFrom rlang .data
#' @param scenario Quantitative scenarios.
#' @param model OpenFAIR model to use.
#' @param simulation_count Number of simulations for each scenario.
#' @param verbose Whether verbose console output is requested.
#' @export
#' @return Dataframe of raw results.
#' @examples
#' data(quantitative_scenarios)
#' # run a single scenario in a trivial number (10) of trials
#' run_simulations(quantitative_scenarios[1, ], 10)
run_simulations <- function(scenario, simulation_count = 10000L,
                            model = "openfair_tef_tc_diff_lm", verbose = FALSE) {

  #model <- rlang::sym(model) # convert characters to symbol
  ## ----run_simulations-----------------------------------------------------
  wrapped_calc <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) & inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

    safe_calculate <- purrr::safely(eval(as.name(model)))
    safe_calculate(scenario = x,
                  n = simulation_count,
                  title = x$scenario_id,
                  verbose = verbose)
    }

  pb <- dplyr::progress_estimated(nrow(scenario))
  dat <- scenario %>% dplyr::mutate(id = row_number()) %>% tidyr::nest(-id)
  pb$print()
  simulation_results <- dat$data %>% purrr::map(~ wrapped_calc(.x, .pb = pb))

  #simulation_results <- purrrlyr::by_row(scenario, wrapped_calc, .pb = pb, .labels = FALSE)
  #y <- simulation_results$`.out` %>% purrr::transpose()

  y <- simulation_results %>% purrr::transpose() %>% purrr::simplify()
  is_ok <- y$error %>% purrr::map_lgl(purrr::is_null)
  errors <- y$error %>% purrr::keep(!is_ok)

  if (sum(is_ok) != nrow(scenario)) {
    stop("Errors encountered with one or more scenarios:\n",
         paste(scenario[!is_ok,]$scenario_id, errors, sep = " - Error: ",
               collapse = "\n"))
  }

  simulation_results <- dplyr::bind_rows(y$result)

  ## ----tidy_results--------------------------------------------------------
  # convert title back to scenario_id
  simulation_results <- dplyr::mutate(simulation_results,
                                      title = as.integer(.data$title)) %>%
    rename(scenario_id = .data$title)

  # add the domain_id column
  simulation_results <- simulation_results %>%
    left_join(select(scenario, c(.data$scenario_id, .data$domain_id)),
              by = "scenario_id") %>%
    select(.data$domain_id, .data$scenario_id, .data$simulation, .data$threat_events,
            .data$loss_events, .data$vuln, dplyr::everything())

  # store the date on which this simulation set was generated
  attr(simulation_results, "generated_on") <- Sys.time()
  # store how many simulations ran on each scenario
  attr(simulation_results, "simulation_count") <- simulation_count

  simulation_results

}
