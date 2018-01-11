#' Run simulations for all scenarios
#'
#' Given a dataframe of quantitative scenarios, run an OpenFAIR Monte
#' Carlo simulation for each scenario, returning a combined dataframe of all
#' results.
#'
#' @import dplyr
#' @importFrom dplyr progress_estimated bind_rows %>% mutate row_number
#' @importFrom purrr safely is_null map_lgl transpose simplify
#' @importFrom tidyr nest
#' @param scenario Quantitative scenarios.
#' @param model OpenFAIR model to use.
#' @param simulation_count Number of simulations for each scenario.
#' @export
#' @return Dataframe of raw results.
run_simulations <- function(scenario, simulation_count = 10000L,
                            model = "openfair_tef_tc_diff_lm") {

  #model <- rlang::sym(model) # convert characters to symbol
  ## ----run_simulations-----------------------------------------------------
  wrapped_calc <- function(x, .pb = NULL) {
    if ((!is.null(.pb)) & inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

    safe_calculate <- purrr::safely(eval(as.name(model)))
    safe_calculate(scenario = x,
                  diff_estimates = x[[1, "diff_params"]],
                  n = simulation_count,
                  title = x$scenario_id,
                  verbose = FALSE)
    }

  pb <- dplyr::progress_estimated(nrow(scenario))
  dat <- scenario %>% dplyr::mutate(id = row_number()) %>% tidyr::nest(-id)
  pb$print()
  simulation_results <- dat$data %>% map(~ wrapped_calc(.x, .pb = pb))

  #simulation_results <- purrrlyr::by_row(scenario, wrapped_calc, .pb = pb, .labels = FALSE)
  #y <- simulation_results$`.out` %>% purrr::transpose()

  y <- simulation_results %>% purrr::transpose() %>% purrr::simplify()
  is_ok <- y$error %>% purrr::map_lgl(purrr::is_null)

  if (sum(is_ok) != nrow(scenario)) {
    stop("Error encountered with with scenario IDs: ",
         paste0(scenario[!is_ok,]$scenario_id, collapse = ", "))
  }

  simulation_results <- dplyr::bind_rows(y$result)

  ## ----tidy_results--------------------------------------------------------
  # convert title back to scenario_id
  simulation_results <- mutate_(simulation_results,
                                "title" = ~ as.integer(title)) %>%
    rename_("scenario_id" = "title")

  # add the domain_id column
  simulation_results <- simulation_results %>%
    left_join(select_(scenario, 'c(scenario_id, domain_id)'),
              by = c("scenario_id" = "scenario_id")) %>%
    select_("domain_id", "scenario_id", "simulation", "threat_events",
            "loss_events", "vuln", ~everything())

  # store the date on which this simulation set was generated
  attr(simulation_results, "generated_on") <- Sys.time()
  # store how many simulations ran on each scenario
  attr(simulation_results, "simulation_count") <- simulation_count

  simulation_results

}
