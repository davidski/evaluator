#' Run simulations for all scenarios.
#'
#' @import dplyr
#' @param scenario Quantitative scenarios
#' @param simulation_count Number of simulations for each scenario
#' @export
#' @return Dataframe of raw results
run_simulations <- function(scenario, simulation_count = 10000L) {


  ## ----run_simulations-----------------------------------------------------
  pb <- tcltk::tkProgressBar(title = "Evaluator simulations",
                      label = "Working on scenario ",
                      min = 1, max = nrow(scenario), initial = 1)

  simulation_results <- purrrlyr::by_row(scenario, function(x) {
    #info <- sprintf("%d%% done", round(i))
    tcltk::setTkProgressBar(pb, x$scenario_id,
                     label = sprintf("Running scenario %s of %s",
                                     x$scenario_id, nrow(scenario)))
    safe_calculate <- purrr::safely(calculate_ale)
    safe_calculate(scenario = x,
                  diff_estimates = x[[1, "diff_params"]],
                  n = simulation_count,
                  title = x$scenario_id,
                  verbose = FALSE)},
    .labels = FALSE
  )

  close(pb)

  y <- simulation_results$`.out` %>% purrr::transpose()
  is_ok <- y$error %>% purrr::map_lgl(purrr::is_null)

  if (sum(is_ok) != nrow(scenario)) {
    stop("Error encountered with with scenario IDs: ",
         paste0(scenario[!is_ok,]$scenario_id, collapse = ", "))
  }

  simulation_results <- bind_rows(y$result)

  ## ----tidy_results--------------------------------------------------------
  # convert title back to scenario_id
  simulation_results %<>% mutate_("title" = ~ as.integer(title)) %>%
    rename_("scenario_id" = "title")

  # calculate the vuln percentage
  # note that for no threat events, we report a NA vuln value
  simulation_results %<>% mutate_(vuln = ~ 1 - (threat_events - loss_events) /
                                    threat_events,
                                  vuln = ~ ifelse(vuln < 0, 0, vuln))

  # add the domain_id column
  simulation_results %<>% left_join(select_(scenario, 'c(scenario_id, domain_id)'),
                                    by = c("scenario_id" = "scenario_id")) %>%
    select_("domain_id", "scenario_id", "simulation", "threat_events",
            "loss_events", "vuln", ~everything())

  # store the date on which this simulation set was generated
  attr(simulation_results, "generated_on") <- Sys.time()
  # store how many simulations ran on each scenario
  attr(simulation_results, "simulation_count") <- simulation_count

  simulation_results

}
