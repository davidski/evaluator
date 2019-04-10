#' Create a skeleton tidyrisk scenario object in the current document
#'
#' Inserts a code block into the active document in RStudio for creating a
#'   \link{tidyrisk_scenario} object. This is an easy way of rapidly running
#'   a simulation.
#'
#' @export
#' @importFrom rstudioapi insertText
create_tidyrisk_scenario_skeleton <- function() {
  rstudioapi::insertText(text =
    "my_scen <- tidyrisk_scenario(
        tef_params = list(min = 1, mode = 10, max = 100, shape = 3, func = \"mc2d::rpert\"),
        tc_params = list(min = .20, mode = .30, max = .70, shape = 2, func = \"mc2d::rpert\"),
        diff_params = list(list(min = .25, mode = .50, max = .60, shape = 3, func = \"mc2d::rpert\")),
        lm_params = list(min = 100, mode = 1000, max = 10000, shape = 3, func = \"mc2d::rpert\"))

my_results <- run_simulation(my_scen, iterations = 1000)
"
    )
}

