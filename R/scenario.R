#' Construct a quantitative scenario object
#'
#' @param tef_params List of threat frequency parameters
#' @param tc_params List of threat capability parameters
#' @param diff_params List of difficulty parameters
#' @param lm_params List of loss magnitude parameters
#' @param model Name of model to run
#' @export
new_tidyrisk_scenario <- function(tef_params = list(),
                               tc_params = list(),
                               diff_params = list(),
                               lm_params = list(),
                               model = "openfair_tef_tc_diff_lm") {
  stopifnot(is.list(tef_params), is.list(tc_params),
            is.list(diff_params), is.list(lm_params),
            is.character(model))
  scenario <- list(tef_params = tef_params,
                   tc_params = tc_params,
                   diff_params = diff_params,
                   lm_params = lm_params,
                   model = model)
  class(scenario) <- c("tidyrisk_scenario") #class(scenario))
  scenario
}

#' @export
#' @rdname new_tidyrisk_scenario
tidyrisk_scenario <- function(tef_params = list(), tc_params = list(),
                          diff_params = list(), lm_params = list(),
                          model = "openfair_tef_tc_diff_lm") {
  new_tidyrisk_scenario(tef_params, tc_params, diff_params, lm_params, model)
}

#' @export
#' @importFrom cli cat_line
print.tidyrisk_scenario <- function(x, ...) {
  cli::cat_line("# Scenario model: ", x$model)
  NextMethod(x)

  invisible(x)
}
