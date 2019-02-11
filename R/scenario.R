new_risk_scenario <- function(tef_params = list(),
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
                   lm_params = lm_params)
  attr(scenario, "model") <- model
  class(scenario) <- c("risk_scenario") #, class(scenario))
  scenario
}

risk_scenario <- function(tef_params = list(), tc_params = list(),
                          diff_params = list(), lm_params = list(),
                          model = "openfair_tef_tc_diff_lm") {
  new_risk_scenario(tef_params, tc_params, diff_params, lm_params, model)
}

print.risk_scenario <- function(x, ...) {
  cli::cat_line("# Scenario model: ", attr(x, "model"))
  NextMethod(x)

  invisible(x)
}

#sample
(risk_scenario(
  diff_params = list(
    "2"  = list(min = 70L, mode = 85, max = 98L, shape = 4L, func = "mc2d::rpert"),
    "5"  = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
    "7"  = list(min = 20L, mode = 30, max = 50L, shape = 4L, func = "mc2d::rpert"),
    "32" = list(min = 20L, mode = 30, max = 50L, shape = 4L, func = "mc2d::rpert"),
    "14" = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
    "15" = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
    "16" = list(min = 0L, mode = 10, max = 30L, shape = 4L, func = "mc2d::rpert")
  ),
  tef_params = list(list(min = 10L, mode = 24, max = 52L, shape = 4L, func = "mc2d::rpert")),
  tc_params = list(list(min = 33L, mode = 50, max = 60L, shape = 3L, func = "mc2d::rpert")),
  lm_params = list(list(min = 10000L, mode = 20000, max = 500000L, shape = 4L,
                        func = "mc2d::rpert"))
) -> scenario)
