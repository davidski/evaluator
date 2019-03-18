# Constructors ------------------------------------------------------------

#' Construct a quantitative scenario object
#'
#' Supply one or more named lists in the format of `foo_params`,
#' where each `foo` is an OpenFAIR element name (e.g. tef, tc, diff, lm).
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
  scenario <- list(
    parameters = list(
      tef = tef_params,
      tc = tc_params,
      diff = diff_params,
      lm = lm_params),
    model = model)
  class(scenario) <- c("tidyrisk_scenario")
  scenario
}

#' @export
#' @importFrom vctrs vec_cast
#' @rdname new_tidyrisk_scenario
tidyrisk_scenario <- function(tef_params = list(), tc_params = list(),
                          diff_params = list(), lm_params = list(),
                          model = "openfair_tef_tc_diff_lm") {
  tef_params <- vctrs::vec_cast(tef_params, list())
  tc_params <- vctrs::vec_cast(tc_params, list())
  diff_params <- vctrs::vec_cast(diff_params, list())
  lm_params <- vctrs::vec_cast(lm_params, list())
  model <- vctrs::vec_cast(model, character())
  new_tidyrisk_scenario(tef_params, tc_params, diff_params, lm_params, model)
}

#' Test if the object is a tidyrisk_scenario
#'
#' This function returns `TRUE` for tidyrisk_scenario (or subclasses)
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `tidyrisk_scenario` class.
#' @export
is_tidyrisk_scenario <- function(x) {
  inherits(x, "tidyrisk_scenario")
}

#' @export
vec_ptype_abbr.tidyrisk_scenario <- function(x) {
  "r_scen"
}

# Formatters --------------------------------------------------------------


#' Default printing of a tidyrisk_scenario
#'
#' Basic printing of a tidyrisk scenario
#'
#' @param x A tidyrisk_scenario
#' @param ... Currently not used
#' @export
#' @importFrom cli cat_line
print.tidyrisk_scenario <- function(x, ...) {
  cli::cat_line("# Scenario model: ", x$model)
  cli::cat_line("# Defined parameters: ", paste0(names(x$parameters), collapse = ", "))
  #NextMethod(x)

  invisible(x)
}

