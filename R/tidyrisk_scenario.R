# Constructors ------------------------------------------------------------

#' Construct a quantitative scenario object
#'
#' Supply one or more named lists in the format of `foo_params`,
#' where each `foo` is an OpenFAIR factor name (e.g. tef, tc, diff, lm).
#' Each factor should include a function name (`func`) to which the
#' other named elements in the list are passed as parameters when
#' sampling.
#'
#'
#' @param ... One or more named OpenFAIR factor with parameters for sampling
#' @param model Name of model to run
#' @importFrom rlang list2
#' @importFrom purrr every
#' @export
new_tidyrisk_scenario <- function(..., model = "openfair_tef_tc_diff_lm") {
  dots <- rlang::list2(...)
  if (any(names(dots) == "")) {
    stop("One or more parameters is unnamed.", call. = FALSE)
  }
  stopifnot(purrr::every(dots, is.list), is.character(model))
  names(dots) <- gsub( "_params", "", names(dots))
  scenario <- list(
    parameters = dots,
    model = model)
  class(scenario) <- c("tidyrisk_scenario")
  scenario
}

#' @export
#' @importFrom purrr modify
#' @importFrom rlang list2
#' @importFrom vctrs vec_cast
#' @rdname new_tidyrisk_scenario
tidyrisk_scenario <- function(..., model = "openfair_tef_tc_diff_lm") {
  dots <- rlang::list2(...)
  purrr::modify(dots, vctrs::vec_cast, list())
  model <- vctrs::vec_cast(model, character())
  validate_tidyrisk_scenario(new_tidyrisk_scenario(!!!dots, model = model))
}

#' Validates that a scenario object is well formed
#'
#' @param x An object
#' @export
#' @importFrom purrr every pluck
validate_tidyrisk_scenario <- function(x) {
  # iterating a ragged list is currently not working as expected
  #if (!purrr::every(x$parameters, purrr::pluck, "func", .default = FALSE)) {
  #  stop(
  #    "All parameters must have a `func` element, specifing the sampling function",
  #    call. = FALSE
  #  )
  #}

  x

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

#' Set an abbreviation when displaying an S3 column in a tibble
#'
#' @param x An object
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

#' Coerce the parameters of a tidyrisk_scenario to a tibble
#'
#' @param x A tidyrisk_scenario
#' @param ... Currently not used
#' @export
#' @importFrom cli cat_line
#' @importFrom purrr map_depth
#' @importFrom dplyr bind_rows
as_tibble.tidyrisk_scenario <- function(x, ...) {
  cli::cat_line("# Scenario model: ", x$model)
  purrr::map_depth(x$parameters, .depth = 1, dplyr::bind_rows, .id = "id") %>%
    dplyr::bind_rows(.id = "openfair_factor")
}

#' @rdname as_tibble.tidyrisk_scenario
#' @export
as.data.frame.tidyrisk_scenario <- as_tibble.tidyrisk_scenario
