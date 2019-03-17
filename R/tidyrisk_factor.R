#' Construct a tidyrisk_factor object
#'
#' @param samples samples
#' @param factor_label fl
#' @param details details
#' @name tidyrisk_factor
NULL

#' @rdname tidyrisk_factor
#' @importFrom vctrs vec_assert new_vctr
#' @export
new_tidyrisk_factor <- function(samples = double(), factor_label = character(),
                               details = list()) {
  vctrs::vec_assert(samples, double())
  vctrs::vec_assert(factor_label, character())
  if (!factor_label %in% c("TF", "TEF", "TC", "DIFF", "LM", "PLM", "SLF", "SLM")) {
    stop("Factor label is not a supported OpenFAIR type.", call. = FALSE)
  }
  vctrs::vec_assert(details, list())
  vctrs::new_vctr(samples, factor_label = factor_label, details = details,
                  class = "tidyrisk_factor")
}

#' @rdname tidyrisk_factor
#' @export
tidyrisk_factor <- function(samples, factor_label, details = list()) {
  samples <- vctrs::vec_cast(samples, double())
  factor_label <- vctrs::vec_cast(factor_label, character())
  details <- vctrs::vec_cast(details, list())
  new_tidyrisk_factor(samples, factor_label, details)
}

factor_label <- function(x) attr(x, "factor_label")
details <- function(x) attr(x, "details")

#' @importFrom cli cat_line cat_bullet cat_rule cat_print
#' @importFrom vctrs vec_data
#' @importFrom crayon bold
#' @importFrom purrr walk2
#' @export
print.tidyrisk_factor <- function(x, ...) {
  cli::cat_line("# Factor samples: ", length(vctrs::vec_data(x)))
  cli::cat_line("# Factor label: ", factor_label(x))
  if (length(details(x)) == 0) {
    cli::cat_bullet("# Summary details: None.")
  } else {
    purrr::walk2(names(details(x)), details(x)
                 ~ cli::cat_bullet("# Summary detail: ", crayon::bold(.x),
                                   " ", .y))
    cli::cat_rule()
  }
  cli::cat_print(vctrs::vec_data(x))

  invisible(x)
}

#' @export
#' @importFrom vctrs vec_data
summary.tidyrisk_factor <- function(object, ...) {
  samples <- vctrs::vec_data(object)
  switch(factor_label(object),
         LM = {
           if (length(samples) == 0 || sum(samples, na.rm = TRUE) == 0) {
             list(ale = 0, sle_max = 0, sle_min = 0, sle_mean = 0,
                  sle_median = 0)
           } else {
             list(ale = sum(samples),
                  sle_max = max(samples),
                  sle_min = min(samples[samples > 0]),
                  sle_mean = mean(samples[samples > 0]),
                  sle_median = stats::median(samples[samples > 0])
             )
           }},
         list()
  )
}

#' Create a tidyrisk_factor sample function
#'
#' @param factor_label abbreviation of the OpenFAIR element
#' @importFrom rlang as_function
#' @export
risk_factory <- function(factor_label = "TC"){

  function(.n = 1, ..., .func) {
    my_func <- rlang::as_function(.func)
    #dots <- enquos(...)

    samples <- my_func(n = .n, ...)
    tidyrisk_factor(samples = samples, factor_label = factor_label)
  }
}
