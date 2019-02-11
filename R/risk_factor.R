#' Construct a risk element object
#'
#' @param samples samples
#' @param factor_label fl
#' @param details details
#' @name evaluator_elem
NULL

#' @export
#' @rdname evaluator_elem
new_evaluator_elem <- function(samples = vector(), factor_label = character(),
                               details = list()) {
  stopifnot(is.vector(samples), is.character(factor_label), is.list(details))
  if (!factor_label %in% c("TF", "TEF", "TC", "DIFF", "LM", "PLM", "SLF", "SLM")) {
    stop("Factor label is not a supported OpenFAIR type.", call. = FALSE)
  }
  structure(list(samples = samples, factor_label = factor_label,
                 details = details),
            class = c("evaluator_elem", "list"))
}

#' @export
#' @rdname evaluator_elem
evaluator_elem <- function(samples, factor_label, details = list()) {
  new_evaluator_elem(samples, factor_label, details)
}

#' @export
#' @importFrom cli cat_line cat_bullet cat_rule cat_print
#' @importFrom crayon bold
#' @importFrom purrr walk2
print.evaluator_elem <- function(x, ...) {
  cli::cat_line("# Factor samples: ", length(x$samples))
  cli::cat_line("# Factor label: ", x$factor_label)
  if (length(x$details) == 0) {
    cli::cat_bullet("# Summary details: None.")
  } else {
    purrr::walk2(names(x$details), x$details,
                 ~ cli::cat_bullet("# Summary detail: ", crayon::bold(.x),
                                   " ", .y))
    cli::cat_rule()
  }
  cli::cat_print(x$samples)

  invisible(x)
}

#' @export
summary.evaluator_elem <- function(object, ...) {
  samples <- object$samples
  switch(object$factor_label,
         LM = {
           if (length(samples) == 0 | sum(samples) == 0) {
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

#' Create a risk element sample function
#'
#' @export
#' @param factor_label abbreviation of the OpenFAIR element
risk_factory <- function(factor_label = "TC"){

  function(.n = 1, ..., .func) {
    my_func <- rlang::as_function(.func)
    #dots <- enquos(...)

    samples <- my_func(n = .n, ...)
    evaluator_elem(samples = samples, factor_label = factor_label)
  }
}
