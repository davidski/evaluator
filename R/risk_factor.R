new_risk_factor <- function(samples = vector(), factor_label = character(), details = list()) {
  stopifnot(is.vector(samples), is.character(factor_label), is.list(details))
  structure(list(samples = samples, factor_label = factor_label,
                 details = details),
            class = c("risk_factor", "list"))
}

risk_factor <- function(samples, factor_label, details = list()) {
  new_risk_factor(samples, factor_label, details)
}

print.risk_factor <- function(x, ...) {
  cli::cat_line("# Factor samples: ", length(x$samples))
  cli::cat_line("# Factor label: ", x$factor_label)
  if (length(x$details) == 0) {
    cli::cat_bullet("# Summary details: None.")
  } else {
    purrr::walk2(names(x$details), x$details,
                 ~ cli::cat_bullet("# Summary detail: ", crayon::bold(.x), " ", .y))
    cli::cat_rule()
  }
  cli::cat_print(x$samples)

  invisible(x)
}

summary.risk_factor <- function(x, ...) {
  samples <- x$samples
  switch(x$factor_label,
         LM = {
           if (length(samples) == 0 | sum(samples) == 0) {
             list(ale = 0, sle_max = 0, sle_min = 0, sle_mean = 0, sle_median = 0)
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

risk_factory <- function(factor_label = "TC"){

  function(.n = 1, ..., .func) {
    my_func <- rlang::as_function(.func)
    #dots <- enquos(...)

    samples <- my_func(n = .n, ...)
    risk_factor(samples = samples, factor_label = factor_label)
  }
}
