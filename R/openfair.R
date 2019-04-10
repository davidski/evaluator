# OpenFAIR components -----------------------------------------------------

#' Calculate the number of simulated threat event frequencies (TEF)
#'
#' @importFrom rlang exec
#' @importFrom mc2d rpert
#' @importFrom stringi stri_split_fixed
#' @param n Number of samples to generate.
#' @param params Optional parameters to pass to `.func`.
#' @param .func Function to use to simulate TEF, defaults to \code{\link[mc2d]{rpert}}.
#' @return List containing type ("tef"), samples (as a vector), and details (as a list).
#' @family OpenFAIR models
#' @export
sample_tef <- function(n, params = NULL, .func = NULL) {
  .func <- if (is.null(.func)) get("rpert", asNamespace("mc2d")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }

  list(type = "tef",
       samples = as.integer(round(rlang::exec(.func, n = n, !!!params))),
       details = list())
}

#' Sample threat capabilities (TC) from a distribution function
#'
#' @importFrom rlang exec
#' @importFrom mc2d rpert
#' @importFrom stringi stri_split_fixed
#' @inheritParams sample_tef
#' @param .func Function to use to simulate TC, defaults to \code{\link[mc2d]{rpert}}.
#' @return List containing type ("tc"), samples (as a vector), and details (as a list).
#' @family OpenFAIR helpers
#' @export
sample_tc <- function(n, params = NULL, .func = NULL) {
  .func <- if (is.null(.func)) get("rpert", asNamespace("mc2d")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }
  list(type = "tc",
       samples = if (n != 0 && !is.na(n)) rlang::exec(.func, n = n, !!!params) else NA,
       details = list())
}

#' Calculate the difficulty presented by controls, given a function and
#' parameters for that function
#'
#' @importFrom rlang exec
#' @importFrom mc2d rpert
#' @importFrom stringi stri_split_fixed
#' @inheritParams sample_tef
#' @param .func Function to use to simulate DIFF, defaults to \code{\link[mc2d]{rpert}}.
#' @return List containing type ("diff"), samples (as a vector), and details (as a list).
#' @family OpenFAIR helpers
#' @export
sample_diff <- function(n, .func = NULL, params = NULL) {
  .func <- if (is.null(.func)) get("rpert", asNamespace("mc2d")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }

  list(type = "diff",
       samples = rlang::exec(.func, n = n, !!!params),
       details = list())
}

#' Calculate the vulnerability
#'
#' @importFrom purrr is_list
#' @importFrom rlang exec
#' @importFrom stringi stri_split_fixed
#' @inheritParams sample_tef
#' @param .func Function to use to simulate VULN, defaults to \code{\link[stats]{rbinom}}.
#' @return List containing type ("vuln"), samples (as a vector), and details (as a list).
#' @family OpenFAIR helpers
#' @export
sample_vuln <- function(n, .func = NULL, params = NULL) {
  .func <- if (is.null(.func)) get("rbinom", asNamespace("stats")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }

  dat <- rlang::exec(.func, n = n, !!!params)
  list(type = "vuln",
       samples = if (purrr::is_list(dat)) dat$samples else dat,
       details = if (purrr::is_list(dat)) dat$details else list()
  )
}

#' Given a number of loss events and a loss distribution, calculate losses
#'
#' @importFrom rlang exec
#' @importFrom stringi stri_split_fixed
#' @importFrom mc2d rpert
#' @inheritParams sample_tef
#' @param .func Function to use to simulate TEF, defaults to \code{\link[mc2d]{rpert}}.
#' @return List containing type ("lm"), samples (as a vector), and details (as a list).
#' @family OpenFAIR helpers
#' @export
sample_lm <- function(n, .func = NULL, params = NULL) {

  .func <- if (is.null(.func)) get("rpert", asNamespace("mc2d")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }
  samples <- if (!is.na(n) && n != 0) rlang::exec(.func, n = n, !!!params) else 0

  # We have to calculate ALE/SLE differently (ALE: 0, SLE: NA) if there are no losses
  details <- if (length(samples) == 0 || sum(samples) == 0) {
      list(ale = 0, sle_max = 0, sle_min = 0, sle_mean = 0, sle_median = 0)
  } else {
      list(ale = sum(samples),
           sle_max = max(samples),
           sle_min = min(samples[samples > 0]),
           sle_mean = mean(samples[samples > 0]),
           sle_median = stats::median(samples[samples > 0])
           )
  }

  if (length(samples) == 0) samples <- 0 # samples == 0 if there are no losses

  return(list(type = "lm", samples = samples, details = details))
}

#' Sample loss event frequency
#'
#' @importFrom rlang exec
#' @importFrom purrr is_list
#' @inheritParams sample_tef
#' @param .func Function to use to simulate LEF, defaults to \code{\link[stats]{rnorm}}.
#' @return List containing type ("lef"), samples (as a vector), and details (as a list).
#' @family OpenFAIR helpers
#' @export
sample_lef <- function(n, .func = NULL, params = NULL) {
  .func <- if (is.null(.func)) get("rpert", asNamespace("mc2d")) else {
    func_split <- stringi::stri_split_fixed(.func, "::", simplify = TRUE)
    requireNamespace(func_split[1], quietly = TRUE)
    get(func_split[2], asNamespace(func_split[1]))
  }

  dat <- rlang::exec(.func, n = n, !!!params)
  list(type = "lef",
       samples = if (purrr::is_list(dat)) dat$samples else dat,
       details = if (purrr::is_list(dat)) dat$details else list()
  )
}

# Control Strength Functions ----------------------------------------------

#' Calculate difficulty strength across multiple controls by taking the mean
#'
#' Given a set of estimation parameters, calculate control strength as the
#'   arithmetic mean of sampled control effectiveness.
#'
#' @importFrom dplyr %>% bind_rows
#' @importFrom purrr pmap map transpose simplify_all map_dbl
#' @importFrom stringi stri_split_fixed
#' @importFrom rlang .data
#' @param n Number of threat events to generate control effectiveness samples.
#' @param diff_parameters Parameters to pass to \code{\link{sample_diff}}.
#' @return Vector of control effectiveness.
#' @family OpenFAIR helpers
#' @export
get_mean_control_strength <- function(n, diff_parameters)  {
  # get the list of control parameters, including the function to call
  # diff_parameters is a list of lists, so strip one layer to get a simple list
  #control_list <- diff_parameters %>% purrr::flatten()
  control_list <- diff_parameters

  # iterate over the control parameters list
  # getting a number of samples for each control

  cs <- purrr::map(control_list, function(x) {

    # get the names of all non-func parameters, then extract them
    param_names <- setdiff(names(x), "func")
    params <- x[param_names]

    #generate the samples
    sample_diff(.func = x$func, n = n, params = params)
    }) %>% purrr::map("samples")

  # pivot the results so each list has 1 sample for each control
  cs <- purrr::transpose(cs) %>% purrr::simplify_all()

  # take the mean of each list, giving the mean control strength for that event
  outcomes <- cs %>% purrr::map_dbl(mean)

  outcomes
}

# Composition Functions ---------------------------------------------------

#' Calculate number of loss events which occur in a simulated period
#'
#' Composition function for use in \code{\link{sample_lef}}. Given a count of
#'   the number of threat events (TEF) and the level of vulnerability (as a
#'   percentage), calculate how many of those become loss events (LEF).
#'
#' @param tef Threat event frequency (n).
#' @param vuln Vulnerability (percentage).
#' @param n Number of samples to generate.
#' @return List containing samples (as a vector) and details (as a list).
#' @export
#' @family OpenFAIR helpers
#' @examples
#' compare_tef_vuln(tef = 500, vuln = .25)
compare_tef_vuln <- function(tef, vuln, n = NULL) {
  samples <- tef * vuln
  samples <- if (!is.null(n) && n <= length(samples)) samples[1:n] else samples
  list(samples = samples,
       details = list())
}

#' Determine which threat events result in loss opportunities
#'
#' Composition function for use in \code{\link{sample_vuln}}, does a simple
#'   compare of all threat events where the threat capability (TC) is greater
#'   than the difficulty (DIFF).
#'
#' @param tc Threat capability (as a percentage).
#' @param diff Difficulty (as a percentage).
#' @param n Number of samples to generate.
#' @param ... Optional parameters (currently ignored).
#' @return List containing boolean values of length TC (as a vector) and details (as a list).
#' @export
#' @family OpenFAIR helpers
#' @examples
#' threat_capabilities <- c(.1, .5, .9)
#' difficulties <- c(.09, .6, .8)
#' select_loss_opportunities(threat_capabilities, difficulties)
select_loss_opportunities <- function(tc, diff, n = NULL, ...) {
  samples <- tc > diff
  samples <- if (!is.null(n) && n <= length(samples)) samples[1:n] else samples

  # mean amount threat strength exceeds control strength, if that ever occurs
  tc_exceedance <- if (
    all(is.na(samples))) {NA} else # if there are no threat events, then NA
      if (sum(samples, na.rm = TRUE) > 0) {
        mean(tc[samples] - diff[samples], na.rm = TRUE)
      } else {0}
  # mean amount control strength exceeds threat strength, if that ever occurs
  diff_exceedance <- if (
    all(is.na(samples))) {NA} else # if there are no threat events, then NA
      if (
        #sum(samples, na.rm = TRUE) > 0
          sum(samples, na.rm = TRUE) != length(tc)) {
        mean(diff[!samples] - tc[!samples], na.rm = TRUE)
      } else {0}

  details <- list(mean_tc_exceedance = tc_exceedance,
                  mean_diff_exceedance = diff_exceedance)

  # samples == 0 if there are no loss events
  if (all(is.na(samples))) samples <- rep(FALSE, length(samples))

  list(samples = samples, details = details)
}

# Top Level Analysis ------------------------------------------------------

#' Run an OpenFAIR simulation at the TEF/TC/DIFF/LM levels
#'
#' Run an OpenFAIR model with parameters provided for TEF, TC, DIFF, and
#'   LM sampling. If there are multiple controls provided for the scenario, the
#'   arithmetic mean (average) is taken across samples for all controls to get
#'   the effective control strength for each threat event.
#'
#' @importFrom purrr pmap map pluck simplify_all transpose map_dbl map_int flatten
#' @importFrom tidyr nest
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @param tef Parameters for TEF simulation
#' @param tc Parameters for TC simulation
#' @param diff Parameters for DIFF simulation
#' @param lm Parameters for LM simulation
#' @param n Number of iterations to run.
#' @param verbose Whether to print progress indicators.
#' @return Dataframe of scenario name, threat_event count, loss_event count,
#'   mean TC and DIFF exceedance, and ALE samples.
#' @family OpenFAIR helpers
#' @export
#' @examples
#' data(mc_quantitative_scenarios)
#' params <- mc_quantitative_scenarios[[1, "scenario"]]$parameters
#' openfair_tef_tc_diff_lm(params$tef, params$tc, params$diff, params$lm, 10)
openfair_tef_tc_diff_lm <- function(tef, tc, diff, lm, n = 10^4, verbose = FALSE) {

  # make samples repeatable (and l33t)
  set.seed(31337)

  if (verbose) {
    message("Working on scenario ")

    message(paste("Scenario is: ",
                  "             ", tef,
                  "             ", tc,
                  "             ", diff,
                  "             ", lm,
                  "\n"))
  }

  # TEF - how many contacts do we have in each simulated period
  TEFestimate <- tef %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  params <- TEFestimate$params %>% unlist()
  TEFsamples <- sample_tef(n = n, .func = TEFestimate$func, params = params)
  TEFsamples <- TEFsamples$samples

  # TC - what is the strength of each threat event
  #    - get the threat capability parameters for this scenario
  TCestimate <- tc %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  #    - sample threat capability for each TEF event in each sample period
  TCsamples <- purrr::map(1:n, function(x) {
    params <- TCestimate$params %>% unlist()
    sample_tc(n = TEFsamples[x], .func = TCestimate$func, params = params)
  })
  # TCSamples is now a list of of the TC for each threat event

  # DIFF - calculate the mean strength of controls for each threat event
  #        in a given period

  # get the difficulty for each threat event across all the simulated periods
  DIFFsamples <- purrr::map(1:n, function(x) {
    if (is.numeric(TEFsamples[[x]]) && TEFsamples[[x]] > 0) {
      get_mean_control_strength(TEFsamples[[x]], diff)
    } else {NA}
  })
  # DIFFsamples is now a list of vectors of the control strength for
  #   each individual threat event in the simulated period

  # LEF - determine how many threat events become losses (TC > DIFF)
  LEFsamples <- purrr::map(1:n, function(x) {
    sample_lef(n = length(TCsamples[[x]]$samples),
               .func = "evaluator::select_loss_opportunities",
               params = list(tc = TCsamples[[x]]$samples,
                             diff = DIFFsamples[[x]]))
  })

  mean_tc_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_tc_exceedance"))
  mean_diff_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_diff_exceedance"))
  LEFsamples <- purrr::map(LEFsamples, c("samples")) %>% purrr::map_int(sum)

  # LM - determine the size of losses for each iteration
  LMestimate <- lm %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  loss_samples <- purrr::map(LEFsamples, function(x) {
    params <- LMestimate$params %>% unlist()
    dat <- sample_lm(n = x, .func = LMestimate$func, params = params)
    dat$samples <- sum(dat$samples)
    dat
  })

  # summary stats for ALE
  if (verbose) {
    print(summary(purrr::map_dbl(loss_samples, "samples")))
    value_at_risk <- stats::quantile(purrr::map_dbl(loss_samples, "samples"),
                                     probs = (0.95), na.rm = TRUE)
    message(paste0("Losses at 95th percentile are $",
                   format(value_at_risk, nsmall = 2, digits = 2,
                          big.mark = ",")))
  }

  tibble::tibble(iteration = seq(1:n),
                 threat_events = TEFsamples,
                 loss_events = LEFsamples,
                 vuln = LEFsamples/TEFsamples,
                 mean_tc_exceedance = mean_tc_exceedance,
                 mean_diff_exceedance = mean_diff_exceedance,
                 ale = purrr::map_dbl(loss_samples, "samples"),
                 sle_mean = purrr::map_dbl(loss_samples, c("details", "sle_mean")),
                 sle_median = purrr::map_dbl(loss_samples, c("details", "sle_median")),
                 sle_max = purrr::map_dbl(loss_samples, c("details", "sle_max")),
                 sle_min = purrr::map_dbl(loss_samples, c("details", "sle_min"))
  )
}

#' Run an OpenFAIR simulation at the TEF/TC/DIFF/PLM/SR levels
#'
#' Run an OpenFAIR model with parameters provided for TEF, TC, DIFF, PLM, and
#'   SR sampling. If there are multiple controls provided for the scenario, the
#'   arithmetic mean (average) is taken across samples for all controls to get
#'   the effective control strength for each threat event.
#'
#' @importFrom purrr pmap map pluck simplify_all transpose map_dbl map_int flatten
#' @importFrom tidyr nest
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @param tef Parameters for TEF simulation
#' @param tc Parameters for TC simulation
#' @param diff Parameters for DIFF simulation
#' @param plm Parameters for PLM simulation
#' @param sr Parameters for SR simulation
#' @param n Number of iterations to run.
#' @param verbose Whether to print progress indicators.
#' @return Dataframe of scenario name, threat_event count, loss_event count,
#'   mean TC and DIFF exceedance, and ALE samples.
#' @family OpenFAIR models
#' @export
openfair_tef_tc_diff_plm_sr <- function(tef, tc, diff, plm, sr, n = 10^4, verbose = FALSE) {

  # make samples repeatable (and l33t)
  set.seed(31337)

  if (verbose) {
    message("Working on scenario ")

    message(paste("Scenario is: ",
                  "             ", tef,
                  "             ", tc,
                  "             ", diff,
                  "             ", plm,
                  "             ", sr,
                  "\n"))
  }

  # TEF - how many contacts do we have in each simulated period
  TEFestimate <- tef %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  params <- TEFestimate$params %>% unlist()
  TEFsamples <- sample_tef(n = n, .func = TEFestimate$func, params = params)
  TEFsamples <- TEFsamples$samples

  # TC - what is the strength of each threat event
  #    - get the threat capability parameters for this scenario
  TCestimate <- tc %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  #    - sample threat capability for each TEF event in each sample period
  TCsamples <- purrr::map(1:n, function(x) {
    params <- TCestimate$params %>% unlist()
    sample_tc(n = TEFsamples[x], .func = TCestimate$func, params = params)
  })
  # TCSamples is now a list of of the TC for each threat event

  # get the difficulty for each threat event across all the simulated periods
  DIFFsamples <- purrr::map(1:n, function(x) {
    if (is.numeric(TEFsamples[[x]]) && TEFsamples[[x]] > 0) {
      get_mean_control_strength(TEFsamples[[x]], diff)
    } else {NA}
  })
  # DIFFsamples is now a list of vectors of the control strength for
  #   each individual threat event in the simulated period

  # LEF - determine how many threat events become losses (TC > DIFF)
  LEFsamples <- purrr::map(1:n, function(x) {
    sample_lef(n = length(TCsamples[[x]]$samples),
               .func = "evaluator::select_loss_opportunities",
               params = list(tc = TCsamples[[x]]$samples,
                             diff = DIFFsamples[[x]]))
  })

  mean_tc_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_tc_exceedance"))
  mean_diff_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_diff_exceedance"))
  LEFsamples <- purrr::map(LEFsamples, c("samples")) %>% purrr::map_int(sum)

  # LM - determine the size of losses for each iteration
  PLMestimate <- plm %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  SRestimate <- sr %>% purrr::flatten() %>%
    tibble::as_tibble() %>% tidyr::nest(-.data$func, .key = "params")
  loss_samples <- purrr::map(LEFsamples, function(x) {
    params <- PLMestimate$params %>% unlist()
    dat_p <- sample_lm(n = x, .func = PLMestimate$func, params = params)
    params <- SRestimate$params %>% unlist()
    dat_s <- sample_lm(n = x, .func = SRestimate$func, params = params)
    samples <- sum(dat_p$samples + dat_s$samples)
    # We have to calculate ALE/SLE differently (ALE: 0, SLE: NA) if there are no losses
    details <- if (length(samples) == 0 | sum(samples) == 0) {
      list(ale = 0, sle_max = 0, sle_min = 0, sle_mean = 0, sle_median = 0)
    } else {
      list(ale = sum(samples),
           sle_max = max(samples),
           sle_min = min(samples[samples > 0]),
           sle_mean = mean(samples[samples > 0]),
           sle_median = stats::median(samples[samples > 0])
      )
    }
    dat <- list(type = "lm",
                samples = sum(samples),
                details = details)
    dat
  })

  # summary stats for ALE
  if (verbose) {
    print(summary(purrr::map_dbl(loss_samples, "samples")))
    value_at_risk <- stats::quantile(purrr::map_dbl(loss_samples, "samples"),
                                     probs = (0.95), na.rm = TRUE)
    message(paste0("Losses at 95th percentile are $",
                   format(value_at_risk, nsmall = 2, digits = 2,
                          big.mark = ",")))
  }

  tibble::tibble(iteration = seq(1:n),
                 threat_events = TEFsamples,
                 loss_events = LEFsamples,
                 vuln = LEFsamples/TEFsamples,
                 mean_tc_exceedance = mean_tc_exceedance,
                 mean_diff_exceedance = mean_diff_exceedance,
                 ale = purrr::map_dbl(loss_samples, "samples"),
                 sle_mean = purrr::map_dbl(loss_samples, c("details", "sle_mean")),
                 sle_median = purrr::map_dbl(loss_samples, c("details", "sle_median")),
                 sle_min = purrr::map_dbl(loss_samples, c("details", "sle_min")),
                 sle_max = purrr::map_dbl(loss_samples, c("details", "sle_max"))
  )
}
