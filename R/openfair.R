# OpenFAIR components -----------------------------------------------------

#' Calculate the number of simulated threat event frequencies (TEF)
#'
#' @importFrom purrr invoke
#' @importFrom mc2d rpert
#' @param func Function to use to simulate TEF, defaults to mc2d::rpert.
#' @param params Optional parameters to pass to `func`.
#' @return List containing type ("tef"), samples (as a vector), and details (as a list).
#' @export
sample_tef <- function(func = NULL, params = NULL) {
  if (is.null(func)) func <- get("rpert", asNamespace("mc2d"))
  list(type = "tef",
       samples = invoke(func, params),
       details = list())
}

#' Sample threat capabilities (TC) from a distribution function
#'
#' @importFrom purrr invoke
#' @importFrom mc2d rpert
#' @param func Function to use to simulate TC, defaults to mc2d::rpert.
#' @param params Optional parameters to pass to `func`.
#' @return List containing type ("tc"), samples (as a vector), and details (as a list).
#' @export
sample_tc <- function(func = NULL, params = NULL) {
  if (is.null(func)) func <- get("rpert", asNamespace("mc2d"))
  list(type = "tc",
       samples = invoke(func, params),
       details = list())
}

#' Calculate the difficulty presented by controls, given a function and
#' parameters for that function
#'
#' @importFrom purrr invoke
#' @importFrom mc2d rpert
#' @param func Function to use to simulate DIFF, defaults to mc2d::rpert.
#' @param params Optional parameters to pass to `func`.
#' @return List containing type ("diff"), samples (as a vector), and details (as a list).
#' @export
sample_diff <- function(func = NULL, params = NULL) {
  if (is.null(func)) func <- get("rpert", asNamespace("mc2d"))
  list(type = "diff",
       samples = invoke(func, params),
       details = list())
}

#' Calculate the vulnerability
#'
#' @importFrom purrr invoke is_list
#' @param func Function to use to simulate DIFF, defaults to `rbinom`.
#' @param params Optional parameters to pass to `func`.
#' @return List containing type ("vuln"), samples (as a vector), and details (as a list).
#' @export
sample_vuln <- function(func = NULL, params = NULL) {
  if (is.null(func)) func <- get("rbinom", asNamespace("stats"))
  dat <- invoke(func, params)
  list(type = "vuln",
       samples = if (purrr::is_list(dat)) dat$samples else dat,
       details = if (purrr::is_list(dat)) dat$details else list()
  )
}

#' Given a number of loss events and a loss distribution, calculate losses
#'
#' @importFrom purrr invoke
#' @importFrom mc2d rpert
#' @param func Function to use to simulate TEF, defaults to mc2d::rpert.
#' @param params Optional parameters to pass to `func`.
#' @return List containing type ("lm"), samples (as a vector), and details (as a list).
#' @export
sample_lm <- function(func = NULL, params = NULL) {

  if (is.null(func)) func <- get("rpert", asNamespace("mc2d"))
  samples <- invoke(func, params)

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

  if (length(samples) == 0) samples <- 0 # samples == 0 if there are no losses

  return(list(type = "lm", samples = samples, details = details))
}

#' Sample loss event frequency
#'
#' @importFrom purrr invoke is_list
#' @param func Function to use to simulate LEF, defaults to `rnorm``
#' @param params Optional parameters to pass to `func`
#' @return List containing type ("diff"), samples (as a vector), and details (as a list).
#' @export
sample_lef <- function(func = NULL, params = NULL) {
  if (is.null(func)) func <- get("rnorm", asNamespace("stats"))
  dat <- invoke(func, params)
  list(type = "lef",
       samples = if (purrr::is_list(dat)) dat$samples else dat,
       details = if (purrr::is_list(dat)) dat$details else list()
  )
}

# Control Strength Functions ----------------------------------------------

#' Calculate difficulty strength across mutliple controls by taking the mean
#'
#' Given a set of estimation parameters, calculate control strength as the
#' arithmetic mean of sampled control effectiveness.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr pmap map transpose simplify_all map_dbl
#' @param n Number of threat events to sample controls across.
#' @param diff_estimates Parameters to pass to `sample_diff`.
#' @return Vector of control effectiveness.
#' @export
get_mean_control_strength <- function(n, diff_estimates)  {
  # ensure control estimates are in the order we expect
  control_params <- with(diff_estimates, list(l = l, ml = ml, h = h,
                                              conf = conf))
  cs <- purrr::pmap(control_params, ~ sample_diff(
    params = list(n, ..1, ..2, ..3, ..4))) %>%
    purrr::map("samples")
  cs <- cs %>% purrr::transpose(.) %>%  purrr::simplify_all(.)
  # take the mean of all controls
  outcomes <- cs %>% purrr::map_dbl(mean)
  # placeholder for control importance work
  # cs_df <- purrr::map_dfc(cs, tibble::as_tibble) %>% t %>% tibble::as_tibble
  # control_importance <- caret::filterVarImp(cs_df, outcomes, nonpara = TRUE) %>%
  #   tibble::rownames_to_column(var = "control") %>%
  #   arrange(desc(Overall)) %>% dplyr::pull("control")
  outcomes
}

# Composition Functions ---------------------------------------------------

#' Calculate number of loss events which occur in a simulated period
#'
#' Composition function for use in sample_lef. Given a count of the
#' number of threat events (tef) and the level of vulnerability (as a
#' percentage), calculate how many of those become loss events (lef).
#'
#' @param tef Threat event frequency (n).
#' @param vuln Vulnerability (percentage).
#' @return List containing samples (as a vector) and details (as a list).
#' @export
compare_tef_vuln <- function(tef, vuln) {
  samples = tef * vuln
  list(samples = samples,
       details = list())
}

#' Determine which threat events result in loss opportunities
#'
#' Composition function for use in sample_vuln, does a simple compare of
#' all threat events where the threat capability (TC) is greater than the
#' difficulty (DIFF).
#'
#' @param tc Threat capability (as a percentage).
#' @param diff Difficulty (as a percentage).
#' @return List containing boolean values of length TC (as a vector) and details (as a list).
#' @export
select_loss_opportunities <- function(tc, diff) {
  samples <-  tc > diff

  # mean amount threat strength exceeds control strength, if that ever occurs
  tc_exceedance <- if (sum(samples) > 0) {
    mean(tc[samples] - diff[samples], na.rm = TRUE)
    } else {0}
  # mean amount control strength exceeds threat strength, if that ever occurs
  diff_exceedance <- if (sum(samples) != length(tc)) {
    mean(diff[!samples] - tc[!samples], na.rm = TRUE)
    } else {0}
  list(samples = samples, details = list(mean_tc_exceedance = tc_exceedance,
                                         mean_diff_exceedance = diff_exceedance))
}

# Top Level Analysis ------------------------------------------------------

#' Run an OpenFAIR simulation at the TEF/TC/DIFF/LM levels with control
#' strength taken as a mean across multiple controls.
#'
#' Run an OpenFAIR model with parameters provided for TEF, TC, DIFF, and
#' LM sampling. If there are multiple controls provided for a scenarios, the
#' arithmetic mean (average) is taken across samples for all controls to get
#' the effective conntrol strength for a given simulation.
#'
#' @importFrom purrr pmap map pluck simplify_all transpose map_dbl map_int
#' @importFrom tibble tibble
#' @importFrom dplyr %>%
#' @param scenario List of tef_, tc_, and LM_ l/ml/h/conf parameters.
#' @param diff_estimates Parameters for estimating the scenario difficulty.
#' @param n Number of simulations to run. Defaults to 10,000.
#' @param title Optional name of scenario. Defaults to 10,000.
#' @param verbose Whether to print progress indicators.
#' @return Dataframe of scenario name, threat_event count, loss_event count,
#'   mean TC and DIFF exceedance, and ALE samples.
#' @export
openfair_tef_tc_diff_lm <- function(scenario, diff_estimates, n = 10^4,
                          title = "Untitled", verbose = FALSE) {

    # make samples repeatable (and l33t)
    set.seed(31337)

    if (verbose) {
        message("Working on scenario ", title)
        message(paste("Scenario is: ", scenario[-which(names(scenario) %in% "diff_samples")], "\n"))
        # message(paste('Names are ', names(scenario)))
    }

    # TEF - how many contacts do we have in each simulated period
    TEFestimate <- with(scenario, tibble::tibble(l = tef_l, ml = tef_ml,
                                                 h = tef_h, conf = tef_conf))
    TEFsamples <- sample_tef(params = list(n, TEFestimate$l, TEFestimate$ml,
                                           TEFestimate$h,
                                           shape = TEFestimate$conf))
    TEFsamples <- round(TEFsamples$samples)

    # TC - what is the strength of each threat event
    #    - get the threat capability parameters for this scenario
    TCestimate <- with(scenario, tibble::tibble(l = tc_l, ml = tc_ml,
                                                h = tc_h, conf = tc_conf))
    #    - sample threat capability for each TEF event in each sample period
    TCsamples <- purrr::map(1:n, ~ sample_tc(params = list(TEFsamples[.x], TCestimate$l,
                                                   TCestimate$ml,
                                                   TCestimate$h,
                                                   shape = TCestimate$conf))$samples)
    # TCSamples is now a list of of the TC for each threat event

    # DIFF - calculate the mean strength of controls for each threat event
    #        in a given period



    # get the difficulty for each threat event across all the simulated periods
    DIFFsamples <- purrr::map(1:n, function(x) {
      if (is.numeric(TEFsamples[[x]]) && TEFsamples[[x]] > 0) {
        get_mean_control_strength(TEFsamples[[x]], diff_estimates)
        } else {NA}
      })
    # DIFFsamples is now a list of vectors of the control strength for
    #   each individual threat event in the simulated period

    # LEF - determine how many threat events become losses (TC > DIFF)
    LEFsamples <- purrr::map(1:n, function(x) {
      sample_lef(func = select_loss_opportunities,
                 params = list(tc = TCsamples[[x]],
                               diff = DIFFsamples[[x]]))
    })

    mean_tc_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_tc_exceedance"))
    mean_diff_exceedance <- purrr::map_dbl(LEFsamples, c("details", "mean_diff_exceedance"))
    LEFsamples <- purrr::map(LEFsamples, c("samples")) %>% purrr::map_int(sum)

    # LM - determine the size of losses for each simulation
    LMestimate <- with(scenario, tibble::tibble(l = lm_l, ml = lm_ml,
                                                h = lm_h, conf = lm_conf))
    loss_samples <- purrr::map(LEFsamples, function(x) {
      dat <- sample_lm(params = list(x, LMestimate$l, LMestimate$ml,
                                    LMestimate$h, LMestimate$conf))
      dat$samples <- sum(dat$samples)
      dat
      })

    # summary stats for ALE
    if (verbose) {
        print(summary(purrr::map_dbl(loss_samples, "samples")))
        value_at_risk <- quantile(purrr::map_dbl(loss_samples, "samples"),
                                  probs = (0.95), na.rm = TRUE)
        message(paste0("Losses at 95th percentile are $",
                       format(value_at_risk, nsmall = 2, digits = 2,
                              big.mark = ",")))
    }

    tibble::tibble(title = rep(as.character(title), n),
                   simulation = seq(1:n),
                   threat_events = TEFsamples,
                   loss_events = LEFsamples,
                   vuln = LEFsamples/TEFsamples,
                   mean_tc_exceedance = mean_tc_exceedance,
                   mean_diff_exceedance = mean_diff_exceedance,
                   ale = purrr::map_dbl(loss_samples, "samples"),
                   sle_max = purrr::map_dbl(loss_samples, c("details", "sle_max")),
                   sle_min = purrr::map_dbl(loss_samples, c("details", "sle_min")),
                   sle_mean = purrr::map_dbl(loss_samples, c("details", "sle_mean")),
                   sle_median = purrr::map_dbl(loss_samples, c("details", "sle_median"))
    )
}
