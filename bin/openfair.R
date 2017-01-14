# mc2d required for *pert functons
if ("pacman" %in% installed.packages()) {
  pacman::p_load(mc2d, dplyr)
} else {
  library(mc2d)
  library(dplyr)
}

sample_lm <- function(n, l, ml, h, conf) {
  # given a number of loss events and a loss distribution, calculate losses
  #
  # ARGS:
  #   n - number of threat events to evaluate
  #   l - low boundary
  #   ml - most likely
  #   h - high boundary
  #   conf - confidence
  #
  # RETURNS:
  #   list of total loss, min/max/mean of sle

  if (is.na(n)) { n <- 0 }
  samples <- rpert(n, l, ml, h, shape = conf)
  
  # We have to calculate ALE/SLE differently (ALE: 0, SLE: NA) if there are 
  # no losses
  results <- if (length(samples)==0) {
    list(ale = 0,
         sle_max = 0,
         sle_min = 0,
         sle_mean = 0,
         sle_median = 0)
  } else {
    list(ale = sum(samples),
         sle_max = max(samples),
         sle_min = min(samples[samples > 0]),
         sle_mean = mean(samples[samples > 0]),
         sle_median = median(samples[samples > 0]))
  }
  
  return(results)
}

select_events <- function(n, TCestimate, DIFFsamples = NULL, 
                          DIFFestimate = NULL, verbose = FALSE){
  # Calculate the threat capability and whether the control resists the attack
  #
  # ARGS:
  #   n - number of threat events to evaluate
  #   TCestimate - threat capability estimate - df(l, m, h, conf)
  #   DIFFestimate - control difficulty estimate - df(l, m, h, conf)
  #   DIFFsamples - pre-sampled control difficulty estimates
  #
  # RETURNS:
  #   List of - Number of succesfull attacks (i.e. loss events)
  #           - mean tc exceedance (how much TC > DIFF)

  # if there are no threat events to select from, then return NAs
  
  paste("Sampling:", n, TCestimate, DIFFestimate)
  
  if (n == 0) {
    return(list(loss_events = 0,
                mean_tc_exceedance = 0))
  }
  
  # sample threat capability
  TCsamples <- rpert(n, TCestimate$l, TCestimate$ml, TCestimate$h,
                     shape = TCestimate$conf)
  
  # sample difficulty sample, either from a precomputed range or from a list of
  # probability distributions
  if (is.null(DIFFsamples)) {
    DIFFsamples <-
      mapply(function(l, ml, h, conf) {
        rpert(n, l, ml, h, conf)
      },
      DIFFestimate$l,
      DIFFestimate$ml,
      DIFFestimate$h,
      DIFFestimate$conf)
    DIFFsamples <- if (is.array(DIFFsamples)) {
      rowMeans(DIFFsamples)
    } else {
      #browser()
      mean(DIFFsamples)
    }
  } else {
    DIFFsamples <- DIFFsamples[1:n]
  }
  
  # loss events occur whenever TC > DIFF
  VULNsamples <- TCsamples > DIFFsamples

  # TC exceedance is TC - DIFF - how much more capable the threat actor 
  # is vs. the capability defending against it
  mean_tc_exceedance <- mean(TCsamples[VULNsamples == TRUE] -
                               DIFFsamples[VULNsamples],
                             na.rm = TRUE)
  if (is.nan(mean_tc_exceedance)) mean_tc_exceedance <- 0

  return(list(loss_events = length(VULNsamples[VULNsamples == TRUE]),
              mean_tc_exceedance = mean_tc_exceedance))
}

calculate_ale <- function(scenario,
                          diff_samples = NULL,
                          diff_estimates = NULL,
                          n = 10 ^ 4,
                          title = "Untitled",
                          verbose = FALSE) {
  # run an OpenFAIR simulation
  #
  # ARGS:
  #   scenario - l/ml/h/conf parameters for tef, ts, and lm
  #   diff_samples - sampled difficulties for the scenario
  #   diff_estimates - sampled difficulties for the scenario
  #   n - number of simulations to run
  #   title - optional name of scenario
  #   verbose - whether to print progress indicators
  #
  # RETURNS:
  #   list of scenario name, threat.events count, loss.events count,
  #   and ALE samples

  # make samples repeatable (and l33t)
  set.seed(31337)

  if (verbose) {
    cat(paste0("Working on scenario ", title, "\n"))
    cat(paste("Scenario is: ", scenario[, -which(names(scenario) %in%
                                                   "diff_samples")], "\n"))
    #cat(paste("Names are ", names(scenario), "\n"))
  }

  # calibrated estimates
  TEFestimate <- data.frame(l = scenario$tef_l, ml = scenario$tef_ml,
                            h = scenario$tef_h, conf = scenario$tef_conf)
  TCestimate  <- data.frame(l = scenario$tc_l, ml = scenario$tc_ml,
                            h = scenario$tc_h, conf = scenario$tc_conf)
  if (is.null(diff_samples)) {
    #DIFFestimate <- data.frame(l = scenario$diff_ml, ml = scenario$diff_ml,
    #                           h = scenario$diff_h, conf = scenario$diff_conf)
    DIFFestimate <- diff_estimates
  }
  LMestimate  <- data.frame(l = scenario$lm_l, ml = scenario$lm_ml,
                            h = scenario$lm_h, conf = scenario$lm_conf)

  # TEF - how many contacts do we have in each simulated year
  TEFsamples <- rpert(n, TEFestimate$l, TEFestimate$ml, TEFestimate$h,
                      shape = TEFestimate$conf)
  # fractional events per year is nonsensical, so round to the nearest integer
  TEFsamples <- round(TEFsamples)

  # for each number of contacts (TEF), calculate how many of those succeed
  # (become LEF counts)
  # LEF contains:  #er of threat events, # of times system is vuln
  if (is.null(diff_samples)) {
    results <- lapply(TEFsamples, 
                      function(x) select_events(x, TCestimate, 
                                                DIFFestimate = DIFFestimate))
  } else {
    results <- lapply(TEFsamples, 
                      function(x) select_events(x, TCestimate, 
                                                DIFFsamples = diff_samples))
  }
  LEF <- unlist(results)[attr(unlist(results), "names") == "loss_events"]
  mean_tc_exceedance <- unlist(results)[attr(unlist(results), "names") ==
                                          "mean_tc_exceedance"]


  # for the range of loss events, calculate the annual sum of losses across
  # the range of possible LMs
  loss_samples <- map_df(unname(LEF), function(x) sample_lm(x, l = LMestimate$l,
                                                  ml = LMestimate$ml,
                                                  h = LMestimate$h,
                                                  conf = LMestimate$conf))
  loss_samples <- bind_rows(loss_samples)

  # summary stats for ALE
  if (verbose) {
    print(summary(loss_samples$ale))
    value_at_risk <- quantile(loss_samples$ale, probs = (0.95))
    cat(paste0("Losses at 95th percentile are $",
               format(value_at_risk, nsmall = 2, big.mark = ","), "\n"))
  }

  simulation_results <- data_frame(title = rep(as.character(title), n),
                             simulation = seq(1:n),
                             threat_events = TEFsamples,
                             loss_events = LEF,
                             mean_tc_exceedance = mean_tc_exceedance,
                             ale = loss_samples$ale,
                             sle_max = loss_samples$sle_max,
                             sle_min = loss_samples$sle_min,
                             sle_mean = loss_samples$sle_mean,
                             sle_median = loss_samples$sle_median)

  return(simulation_results)
}
