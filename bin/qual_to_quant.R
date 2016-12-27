convert_tef_qual_to_quant <- function(scenario) {
  # Convert qualitative ratings to quant estimate ranges
  #
  # ARGS:
  #		scenario - qualitative TEF
  #
  # RETURNS:
  # 	dataframe of tef_estimate
  
  tef_estimate <- data.frame(l = rep(NA, length(scenario)), 
                             ml = rep(NA, length(scenario)),
                             h = rep(NA, length(scenario)),
                             conf = rep(NA, length(scenario)))
  
  tef_estimate[scenario == "Frequent", ] <- data.frame(l = 1, 
                                                       ml = 24, 
                                                       h = 90, 
                                                       conf = 1)
  tef_estimate[scenario == "Occasional", ] <- data.frame(l = 1, 
                                                         ml = 12, 
                                                         h = 50, 
                                                         conf = 1)
  tef_estimate[scenario == "Rare", ] <- data.frame(l = 0, 
                                                   ml = 6, 
                                                   h = 50, 
                                                   conf = 1)
  #print(paste("TEF is ", TEF))
  
  return(tef_estimate)
}

convert_tc_qual_to_quant <- function(scenario) {
  # Convert qualitative ratings to quant estimate ranges
  #
  # ARGS:
  #		scenario - qualitative TC
  #
  # RETURNS:
  # 	dataframe of tc_estimate
  
  tc_estimate <- data.frame(l = rep(NA, length(scenario)), 
                            ml = rep(NA, length(scenario)),
                            h = rep(NA, length(scenario)),
                            conf = rep(NA, length(scenario)))
  
  tc_estimate[scenario == "High", ] <- data.frame(l = 40, 
                                                  ml = 50, 
                                                  h = 60, 
                                                  conf = 1)
  tc_estimate[scenario == "Medium", ] <- data.frame(l = 20, 
                                                    ml = 40, 
                                                    h = 60, 
                                                    conf = 1)
  tc_estimate[scenario == "Low", ] <- data.frame(l = 10, 
                                                 ml = 30, 
                                                 h = 50, 
                                                 conf = 1)
  
  return(tc_estimate)
  
}

convert_lm_qual_to_quant <- function(scenario) {
  # Convert qualitative ratings to quant estimate ranges
  #
  # ARGS:
  #		scenario - qualitative LM
  #
  # RETURNS:
  # 	dataframe of lm_estimate
  
  lm_estimate <- data.frame(l = rep(NA, length(scenario)), 
                            ml = rep(NA, length(scenario)),
                            h = rep(NA, length(scenario)),
                            conf = rep(NA, length(scenario)))
  
  lm_estimate[scenario == "High", ] <- data.frame(l = 40, 
                                                  ml = 50, 
                                                  h = 60, 
                                                  conf = 1)
  lm_estimate[scenario == "Medium", ] <- data.frame(l = 20, 
                                                    ml = 40, 
                                                    h = 60, 
                                                    conf = 1)
  lm_estimate[scenario == "Low", ] <- data.frame(l = 10, 
                                                 ml = 30, 
                                                 h = 50, 
                                                 conf = 1)
  
  return(lm_estimate)
  
}

agg_controls <- function(diff, n = 10000, verbose = FALSE) {
  # Create aggregate sample of control strength
  #
  # ARGS:
  #		diff - vector of indicies to various distribution parameters
  #   n - number of simulations to run
  #
  # RETURNS:
  # 	dataframe of mean control strength for all simulations
  
  diff_parms <- convert_diff_qual_to_quant(diff)
  if (verbose) print(str(diff_parms))
  diff_samples <- rpert(n*length(diff), diff_parms$l, diff_parms$ml, 
                        diff_parms$h, diff_parms$conf) %>% 
    matrix(byrow = TRUE, ncol = length(diff)) %>% rowMeans()
  return(diff_samples)
}

convert_diff_qual_to_quant <- function(scenario) {
  # Convert qualitative ratings to quant estimate ranges
  #
  # ARGS:
  #		scenario - qualitative scenario, including DIFF qualitative labels
  #
  # RETURNS:
  # 	dataframe of estimates
  
  #print(paste("length of scenario is", length(scenario)))
  diff_estimate <- data.frame(l = rep(NA, length(scenario)), 
                              ml = rep(NA, length(scenario)),
                              h = rep(NA, length(scenario)),
                              conf = rep(NA, length(scenario)))
  diff_estimate[scenario == 5, ] <- data.frame(l = 50, ml = 60, 
                                               h = 90, conf = 1)
  diff_estimate[scenario == 4, ] <- data.frame(l = 40, ml = 60, 
                                               h = 80, conf = 1)
  diff_estimate[scenario == 3, ] <- data.frame(l = 40, ml = 50, 
                                               h = 60, conf = 1)
  diff_estimate[scenario == 2, ] <- data.frame(l = 20, ml = 40, 
                                               h = 60, conf = 1)
  diff_estimate[scenario == 1, ] <- data.frame(l = 10, ml = 30, 
                                               h = 40, conf = 1) 
  
  #estimate <- tbl_df(diff_estimate)
  
  return(diff_estimate)
}