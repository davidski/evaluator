#' Encode qualitative data to quantitative parameters
#'
#' Given an input of:
#' * qualitative risk scenarios
#' * qualitative capabilities
#' * translation table from qualitative labels to quantitative parameters
#'
#' Create a unified dataframe of quantitative scenarios ready for simulation.
#'
#' @importFrom dplyr rename_ select_ left_join
#' @importFrom rlang .data
#' @importFrom purrr map
#' @param scenarios Qualitative risk scenarios dataframe.
#' @param capabilities Qualitative program capabilities dataframe.
#' @param mappings Qualitative to quantitative mapping dataframe.
#' @export
#' @return A dataframe of capabilities for the scenario and parameters for quantified simulation.
#' @examples
#' data(qualitative_scenarios, capabilities, mappings)
#' encode_scenarios(qualitative_scenarios, capabilities, mappings)
encode_scenarios <- function(scenarios, capabilities, mappings) {
  # fetch DIFF params
  scenarios$diff_params <- purrr::map(scenarios$controls,
                             ~derive_controls(capability_ids = .x,
                                              capabilities = capabilities,
                                              mappings = mappings))
  # fetch TEF params
  scenarios <- dplyr::left_join(scenarios, mappings[mappings$type == "tef",],
                                by = c("tef" = "label")) %>%
    dplyr::rename("tef_l" = .data$l, "tef_ml" = .data$ml, "tef_h" = .data$h,
                  "tef_conf" = .data$conf) %>%
    dplyr::select_('-c(tef, type)')

  # fetch TC params
  scenarios <- dplyr::left_join(scenarios, mappings[mappings$type == "tc",],
                                by = c("tc" = "label")) %>%
    dplyr::rename_("tc_l" = "l", "tc_ml" = "ml", "tc_h" = "h",
                   "tc_conf" = "conf") %>%
    dplyr::select_('-c(tc, type)')

  # fetch LM params
  scenarios <- dplyr::left_join(scenarios, mappings[mappings$type == "lm",],
                                by = c("lm" = "label")) %>%
    dplyr::rename_("lm_l" = "l", "lm_ml" = "ml", "lm_h" = "h",
                   "lm_conf" = "conf") %>%
    dplyr::select_('-c(lm, type)')

  scenarios
}

#' Derive control difficulty parameters for a given qualitative scenario
#'
#' Given a comma-separated list of control IDs in a scenario, identify
#' the qualitative rankings associated with each scenario, convert to
#' their quantitative parameters, and return a dataframe of the set of
#' parameters.
#'
#' @importFrom dplyr left_join mutate_ select rename
#' @importFrom stringi stri_split_fixed
#' @param capability_ids Comma-delimited list of capabilities in scope for a scenario.
#' @param capabilities Dataframe of master list of all qualitative capabilities.
#' @param mappings Qualitative mappings dataframe.
#' @return A dataframe of quantitative estimate parameters for the capabilities
#'   applicable to a given scenario.
#' @export
#' @examples
#' data(capabilities)
#' capability_ids <- c("1, 3")
#' mappings <- data.frame(type = "diff", label = "1 - Immature", l = 0, ml = 2, h = 10,
#'                        conf = 3, stringsAsFactors = FALSE)
#' derive_controls(capability_ids, capabilities, mappings)
derive_controls <- function(capability_ids, capabilities, mappings) {
  control_list <- stringi::stri_split_fixed(capability_ids, ", ") %>% unlist()

  control_list <- capabilities[capabilities$id %in% as.numeric(control_list), ] %>%
    dplyr::rename(control_id = id)

  # Find the qualitative rating for each control ID, then lookup it's
  # distribution parameters from the mappings table
  #results <- capabilities[capabilities$id %in%
  #                          as.numeric(control_list), "diff"] %>%
  results <- control_list %>%
    dplyr::mutate_(label = ~ as.character(diff)) %>% dplyr::select(-diff) %>%
    dplyr::left_join(mappings[mappings$type == "diff", ],
                     by = c(label = "label"))

  return(results)
}
