#' Encode qualitative data to quantitative parameters
#'
#' Given an input of:
#'   * qualitative risk scenarios
#'   * qualitative capabilities
#'   * translation table from qualitative labels to quantitative parameters
#'
#'   Create a unified dataframe of quantitative scenarios ready for simulation.
#'
#' @importFrom dplyr rename_ select_ left_join filter rowwise
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
  tef_nested <- dplyr::filter(mappings, .data$type=="tef") %>%
    dplyr::rowwise() %>%
    dplyr::do(tef_params = list(min = .$l, mode = .$ml, max = .$h,
                                shape = .$conf, func = "mc2d::rpert"),
              label = .$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  scenarios <- dplyr::left_join(scenarios, tef_nested,
                                by = c("tef" = "label")) %>%
    dplyr::select(-.data$tef)

  # fetch TC params
  tc_nested <- dplyr::filter(mappings, .data$type=="tc") %>%
    dplyr::rowwise() %>%
    dplyr::do(tc_params = list(min = .$l, mode = .$ml, max = .$h,
                               shape = .$conf, func = "mc2d::rpert"),
              label = .$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  scenarios <- dplyr::left_join(scenarios, tc_nested,
                                by = c("tc" = "label")) %>%
    dplyr::select(-.data$tc)

  # fetch LM params
  lm_nested <- dplyr::filter(mappings, .data$type=="lm") %>%
    dplyr::rowwise() %>%
    dplyr::do(lm_params = list(min = .$l, mode = .$ml, max = .$h,
                               shape = .$conf, func = "mc2d::rpert"),
              label = .$label) %>%
    dplyr::mutate(label = as.character(.data$label))
  scenarios <- dplyr::left_join(scenarios, lm_nested,
                                by = c("lm" = "label")) %>%
    dplyr::select(-.data$lm)

  scenarios
}

#' Derive control difficulty parameters for a given qualitative scenario
#'
#' Given a comma-separated list of control IDs in a scenario, identify
#'   the qualitative rankings associated with each scenario, convert to
#'   their quantitative parameters, and return a dataframe of the set of
#'   parameters.
#'
#' @importFrom dplyr left_join mutate_ select rename pull
#' @importFrom rlang .data
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

  control_list <- capabilities[capabilities$capability_id %in% control_list, ] %>%
    dplyr::rename(control_id = capability_id)

  # Find the qualitative rating for each control ID, then lookup its
  # distribution parameters from the mappings table
  #results <- capabilities[capabilities$id %in%
  #                          as.numeric(control_list), "diff"] %>%
  results <- control_list %>%
    dplyr::mutate(label = as.character(.data$diff)) %>%
    dplyr::select(-diff) %>%
    dplyr::left_join(mappings[mappings$type == "diff", ],
                     by = c(label = "label")) %>%
    dplyr::rowwise() %>%
    dplyr::do(diff_params = list(min = .$l, mode = .$ml, max = .$h,
                                 shape = .$conf, func = "mc2d::rpert")) %>%
    dplyr::pull()

  return(results)
}
