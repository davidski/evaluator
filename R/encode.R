## ----encode_capabilities-------------------------------------------------
#' Encode qualitative data to quantitative parameters.
#'
#' @import dplyr
#' @importFrom magrittr "%<>%"
#' @param scenarios Qualitative risk scenarios dataframe
#' @param capabilities Qualitative program capabilities dataframe
#' @param mappings Number of simulations for each scenario
#' @export
#' @return Dataframe of encoded quantitative scenarios
#'
encode_scenarios <- function(scenarios, capabilities, mappings) {
  # fetch DIFF params
  scenarios$diff_params <- purrr::map2(scenarios$controls, scenarios$scenario_id,
                             ~derive_controls(labels = .x, id = .y,
                                              capabilities = capabilities,
                                              mappings = mappings))
  #%>%  purrr::flatten()
  #names(diff_params) <- scenarios$scenario_id

  # # verify everything encoded
  # unencoded_params <- purrr::map(diff_params, ~ sum(!complete.cases(.))) %>%
  #   unlist(use.names = FALSE) %>% sum
  # if (unencoded_params > 0) {
  #   stop("Not able to translate all capabilities to parameters. ",
  #        "Do the scenarios have matches in the mapping table?")
  # }

  # add the DIFF params to the scenarios object
  # scenarios$diff_params <- diff_params

  # fetch TEF params
  scenarios %<>% left_join(mappings[mappings$type == "tef",],
                           by = c("tef" = "label")) %>%
    rename_("tef_l" = "l", "tef_ml" = "ml", "tef_h" = "h", "tef_conf" = "conf") %>%
    select_('-c(tef, type)')

  # fetch TC params
  scenarios %<>% left_join(mappings[mappings$type == "tc",],
                           by = c("tc" = "label")) %>%
    rename_("tc_l" = "l", "tc_ml" = "ml", "tc_h" = "h", "tc_conf" = "conf") %>%
    select_('-c(tc, type)')

  # fetch LM params
  scenarios %<>% left_join(mappings[mappings$type == "lm",],
                           by = c("lm" = "label")) %>%
    rename_("lm_l" = "l", "lm_ml" = "ml", "lm_h" = "h", "lm_conf" = "conf") %>%
    select_('-c(lm, type)')

  scenarios
}

#' Derive control difficulty parameters for a given qualitative scenario.
#'
#' Given a comma separated list of control IDs in a scenario, identify
#' the qualitative rankings associated with each scenario, convert to
#' their quantitative parameters, and return a dataframe of the set of
#' parameters.
#'
#' @param labels Comma delimited list of qualitative labels
#' @param capabilities Character string of the type (tef, tc, diff, etc)
#' @param mappings Qualitative mappings dataframe
#' @param id Character string of the type (tef, tc, diff, etc)
#' @import dplyr
#' @return List-wrapped dataframe of estimate parameters
derive_controls <- function(labels, capabilities, mappings, id = "None") {
  control_list <- stringi::stri_split_fixed(labels, ", ") %>% unlist()

  control_list <- purrr::map_df(control_list,
                                ~ capabilities[as.numeric(.x),"diff"]) %>%
    mutate(control_id = control_list)

  # Find the qualitative rating for each control ID, then lookup it's
  # distribution parameters from the mappings table
  #results <- capabilities[capabilities$id %in%
  #                          as.numeric(control_list), "diff"] %>%
  results <- control_list %>%
    mutate_(label = ~ as.character(diff)) %>% select(-diff) %>%
    convert_qual_to_quant(qual_type = "diff", mappings = mappings)

  #results <- list(id = results)
  #names(results) <- id
  return(results)
}

#' Convert qualitative ratings to quantitative estimate ranges.
#'
#' @param qual_label Dataframe of qualitative labels (label=H/M/L, etc)
#' @param qual_type Character string of the type (tef, tc, diff, etc)
#' @param mappings Qualitative mappings dataframe
#' @import dplyr
#' @return Dataframe of estimate parameters
#' @export
convert_qual_to_quant <- function(qual_label, qual_type, mappings) {

  filtered_mappings <- filter_(mappings, ~ type == qual_type)
  left_join(qual_label, filtered_mappings, by = c(label = "label"))
  # %>% select_("l", "ml", "h", "conf")
}
