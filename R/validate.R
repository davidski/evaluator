## ----valdiate_spreadsheet------------------------------------------------
#' Validate scenario data.
#'
#' @import dplyr
#' @param scenarios Dataframe of scenarios
#' @param capabilities Dataframe of capabilities
#' @param domains Dataframe of domain mappings
#' @param mappings Dataframe of qualitative mappings
#' @export
#' @return Invisible NULL.
validate_scenarios <- function(scenarios, capabilities, domains, mappings) {
  # Check that scenario IDs have no gaps
  scenario_gaps <- setdiff(seq(range(scenarios$scenario_id)[1],
                               range(scenarios$scenario_id)[2]),
                           scenarios$scenario_id)
  if (length(scenario_gaps) != 0) stop(paste("Scenario gaps found:",
                                             scenario_gaps, collapse = "\n"))

  # Verify there are no duplicate scenarios
  scenarios %>% group_by_("scenario_id") %>% tally %>%
    filter_(~ n > 1) %>%
    left_join(scenarios, by = c("scenario_id" = "scenario_id")) %>%
    rename_(times_duplicated = "n") ->
    duplicate_scenarios
  if (nrow(duplicate_scenarios) != 0) stop(paste("Duplicate scenarios found:",
                                                 duplicate_scenarios$scenario_id,
                                                 collapse = "\n"))

  # Check that control IDs have no gaps
  capability_gaps <- setdiff(seq(range(capabilities$id)[1],
                                 range(capabilities$id)[2]),
                             capabilities$id)
  if (length(capability_gaps) != 0) stop(paste("Capability gaps found:",
                                               capability_gaps, collapse = "\n"))

  # Are all the capabilities referenced in the scenarios defined?
  missing_capabilities <- scenarios %>%
    tidyr::separate_rows_("controls", sep = ",", convert = TRUE) %>%
    anti_join(capabilities, by = c("controls" = "id"))
  if (nrow(missing_capabilities) != 0) stop(paste("Scenarios with undefined controls:",
                                                  missing_capabilities$scenario_id,
                                                  collapse = "\n"))

  # Verify there are no duplicate controls
  capabilities %>% group_by_("id") %>% tally %>%
    filter_(~ n > 1) %>% left_join(capabilities, by = c("id" = "id")) %>%
    rename_(times_duplicated = "n") -> duplicate_capabilities
  if (nrow(duplicate_capabilities) != 0) stop(paste("Duplicate capabilities found:",
                                                    duplicate_capabilities$id,
                                                    collapse = "\n"))

  ## ----check_qualitative_mappings------------------------------------------
  stop_message <- NULL

  # TEF
  if (!all((tolower(distinct_(scenarios, "tef")$tef) %in% filter_(mappings, ~ type=="tef")$label))) {
    stop_message <- paste(stop_message,
                          "There are TEF values in the scenarios spreadsheet which are not in the mappings", sep = "\n")
  }
  # TC
  if (!all((tolower(distinct_(scenarios, "tc")$tc) %in% filter_(mappings, ~ type=="tc")$label))) {
    stop_message <- paste(stop_message,
                          "There are TC values in the scenarios spreadsheet not found in the mappings", sep = "\n")
  }
  # DIFF
  if (!all((tolower(distinct_(capabilities, "diff")$diff) %in% tolower(filter_(mappings, ~ type=="diff")$label)))) {
    stop_message <- paste(stop_message,
                          "There are DIFF values in the scenarios spreadsheet not found in the mappings", sep = "\n")
  }
  # LM
  if (!all((tolower(distinct_(scenarios, "lm")$lm) %in% filter_(mappings, ~ type=="lm")$label))) {
    stop_message <- paste(stop_message,
                          "There are LM values in the scenarios spreadsheet not found in qualitative_mappings.csv", sep = "\n")
  }

  if (!is.null(stop_message)) {
    stop(stop_message)
  }


  # add the number of controls applicable to each scenario as a validation step
  scenarios <- scenarios %>%
    rowwise %>%
    mutate_("control_count" =
              ~ length(stringi::stri_split_fixed(controls, ", ", simplify = TRUE))) %>%
    ungroup

  invisible(NULL)
}
