#' Validate qualitative scenario data
#'
#' Run a set of basic consistency checks on the key qualitative data inputs
#'   (scenarios, capabilities, domains, and mappings).
#'
#'   Checks that:
#'   - All scenarios are distinct
#'   - All controls referenced in scenarios are defined in the controls table
#'   - All controls are distinct
#'
#' @importFrom dplyr tally filter left_join rename_ anti_join
#' @importFrom rlang .data
#' @importFrom stringi stri_split_fixed
#' @importFrom tidyr separate_rows
#' @param scenarios Dataframe of qualitative scenarios.
#' @param capabilities Dataframe of capabilities.
#' @param domains Dataframe of domain mappings.
#' @param mappings Dataframe of qualitative to quantitative mappings.
#' @export
#' @return An invisible boolean as to success/failure of validation steps.
#' @examples
#' \dontrun{
#' validate_scenarios(scenarios, capabilities, domains, mappings)
#' }
validate_scenarios <- function(scenarios, capabilities, domains, mappings) {

  validated <- TRUE

  # Verify there are no duplicate scenarios
  scenarios %>% dplyr::group_by_at(.vars = "scenario_id") %>% dplyr::tally() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::left_join(scenarios, by = "scenario_id") %>%
    dplyr::rename(times_duplicated = .data$n) ->
    duplicate_scenarios
  if (nrow(duplicate_scenarios) != 0) {
    warning(paste("Duplicate scenarios found:",
                  duplicate_scenarios$scenario_id, collapse = "\n"),
            call. = FALSE)
    validated <- FALSE
  }

  # Are all the capabilities referenced in the scenarios defined?
  missing_capabilities <- scenarios %>%
    tidyr::separate_rows(.data$controls, sep = ", ", convert = FALSE) %>%
    dplyr::anti_join(capabilities, by = c("controls" = "capability_id"))
  if (nrow(missing_capabilities) != 0) {
    warning(paste("Scenarios with undefined capabilities:",
                  missing_capabilities$scenario_id, collapse = "\n"),
            call. = FALSE)
    validated <- FALSE
  }

  # Verify there are no duplicate controls
  capabilities %>% dplyr::group_by_at(.vars = "capability_id") %>% dplyr::tally() %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::left_join(capabilities, by = "capability_id") %>%
    dplyr::rename(times_duplicated = .data$n) -> duplicate_capabilities
  if (nrow(duplicate_capabilities) != 0) {
    warning(paste("Duplicate capabilities found:",
                  duplicate_capabilities$capability_id, collapse = "\n"),
            call. = FALSE)
    validated <- FALSE
  }

  ## ----check_qualitative_mappings------------------------------------------
  stop_message <- NULL

  # TEF
  if (!all((tolower(distinct(scenarios, .data$tef)$tef) %in% filter(mappings, .data$type=="tef")$label))) {
    stop_message <- paste(stop_message,
                          "There are qualitative TEF values in the scenarios which are not found in the mappings", sep = "\n")
  }
  # TC
  if (!all((tolower(distinct(scenarios, .data$tc)$tc) %in% filter(mappings, .data$type=="tc")$label))) {
    stop_message <- paste(stop_message,
                          "There are qualitative TC values in the scenarios which are not found in the mappings", sep = "\n")
  }
  # DIFF
  if (!all((tolower(distinct(capabilities, .data$diff)$diff) %in% tolower(filter(mappings, .data$type=="diff")$label)))) {
    stop_message <- paste(stop_message,
                          "There are qualitative DIFF values in the capabilities which are not found in the mappings", sep = "\n")
  }
  # LM
  if (!all((tolower(distinct(scenarios, .data$lm)$lm) %in% filter(mappings, .data$type=="lm")$label))) {
    stop_message <- paste(stop_message,
                          "There are qualitative LM values in the scenarios which not found in the mappings", sep = "\n")
  }

  if (!is.null(stop_message)) {
    warning(stop_message, call. = FALSE)
    validated <- FALSE
  }

  # add the number of controls applicable to each scenario as a validation step
  scenarios <- scenarios %>%
    dplyr::rowwise() %>%
    dplyr::mutate(control_count =
                    length(stringi::stri_split_fixed(.data$controls,
                                                     ", ",
                                                     simplify = TRUE))) %>%
    dplyr::ungroup()

  invisible(validated)
}
