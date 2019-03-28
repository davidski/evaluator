#' Import the scenario spreadsheet
#'
#' This is a convenience wrapper around the \code{\link{import_scenarios}} and
#'   \code{\link{import_capabilities}} functions. Writes cleaned
#'   comma-separated formatted files for the scenarios and
#'   capabilities to disk.
#'
#' @importFrom dplyr %>%
#' @importFrom readr write_csv
#' @importFrom utils data
#' @importFrom tibble as_tibble rownames_to_column
#' @param survey_file Path to survey Excel file. Defaults to an Evaluator-provided sample spreadsheet.
#' @param domains Dataframe of domains and domain IDs. Defaults to built-in sample \code{domains} dataset.
#' @param output_dir Output file directory.
#' @export
#' @return Dataframe of file information on the two newly created files.
import_spreadsheet <- function(survey_file = system.file("survey",
                                                         "survey.xlsx",
                                                         package = "evaluator"),
                               domains = NULL,
                               output_dir = "~/evaluator/results"){

  ## ----survey_sheet--------------------------------------------------------
  #message("Target file is ", survey_file)
  if (!dir.exists(output_dir)) {
    stop("Non existant output directory specified - ", output_dir)}

  qualitative_scenarios <- import_scenarios(survey_file = survey_file,
                                            domains = domains)
  capabilities <- import_capabilities(survey_file = survey_file,
                                      domains = domains)

  ## ----write_files---------------------------------------------------------
  if (!dir.exists(output_dir)) dir.create(output_dir)
  readr::write_csv(capabilities, path = file.path(output_dir, "capabilities.csv"))
  readr::write_csv(qualitative_scenarios, path =
                     file.path(output_dir, "qualitative_scenarios.csv"))

  file.info(c(file.path(output_dir, "capabilities.csv"),
              file.path(output_dir, "qualitative_scenarios.csv"))) %>%
    tibble::rownames_to_column("filename") %>% tibble::as_tibble()
}

#' Import scenarios from survey spreadsheet
#'
#' @importFrom dplyr funs select mutate mutate_at arrange
#' @importFrom rlang .data
#' @importFrom utils data
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom readxl read_excel
#' @param survey_file Path to survey Excel file. Defaults to a sample file if not supplied.
#' @param domains Dataframe of domains and domain IDs.
#' @export
#' @return Extracted qualitative scenarios as a dataframe.
#' @examples
#' data(mc_domains)
#' import_scenarios(domains = mc_domains)
import_scenarios <- function(survey_file = NULL, domains = NULL) {

  survey_file <- if (is.null(survey_file)) {
    system.file("survey", "survey.xlsx", package = "evaluator")
  } else {
    survey_file
  }
  if (!file.exists(survey_file)) {
    stop("Non existant survey file specified - ", survey_file)}

  # use default domains if none supplied
  domains <- if (is.null(domains)) {
    env <- new.env()
    utils::data("mc_domains", package = "evaluator", envir = env)
    get("mc_domains", envir = env)
  } else {domains}

  # scenarios begin with the list of domains
  scenarios <- domains

  # attach each sheet as a nested dataframe to the domains object
  scenarios$raw_data <- scenarios$domain_id %>%
    purrr::map(~ readxl::read_excel(
      survey_file, skip = 2, col_names = c("Name", "DIFF", "Evidence",
                                           "CapabilityID", letters[5:7]),
      sheet = .))

  # separate out the threats from the nested frame
  scenarios$threats <- scenarios$raw_data %>%
    purrr::map(~split_sheet(dat = .x, table_type = "threats"))
  scenarios <- scenarios[, (!names(scenarios) %in% "raw_data")]

  # fetch threats
  scenarios <- tidyr::unnest(scenarios, .data$threats)

  # clean the frame for returning
  dplyr::select(scenarios, scenario_id = "ScenarioID", scenario = "Scenario",
         tcomm = "TComm", tef = "TEF", tc = "TC", lm = "LM",
         domain_id = "domain_id", controls = "Capabilities") %>%
    dplyr::mutate_at(vars("tef", "lm", "tc"), dplyr::funs(tolower)) %>%
    dplyr::arrange(.data$scenario_id)

}

#' Import capabilities from survey spreadsheet
#'
#' @importFrom dplyr mutate select arrange
#' @importFrom readxl read_excel
#' @importFrom purrr map
#' @importFrom rlang .data
#' @param survey_file Path to survey Excel file. If not supplied, a default sample file is used.
#' @param domains Dataframe of domains and domain IDs.
#' @export
#' @return Extracted capabilities as a dataframe.
#' @inheritParams import_scenarios
#' @examples
#' data(mc_domains)
#' import_capabilities(domains = mc_domains)
import_capabilities <- function(survey_file = NULL, domains = NULL){

  survey_file <- if (is.null(survey_file)) {
    system.file("survey", "survey.xlsx", package = "evaluator")
    } else {
      survey_file
    }
  if (!file.exists(survey_file)) {
    stop("Non existant survey file specified - ", survey_file)}

  # use default domains if none supplied
  domains <- if (is.null(domains)) {
    env <- new.env()
    utils::data("mc_domains", package = "evaluator", envir = env)
    get("mc_domains", env)
  } else {domains}

  raw_domains <- domains %>%
    dplyr::mutate(raw_data = purrr::map(.data$domain_id, ~ readxl::read_excel(
      survey_file, skip = 2,
      col_names = c("Name", "DIFF", "Evidence", "CapabilityID", letters[5:7]), sheet = .)))

  ## ----walk_the_frame------------------------------------------------------
  dat <- raw_domains %>%
    dplyr::mutate(capabilities = purrr::map(.data$raw_data, split_sheet)) %>%
    dplyr::select(-.data$raw_data)

  # fetch and clean capabilities
  capabilities <- tidyr::unnest(dat, capabilities) %>%
    dplyr::select(capability_id = .data$CapabilityID, .data$domain_id,
                  capability = .data$Name, diff = .data$DIFF) %>%
    dplyr::arrange(.data$capability_id)

  capabilities
}

#' Split a sheet of the survey spreadsheet into either capabilities or threats
#'
#' The default data collection Excel spreadsheet solicits threat
#'   scenarios and applicable controls for each domain. This function
#'   takes a single sheet from the spreadsheet, as read by \code{\link[readxl]{readxl}}
#'   and pulls out either the capabilities or threats, as directed by the
#'   user.
#'
#' @importFrom dplyr %>% select
#' @param dat Raw sheet input from \code{\link[readxl]{readxl}}.
#' @param table_type Either \code{capabilities} or \code{threats}
#' @return Extracted table as a dataframe
split_sheet <- function(dat, table_type = "capabilities") {
  # remote all NA rows
  dat <- dat[!!rowSums(!is.na(dat) != 0 ), ]
  # find the row where the threats table starts
  split_point <- which(dat$Name == "Threats")
  #message("Type is ", table_type)

  if (table_type == "capabilities") {
    # generate capabilities data
    capabilities_data <- dat[1:(split_point - 1), ]
    names(capabilities_data) <-
      make.names(
        names = names(capabilities_data),
        unique = TRUE,
        allow_ = TRUE
      )
    capabilities_data <- dplyr::select(capabilities_data, matches("^[^X]"))
    capabilities_data
  } else {
    # generate threats_data
    threats_data <- dat[(split_point + 2):nrow(dat), ]
    names(threats_data) <-
      dat[(split_point + 1), ] %>% t %>% as.matrix() %>% as.vector()
    threats_data
  }
}
