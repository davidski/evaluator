#' Create initial template files
#'
#' Given a base directory, copy the provided sample files into an `inputs`
#'   subdirectory. This makes the starter files available for customizing and
#'   data collection. The `inputs` directory will be created if not already present.
#'   Preexisting files, if present, will not be overwritten. Also creates an
#'  empty `results` subdirectory as a default location for evaluator output.
#'
#' @importFrom purrr map_dfr
#' @importFrom tibble add_row tibble
#' @param base_directory Parent directory under which to create starter files.
#' @export
#' @return A dataframe of the starter filenames, along with a flag on whether a file was copied.
#' @examples
#' \dontrun{
#' create_templates("~/evaluator")
#' }
create_templates <- function(base_directory = "~/evaluator"){

  inputs_dir = file.path(base_directory, "inputs")
  if (!dir.exists(inputs_dir)) dir.create(inputs_dir, recursive = TRUE)

  results_dir = file.path(base_directory, "results")
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

  # copy all the standard CSV files
  res <- c("domains.csv", "qualitative_mappings.csv", "risk_tolerances.csv") %>%
    purrr::map_dfr(
      ~ tibble::tibble(filename = .x,
                          copied = file.copy(system.file("extdata", .x,
                                                         package = "evaluator"),
                                             inputs_dir)))

  # copy the quick start script
  res <- tibble::add_row(res, filename = "run_analysis.R",
                         copied = file.copy(system.file("run_analysis.R",
                                                        package = "evaluator"),
                                            base_directory))

  # copy the survey file
  res <- tibble::add_row(res, filename = "survey.xlsx",
                         copied = file.copy(system.file("survey", "survey.xlsx",
                                                        package = "evaluator"),
                                            inputs_dir))

  return(res)
}


#' Load input and results files
#'
#' Given an input directory and a directory of simulation results, load all
#'   of the key Evaluator data objects into memory.
#'
#' @param input_directory Location of input files.
#' @param results_directory Location of simulation results.
#' @return List of all key data objects.
#' @examples
#' \dontrun{
#' load_data("~/evaluator/inputs", "~/evaluator/results")
#' }
#'
#' @name load_data-deprecated
#' @seealso \code{\link{evaluator-deprecated}}
#' @keywords internal
NULL

#' @importFrom dplyr summarize mutate group_by_at
#' @importFrom rlang .data
#' @importFrom readr read_csv cols col_character col_integer col_double
#' @rdname evaluator-deprecated
#' @section \code{load_data}:
#' Instead of \code{load_data}, use \code{\link{read_quantitative_inputs}}
#' and \code{\link{read_qualitative_inputs}} as appropriate.
#' @export
load_data <- function(input_directory = "~/evaluator/inputs", results_directory = "~/evaluator/results") {
  .Deprecated("read_quantitative_inputs()")
  # load simulation results and default summary objects
  simulation_results <- NULL # detailed sumulation results
  load(file.path(results_directory, "simulation_results.rda"))
  scenario_summary <- NULL   # scenario level summary
  load(file.path(results_directory, "scenario_summary.rda"))
  domain_summary <- NULL     # domain level summary
  load(file.path(results_directory, "domain_summary.rda"))

  # all input files
  domains <- readr::read_csv(file.path(input_directory, "domains.csv"),
                             col_types = readr::cols(
                                 domain_id = readr::col_character(),
                                 domain = readr::col_character()
                                 )) # domain catalog
  mappings <- readr::read_csv(file.path(input_directory, "qualitative_mappings.csv"),
                              col_types = readr::cols(
                                type = readr::col_character(),
                                label = readr::col_character(),
                                l = readr::col_integer(),
                                ml = readr::col_double(),
                                h = readr::col_integer(),
                                conf = readr::col_integer()
                              ))  # qualitative translations
  capabilities <- readr::read_csv(file.path(input_directory,
                                            "capabilities.csv"),
                                  col_types = readr::cols(
                                    capability_id = readr::col_character(),
                                    domain_id = readr::col_character(),
                                    capability = readr::col_character(),
                                    diff = readr::col_character()))  # i.e. objectives & controls
  risk_tolerances <- readr::read_csv(file.path(input_directory,
                                               "risk_tolerances.csv"),
                                     col_types = readr::cols(
                                       level = readr::col_character(),
                                       amount = readr::col_integer()
                                     ))  # i.e. risk tolerances
  qualitative_scenarios <- readr::read_csv(file.path(input_directory,
                                                     "qualitative_scenarios.csv"),
                                           col_types = readr::cols(
                                             scenario_id = readr::col_character(),
                                             scenario = readr::col_character(),
                                             tcomm = readr::col_character(),
                                             tef = readr::col_character(),
                                             tc = readr::col_character(),
                                             lm = readr::col_character(),
                                             domain_id = readr::col_character(),
                                             controls = readr::col_character()
                                           )) %>%
    dplyr::mutate(tef = tolower(.data$tef),
                  lm = tolower(.data$lm),
                  tc = tolower(.data$tc))

  # precalculate the standard order of scenarios (domain, then ID of the scenario)
  scenario_order <- dplyr::group_by_at(simulation_results,
                                       .vars = c("domain_id", "scenario_id")) %>%
    dplyr::summarize()

  # build a vector of scenario outliers
  scenario_outliers <- identify_outliers(scenario_summary)

  # build a named vector of risk tolerance threshold
  risk_tolerance <- risk_tolerances$amount
  names(risk_tolerance) <- risk_tolerances$level %>% tolower

  # enhance scenario_summary assign loss tolerance to ALE VaR size
  # scenario_summary <- mutate(scenario_summary, annual_tolerance =
  #                              ifelse(.data$ale_var >= risk_tolerance["high"],
  #                                     "High",
  #                                     ifelse(.data$ale_var >= risk_tolerance["medium"],
  #                                            "Medium",
  #                                            "Low"))) %>%
  #   mutate(annual_tolerance = factor(.data$annual_tolerance,
  #                                    levels = c("High", "Medium", "Low"),
  #                                    ordered = TRUE))

  list(simulation_results = simulation_results,
       scenario_summary = scenario_summary,
       domain_summary = domain_summary,
       domains = domains,
       mappings = mappings,
       capabilities = capabilities,
       risk_tolerances = risk_tolerances,
       risk_tolerance = risk_tolerance,
       scenario_outliers = scenario_outliers,
       scenarios = qualitative_scenarios)
}

#' Load qualitative inputs
#'
#' Given an input directory, load the key qualitative objects into memory.
#'   The key qualitative inputs for Evaluator processing include:
#'     * domains - domains and domain_ids
#'     * mappings - qualitative to quantitative mappings
#'     * capabilities - qualitative capabilities
#'     * qualitative_scenarios - qualitative risk scenarios
#'
#' @importFrom dplyr summarize mutate group_by_at
#' @importFrom rlang .data
#' @importFrom readr read_csv cols col_character col_integer col_double
#' @param input_directory Location of input files.
#' @return List of domains, quantitative_scenarios, and risk_tolerances
#' @export
#' @examples
#' \dontrun{
#' read_qualitative_inputs("~/evaluator/inputs")
#' }
read_qualitative_inputs <- function(input_directory = "~/evaluator/inputs") {

  domains <- readr::read_csv(file.path(input_directory, "domains.csv"),
                             col_types = readr::cols(
                               domain_id = readr::col_character(),
                               domain = readr::col_character()
                             )) # domain catalog
  mappings <- readr::read_csv(file.path(input_directory, "qualitative_mappings.csv"),
                              col_types = readr::cols(
                                type = readr::col_character(),
                                label = readr::col_character(),
                                l = readr::col_integer(),
                                ml = readr::col_double(),
                                h = readr::col_integer(),
                                conf = readr::col_integer()
                              ))  # qualitative translations
  capabilities <- readr::read_csv(file.path(input_directory,
                                            "capabilities.csv"),
                                  col_types = readr::cols(
                                    capability_id = readr::col_character(),
                                    domain_id = readr::col_character(),
                                    capability = readr::col_character(),
                                    diff = readr::col_character()))  # i.e. objectives & controls
  qualitative_scenarios <- readr::read_csv(file.path(input_directory,
                                                     "qualitative_scenarios.csv"),
                                           col_types = readr::cols(
                                             scenario_id = readr::col_character(),
                                             scenario = readr::col_character(),
                                             tcomm = readr::col_character(),
                                             tef = readr::col_character(),
                                             tc = readr::col_character(),
                                             lm = readr::col_character(),
                                             domain_id = readr::col_character(),
                                             controls = readr::col_character()
                                           )) %>%
    dplyr::mutate(tef = tolower(.data$tef),
                  lm = tolower(.data$lm),
                  tc = tolower(.data$tc))

  list(domains = domains,
       mappings = mappings,
       capabilities = capabilities,
       qualitative_scenarios = qualitative_scenarios)
}



#' Load quantitative inputs
#'
#' Given an input directory, load the key quantitative objects into memory.
#'   The key quantitative inputs for Evaluator processing include:
#'     * domains - domains and domain_ids
#'     * risk_tolerances - the risk tolerances of the organization
#'     * quantitative_scenarios - risk scenarios and quantified parameters
#'
#' @importFrom dplyr summarize mutate group_by_at
#' @importFrom rlang .data
#' @importFrom readr read_csv cols col_character col_integer col_double
#' @param input_directory Location of input files.
#' @return List of domains, quantitative_scenarios, and risk_tolerances
#' @export
#' @examples
#' \dontrun{
#' read_quantitative_inputs("~/evaluator/inputs")
#' }
read_quantitative_inputs <- function(input_directory = "~/evaluator/inputs") {

  # all input files
  domains <- readr::read_csv(file.path(input_directory, "domains.csv"),
                             col_types = readr::cols(
                               domain_id = readr::col_character(),
                               domain = readr::col_character()
                             )) # domain catalog
  risk_tolerances <- readr::read_csv(file.path(input_directory,
                                               "risk_tolerances.csv"),
                                     col_types = readr::cols(
                                       level = readr::col_character(),
                                       amount = readr::col_integer()
                                     ))  # i.e. risk tolerances

  quantitative_scenarios <- readRDS(file.path(input_directory, "quantitative_scenarios.Rds"))

  list(domains = domains,
       risk_tolerances = risk_tolerances,
       quantitative_scenarios = quantitative_scenarios)
}
