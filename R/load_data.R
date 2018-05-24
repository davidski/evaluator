#' Create initial template files
#'
#' Given a base directory, copy the provided sample files into an `inputs`
#' subdirectory. This makes the starter files available for customizing and
#' data collection. The `inputs` directory will be created if not already present.
#' Preexisting files, if present, will not be overwritten. Also creates an
#' empty `results` subdirectory as a default location for evaluator output.
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
#' Given a input directory and a directory of simulation results, load all
#' of the key Evaluator data objects into memory.
#'
#' @importFrom dplyr summarize mutate_ group_by_
#' @importFrom readr read_csv
#' @param input_directory Location of input files.
#' @param results_directory Location of simulation results.
#' @return List of all key data objects.
#' @export
#' @examples
#' \dontrun{
#' load_data("~/evaluator/inputs", "~/evaluator/results")
#' }
load_data <- function(input_directory = "~/evaluator/inputs", results_directory = "~/evaluator/results") {
  # load simulation results and default summary objects
  simulation_results <- NULL # detailed sumulation results
  load(file.path(results_directory, "simulation_results.rda"))
  scenario_summary <- NULL   # scenario level summary
  load(file.path(results_directory, "scenario_summary.rda"))
  domain_summary <- NULL     # domain level summary
  load(file.path(results_directory, "domain_summary.rda"))

  # all input files
  domains <- readr::read_csv(file.path(input_directory, "domains.csv"))  # domain catalog
  mappings <- readr::read_csv(file.path(input_directory, "qualitative_mappings.csv"))  # qualitative translations
  capabilities <- readr::read_csv(file.path(input_directory, "capabilities.csv"))  # i.e. objectives & controls
  risk_tolerances <- readr::read_csv(file.path(input_directory, "risk_tolerances.csv"))  # i.e. risk tolerances
  qualitative_scenarios <- readr::read_csv(file.path(input_directory, "qualitative_scenarios.csv")) %>%
    dplyr::mutate_("tef" = ~ tolower(tef), "lm" = ~ tolower(lm), "tc" = ~ tolower(tc))

  # precalculate the standard order of scenarios (domain, then ID of the scenario)
  scenario_order <- dplyr::group_by_(simulation_results, "domain_id",
                                     "scenario_id") %>%
    dplyr::summarize()

  # build a vector of scenario outliers
  scenario_outliers <- scenario_summary[which(scenario_summary$outlier == TRUE),
                                        "scenario_id"] %>% unlist %>% unname

  # build a named vector of risk tolerance threshold
  risk_tolerance <- risk_tolerances$amount
  names(risk_tolerance) <- risk_tolerances$level %>% tolower

  # enhance scenario_summary assign loss tolerance to ALE VaR size
  scenario_summary <- mutate_(scenario_summary,
                              annual_tolerance = ~ ifelse(ale_var >= risk_tolerance["high"],
                                                          "High",
                                                          ifelse(ale_var >= risk_tolerance["medium"], "Medium", "Low"))) %>%
    mutate_(annual_tolerance = ~factor(annual_tolerance,
                                       levels = c("High", "Medium", "Low"),
                                       ordered = TRUE))

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
