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
#' load_data("~/input_files", "~/simulation_results")
#' }
load_data <- function(input_directory = "~/data", results_directory = "~/data") {
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
