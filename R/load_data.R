# ---- load_data ----
#' Load input and results files
#'
#' @import dplyr
#' @importFrom magrittr "%<>%"
#' @param input_directory Location of input files
#' @param results_directory Location of simulation results
#' @return List of data objects
#' @export
load_data <- function(input_directory, results_directory) {
  simulation_results <- NULL
  load(file = file.path(results_directory, "simulation_results.Rdata"))  # full results
  scenario_summary <- NULL
  load(file = file.path(results_directory, "scenario_summary.Rdata"))  # scenario level summary
  domain_summary <- NULL
  load(file = file.path(results_directory, "domain_summary.Rdata"))  # domain level summary

  # details on our scenarios
  domains <- readr::read_csv(file.path(input_directory, "domains.csv"))  # domain catalog
  mappings <- readr::read_csv(file.path(input_directory, "qualitative_mappings.csv"))  # qualitative translations
  capabilities <- readr::read_csv(file.path(input_directory, "capabilities.csv"))  # i.e. objectives & controls
  risk_tolerances <- readr::read_csv(file.path(input_directory, "risk_tolerances.csv"))  # i.e. risk tolerances
  scenarios <- readr::read_csv(file.path(input_directory, "scenarios.csv")) %>%
    mutate_("tef" = ~ tolower(tef), "lm" = ~ tolower(lm), "tc" = ~ tolower(tc))

  # Precalculate the standard order of scenarios (domain, then ID of the scenario)
  scenario_order <- group_by_(simulation_results, "domain_id", "scenario_id") %>%
    summarise()
  # store the `scenario_id` of outliers
  scenario_outliers <- scenario_summary[which(scenario_summary$outlier == TRUE), "scenario_id"] %>% unlist %>% unname
  # a text vector of numbers to english words
  numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  risk_tolerance <- risk_tolerances$amount
  names(risk_tolerance) <- risk_tolerances$level %>% tolower

  # enhance scenario_summary assign loss tolerance to ALE VaR size
  scenario_summary %<>% mutate_(annual_tolerance = ~ ifelse(ale_var >= risk_tolerance["high"],
                                                           "High",
                                                           ifelse(ale_var >= risk_tolerance["medium"],
                                                                  "Medium", "Low"))) %>%
    mutate_(annual_tolerance = ~factor(annual_tolerance, levels = c("High", "Medium", "Low"), ordered = TRUE))
  scenario_outliers <- scenario_summary[which(scenario_summary$outlier == TRUE), "scenario_id"] %>% unlist %>% unname
  list(simulation_results = simulation_results, scenario_summary = scenario_summary,
       domain_summary = domain_summary, domains = domains, mappings = mappings,
       capabilities = capabilities, risk_tolerances = risk_tolerances,
       risk_tolerance = risk_tolerance,
       scenario_outliers = scenario_outliers, scenarios = scenarios)
}
