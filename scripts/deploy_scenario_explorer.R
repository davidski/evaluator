library(rsconnect)
rsconnect::setAccountInfo(name = Sys.getenv("RSCONNECT_NAME"),
                          token = Sys.getenv("RSCONNECT_TOKEN"),
                          secret = Sys.getenv("RSCONNECT_SECRET"))

# prep the test data files
tmpdir <- tempdir()
tmpdata <- file.path(tmpdir, "data")
dir.create(tmpdata)
tmpinputs <- file.path(tmpdir, "inputs")
dir.create(tmpinputs, showWarnings = FALSE)

data("mc_simulation_results", package = "evaluator", envir = environment())
saveRDS(mc_simulation_results, file = file.path(tmpdata, "simulation_results.rds"))

res <- c("risk_tolerances.csv", "domains.csv") %>%
  purrr::map(~ file.copy(system.file("extdata", .x, package = "evaluator"),
                         tmpinputs))

data("mc_quantitative_scenarios", envir = environment())
saveRDS(mc_quantitative_scenarios, file.path(tmpinputs, "quantitative_scenarios.rds"))

deployApp(here::here("inst/explore_scenarios"), appName = "scenario_explorer",
          appTitle = "Scenario Explorer")
