#' Generate sample analysis report
#'
#' Given a set of input files and summarized simulation results, create a
#' skeleton risk analysis report. This report attempts to summarize the results
#' of the analysis at a top level, using 95% Value at Risk (VaR) as the primary
#' metric, while also providing more detailed analysis at both a per-domain and
#' per-scenario level.
#'
#' This report includes several sections where an analyst will need to modify and
#' fill in details for their specific organization. Of particular note is the
#' Recommendations section, which will always need to be updated.
#'
#' @importFrom dplyr case_when
#' @param input_directory Location of input files.
#' @param results_directory Location of simulation results.
#' @param output_file Full path to output file.
#' @param styles Optional full path to CSS file to override default styles.
#' @param focus_scenario_ids IDs of scenarios of special interest.
#' @param format Format to generate (html, pdf, word).
#' @param intermediates_dir Location for intermediate knit files.
#' @param quiet TRUE to suppress printing of pandoc output.
#' @param ... Any other parameters to pass straight to \code{rmarkdown::render}.
#' @return Default return values of the \code{rmarkdown::render} function.
#' @export
#' @examples
#' \dontrun{
#' generate_report("~/inputs", "~/results", "~/risk_report.html")
#' }
generate_report <- function(input_directory = "~/evaluator/inputs",
                            results_directory = "~/evaluator/results",
                            output_file,
                            styles = NULL,
                            focus_scenario_ids = c(51, 12),
                            format = "html",
                            intermediates_dir = tempdir(),
                            quiet = TRUE,
                            ...) {
  check_availability(packages = c("psych", "pander", "purrrlyr", "ggalt", "rmarkdown"),
                     func = "generate_report")

  # figure out the correct style format to apply
  styles <- if (is.null(styles)) {
    if (format == "html") system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  } else {styles}

  # select the appropriate renderer
  out_format <- rmarkdown::html_document(css = styles)
  out_format <- if (format == "pdf") {
    rmarkdown::pdf_document()} else {
      if (format == "word") {
        rmarkdown::word_document(reference_docx = styles) } else {
          out_format } }

  rmarkdown::render(system.file("rmd", "analyze_risk.Rmd", package = "evaluator"),
                    output_file = output_file,
                    intermediates_dir = intermediates_dir,
                    params = list(input_directory = input_directory,
                                  results_directory = results_directory,
                                  focus_scenario_ids = focus_scenario_ids),
                    output_format = out_format,
                    quiet = quiet,
                    ...)
}

#' Launch the Scenario Explorer web application
#'
#' Evaluator provides a simple Shiny-based web application for interactive
#' exploration of simulation results. This allows a user to interactively
#' review simulation output without generating an extensive report. For users
#' comfortable with R, working directly with the result dataframes will usually
#' be preferable, with the Explorer application provided as a bare-bones data
#' exploration tool.
#'
#' @param input_directory Location of input files.
#' @param results_directory Location of simulation results.
#' @param styles Optional full path to CSS file to override default styles.
#' @param intermediates_dir Location for intermediate knit files.
#' @param quiet TRUE to suppress printing of pandoc output.
#' @param ... Any other parameters to pass straight to \code{rmarkdown::run}.
#' @import dplyr
#' @import ggplot2
#' @return Invisible NULL.
#' @export
#' @examples
#' \dontrun{
#' explore_scenarios("~/inputs", "~/results")
#' }
explore_scenarios <- function(input_directory = "~/evaluator/inputs",
                              results_directory = "~/evaluator/results",
                              styles = NULL,
                              intermediates_dir = tempdir(),
                              quiet = TRUE,
                              ...) {
  check_availability(c("rmarkdown", "shiny", "DT", "statip", "flexdashboard"),
                     func = "explore_scenarios")

  # figure out the correct style format to apply
  if (is.null(styles)) {
    styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  }
  icon <- system.file("rmd", "img", "evaluator_logo_48px.png", package = "evaluator")

  rmarkdown::run(system.file("rmd", "explore_scenarios.Rmd", package = "evaluator"),
                 #dir = file.path(basename(system.file("rmd", "explore_scenarios.Rmd", package = "evaluator")), ".."),
                 render_args = list(
                   output_options =  list(css = styles, favicon = icon,
                                          logo = icon),
                   intermediates_dir = intermediates_dir,
                   params = list(input_directory = input_directory,
                                 results_directory = results_directory),
                   quiet = quiet),
                 ...)
  invisible(NULL)
}

#' Launch OpenFAIR demonstration web application
#'
#' A simple web application to demonstrate OpenFAIR modeling. This application
#' allows a user to enter beta PERT parameters and run simulations to see the
#' distribution of results, with high level summary statistics. As a demonstration
#' application, only TEF+TC+DIFF+LM parameters may be entered.
#'
#' @param intermediates_dir Location for intermediate knit files.
#' @param quiet TRUE to suppress printing of pandoc output.
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' openfair_example()
#' }
openfair_example <- function(intermediates_dir = tempdir(),
                             quiet = TRUE) {
  check_availability(c("rmarkdown", "shiny", "flexdashboard"),
                     func = "openfair_example")

  styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  icon <- system.file("rmd", "img", "evaluator_logo_48px.png", package = "evaluator")

  rmarkdown::run(system.file("rmd", "openfair_example.Rmd",
                             package = "evaluator"),
                 render_args = list(output_options =  list(css = styles,
                                                           favicon = icon,
                                                           logo = icon),
                                    intermediates_dir = intermediates_dir,
                                    quiet = quiet
                                    )
                 )
  invisible(NULL)
}

#' Launch a single page summary risk dashboard
#'
#' Given the input files and the analysis summary file, create a basic one-
#' page summary with an overview of the results per domain and scenario.
#' Intended as a skeleton showing how the results could be displayed at an
#' executive level.
#'
#' @param input_directory Location of input files
#' @param results_directory Location of simulation results
#' @param output_file Full path to the desired output file.
#' @param intermediates_dir Location for intermediate knit files.
#' @param quiet TRUE to suppress printing of pandoc output.
#' @param ... Any other parameters to pass straight to \code{rmarkdown::render}
#' @return Default return values of the \code{rmarkdown::render} function.
#' @export
#' @examples
#' \dontrun{
#' risk_dashboard("~/inputs", "~/simulations")
#' }
risk_dashboard <- function(input_directory = "~/evaluator/inputs",
                           results_directory = "~/evaluator/results",
                           output_file,
                           intermediates_dir = tempdir(),
                           quiet = TRUE,
                           ...) {
  check_availability(c("rmarkdown", "shiny", "flexdashboard", "forcats"),
                     func = "risk_dashboard")

  styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  icon <- system.file("rmd", "img", "evaluator_logo_48px.png", package = "evaluator")

  rmarkdown::render(system.file("rmd", "risk_dashboard.Rmd", package = "evaluator"),
                    output_options =  list(css = styles, favicon = icon, logo = icon),
                    output_file = output_file,
                    intermediates_dir = intermediates_dir,
                    params = list(input_directory = input_directory,
                                  results_directory = results_directory),
                    quiet = quiet,
                    ...)
}
