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
#' @param styles Optional full path to CSS file to overide default styles.
#' @param focus_scenario_ids IDs of scenarios of special interest.
#' @param format Format to generate (html, pdf, word).
#' @param ... Any other parameters to pass straight to \code{rmarkdown::html_document}.
#' @return Default return values of the \code{rmarkdown::render} function.
#' @export
#' @examples
#' \dontrun{
#' generate_report("~/inputs", "~/results", "~/risk_report.html")
#' }
generate_report <- function(input_directory = "~/data",
                            results_directory = "~/results",
                            output_file,
                            styles = NULL,
                            focus_scenario_ids = c(51, 12),
                            format = "html",
                            ...) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Install the psych package to generate reports.")
  }
  if (!requireNamespace("pander", quietly = TRUE)) {
    stop("Install the pander package to generate reports.")
  }
  if (!requireNamespace("purrrlyr", quietly = TRUE)) {
    stop("Install the purrrlyr package to generate reports.")
  }
  if (!requireNamespace("ggalt", quietly = TRUE)) {
    stop("Install the ggalt package to generate reports.")
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install the rmarkdown package to generate reports.")
  }

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
                    params = list(input_directory = input_directory,
                                  results_directory = results_directory,
                                  focus_scenario_ids = focus_scenario_ids),
                    output_format = out_format)
}

#' Launch the Scenario Explorer web application
#'
#' Evaluator provides a simple Shiny-based web application for interactive
#' exploration of simulation results. This allows a user to interactively
#' review simluation output without generating an extensive report. For users
#' comfortable with R, working directly with the result dataframes will usually
#' be preferable, with the Explorer application provided as a bare-bones data
#' exploration tool.
#'
#' @param input_directory Location of input files.
#' @param results_directory Location of simulation results.
#' @param styles Optional full path to CSS file to overide default styles.
#' @import dplyr
#' @import ggplot2
#' @return Invisible NULL.
#' @export
#' @examples
#' \dontrun{
#' explore_scenarios("~/inputs", "~/results")
#' }
explore_scenarios <- function(input_directory = "~/data",
                              results_directory = "~/results",
                              styles = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install the rmarkdown package to generate reports.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the shiny package to run the Scenario Explorer.")
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Install the DT package to run the Scenario Explorer.")
  }
  if (!requireNamespace("statip", quietly = TRUE)) {
    stop("Install the statip package to run the Scenario Explorer.")
  }
  if (!requireNamespace("flexdashboard", quietly = TRUE)) {
    stop("Install the flexdashboard package to run the Scenario Explorer.")
  }

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
                   params = list(input_directory = input_directory,
                               results_directory = results_directory)))
  invisible(NULL)
}

#' Launch OpenFAIR demonstration web application
#'
#' A simple web application to demonstrate OpenFAIR modelling. This application
#' allows a user to enter beta PERT parameters and run simulations to see the
#' distribution of results, with high level summary statistics. As a demonstration
#' application, only TEF+TC+DIFF+LM parameters may be entered.
#'
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' openfair_example()
#' }
openfair_example <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install the rmarkdown package to run the OpenFAIR demonstration application.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Install the shiny package to run the OpenFAIR demonstration application.")
  }
  if (!requireNamespace("flexdashboard", quietly = TRUE)) {
    stop("Install the flexdashboard package to run the OpenFAIR demonstration application.")
  }

  styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  icon <- system.file("rmd", "img", "evaluator_logo_48px.png", package = "evaluator")

  rmarkdown::run(system.file("rmd", "openfair_example.Rmd",
                             package = "evaluator"),
                 render_args = list(output_options =  list(css = styles,
                                                           favicon = icon,
                                                           logo = icon)
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
#' @param ... Any other parameters to pass straight to \code{rmarkdown::render}
#' @return Default return values of the \code{rmarkdown::render} function.
#' @export
#' @examples
#' \dontrun{
#' risk_dashboard("~/inputs", "~/simulations")
#' }
risk_dashboard <- function(input_directory = "~/data",
                           results_directory = "~/results",
                           output_file,
                           ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Install the rmarkdown package to generate the risk dashboard.")
  }
  if (!requireNamespace("flexdashboard", quietly = TRUE)) {
    stop("Install the flexdashboard to generate the risk dashboard.")
  }
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Install the forcats package to generate the risk dashboard.")
  }
  styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
  icon <- system.file("rmd", "img", "evaluator_logo_48px.png", package = "evaluator")

  rmarkdown::render(system.file("rmd", "risk_dashboard.Rmd", package = "evaluator"),
                    output_options =  list(css = styles, favicon = icon, logo = icon),
                    output_file = output_file,
                    params = list(input_directory = input_directory,
                                  results_directory = results_directory),
                      ...)
}
