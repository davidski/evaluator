#' \code{evaluator} package
#'
#' Quantified Information Risk Simulation Toolkit
#'
#' See the online documentation located at
#' \href{https://evaluator.tidyrisk.org/}{https://evaluator.tidyrisk.org/}
#'
#' @docType package
#' @name evaluator
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## technique from Jenny Bryan's googlesheet package
utils::globalVariables(c("."))
