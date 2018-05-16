#' \code{evaluator} package
#'
#' Quantified Information Risk Simulation Toolkit
#'
#' See the README on
#' \href{https://cran.r-project.org/package=evaluator/README.html}{CRAN}
#' or \href{https://github.com/davidski/evaluator}{GitHub}
#'
#' @docType package
#' @name evaluator
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## technique from Jenny Bryan's googlesheet package
utils::globalVariables(c("."))
