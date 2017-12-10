#' \code{evaluator} package
#'
#' Information Security Risk Simulation Toolkit
#'
#' See the README on
#' [CRAN](https://cran.r-project.org/package=evaluator/README.html)
#' or [GitHub](https://github.com/davidski/evaluator)
#'
#' @seealso
#' Useful links:
#' * [FAIR Institute](http://www.fairinstitute.org/)
#' * Report bugs at [GitHub](https://github.com/davidski/evaluator/issues)
#' @docType package
#' @name evaluator
#' @importFrom dplyr %>%
#' @importFrom utils globalVariables
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## technique from Jenny Bryan's googlesheet package
if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))
