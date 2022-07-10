
<!-- README.md is generated from README.Rmd. Please edit that file -->

# evaluator <a href="https://evaluator.tidyrisk.org"><img alt="evaluator Logo" title="evaluator" align="right" src="man/figures/logo.png" height="139"/></a>

<!-- badges: start -->

[![R build
status](https://github.com/davidski/evaluator/workflows/R-CMD-check/badge.svg)](https://github.com/davidski/evaluator/actions)
[![Coverage
Status](https://codecov.io/gh/davidski/evaluator/branch/master/graph/badge.svg)](https://codecov.io/github/davidski/evaluator?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/evaluator)](https://cran.r-project.org/package=evaluator)
![downloads](https://cranlogs.r-pkg.org/badges/grand-total/evaluator)
<!-- badges: end -->

## Overview

evaluator is an open source quantitative risk analysis toolkit. Based on
the OpenFAIR [ontology](https://publications.opengroup.org/c20b) and
risk analysis [standard](https://publications.opengroup.org/c20a),
evaluator empowers an organization to perform a quantifiable,
repeatable, and data-driven risk review.

Three sample outputs of this toolkit are available:

1.  A [sample risk
    analysis](https://evaluator.tidyrisk.org/reports/evaluator_risk_analysis.html)
    report
2.  A one page [risk
    dashboard](https://evaluator.tidyrisk.org/reports/evaluator_risk_dashboard.html)
3.  A demonstration copy of [Scenario
    Explorer](https://davidski.shinyapps.io/scenario_explorer)

## Installation

Install evaluator via the standard CRAN mechanisms. If you wish to use
the optional, but recommended, reporting functions, also install the
suggested dependencies. These additional packages are not needed for
modeling, but are used in the generation of reports.

``` r
install.packages("evaluator", dependencies = TRUE)
```

If you wish to run the development (and potentially bleeding edge)
version of evaluator, you can install directly from GitHub with the
following `devtools` command.

``` r
# install.packages("devtools")
devtools::install_github("davidski/evaluator", dependencies = TRUE)
```

Optionally, Docker images with all dependencies pre-installed are
available on the [Docker Hub](https://hub.docker.com/r/tidyrisk).

## Usage

The primary workflow for evaluator involves gathering data in Excel then
running the analysis from within the R and Evaluator environment:

From Excel:

1.  Populate the evaluator-supplied data acquisition spreadsheet

From Evaluator:

2.  Import the data
3.  Prepare the data for simulation
4.  Run the simulations
5.  Summarize the results
6.  Generate draft reports for customization

A detailed guide is available in the vignette accessed via
`vignette("usage", package="evaluator")`. A short screencast showing the
basic workflow (not including generation of reports) is available below:

[![demo](https://asciinema.org/a/qIBU3lhPkWHGMYD9O2GU1YgcU.png)](https://asciinema.org/a/qIBU3lhPkWHGMYD9O2GU1YgcU?s=2&autoplay=1)

## Where to Go from Here

While evaluator is a powerful tool, it does not attempt to address
interactions between risk scenarios, rolling up multiple levels of risk
into aggregations, or other advanced topics. As you become more
comfortable with quantitative risk analysis, you may wish to dive deeper
into these areas (and I hope you do!). The following resources may help
you explore these and other topics in risk management.

### Alternative Software

-   [RiskLens](https://www.risklens.com), a commercial analysis suite,
    founded by the original creator of the FAIR methodology
-   [FAIR Tool](https://github.com/zugo01/FAIRTool), a Shiny and R based
    two scenario simulator, authored by Ezeugo Aguta under an MIT
    license
-   [FAIR-U](https://www.fairinstitute.org/fair-u), a free educational
    tool for learning FAIR analysis, powered by RiskLens
-   [Open FAIR Risk Analysis
    Tool](https://publications.opengroup.org/i181), an Excel and SIPMath
    base tool with a limited open license

### Blogs/Books/Training

-   Russell C. Thomas’s excellent and provocative blog post on systemic
    [Risk
    Management](https://exploringpossibilityspace.blogspot.com/2013/08/risk-management-out-with-old-in-with-new.html)
-   [Measuring and Managing Information
    Risk](https://smile.amazon.com/gp/product/0124202314)
-   [OpenFAIR
    certification](https://www.opengroup.org/certifications/openfair)
-   [Hubbard Decision Research calibration
    training](https://hubbardresearch.com/training/)

### Associations

-   [FAIR Institute](https://www.fairinstitute.org)
-   [Society of Information Risk Analysts
    (SIRA)](https://www.societyinforisk.org/)

## Contributing

This project is governed by a [Code of
Conduct](https://evaluator.tidyrisk.org/CODE_OF_CONDUCT.html). By
participating in this project you agree to abide by these terms.

## License

The [MIT License](LICENSE) applies.
