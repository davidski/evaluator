
<!-- README.md is generated from README.Rmd. Please edit that file -->
Evaluator <img alt="Evaluator Logo" title="Evaluator" align="right" src="inst/rmd/img/evaluator_logo.jpg" width="100" style="float:right;width:100px;"/>
========================================================================================================================================================

[![Build Status](https://travis-ci.org/davidski/evaluator.svg?branch=master)](https://travis-ci.org/davidski/evaluator) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/davidski/evaluator?branch=master&svg=true)](https://ci.appveyor.com/project/davidski/evaluator) [![Coverage Status](https://codecov.io/gh/davidski/evaluator/branch/master/graph/badge.svg)](https://codecov.io/github/davidski/evaluator?branch=master) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/evaluator)](https://cran.r-project.org/package=evaluator) ![downloads](https://cranlogs.r-pkg.org/badges/grand-total/evaluator)

Overview
--------

Evaluator is an open source quantitative risk analysis toolkit. Based on the OpenFAIR [taxonomy](https://www2.opengroup.org/ogsys/catalog/C13K) and risk assessment [standard](https://www2.opengroup.org/ogsys/catalog/C13G), Evaluator empowers an organization to perform a quantifiable, repeatable, and data-driven risk review.

Three sample outputs of this toolkit are available:

1.  A detailed risk analysis template, located at [RPubs](https://rpubs.com/davidski/evaluator_risk_analysis)
2.  A one page risk dashboard, also located at [RPubs](https://rpubs.com/davidski/evaluator_risk_dashboard)
3.  A demonstration copy of [Scenario Explorer](https://davidski.shinyapps.io/scenario_explorer)

Installation
------------

``` r
install.packages("evaluator")

# Optionally, to install the development verison from GitHub
# install.pacakges("devtools")
devtools::install_github("davidski/evaluator")
```

Optionally, a prototype Docker image is available on the [Docker Hub](https://hub.docker.com/r/davidski/evaluator-docker/).

Usage
-----

The primary workflow for Evaluator involves gathering data in Excel then running the analysis from within the R and Evaluator environment:

From Excel:

1.  Populate the Evaluator-supplied data acquisition spreadsheet

From Evaluator:

1.  Import the data
2.  Prepare the data for simulation
3.  Run the simulations
4.  Summarize the results
5.  Generate draft reports for customization

A detailed guide is available in the vignette accessed via `vignette("usage", package="evaluator")`. A short screencast showing the basic workflow (not including generation of reports) is available below:

[![demo](https://asciinema.org/a/qIBU3lhPkWHGMYD9O2GU1YgcU.png)](https://asciinema.org/a/qIBU3lhPkWHGMYD9O2GU1YgcU?s=2&autoplay=1)

Where to Go from Here
---------------------

While Evaluator is a powerful tool, it does not attempt to address interactions between risk scenarios, rolling up multiple levels of risk into aggregations, or other advanced topics. As you become more comfortable with quantitative risk analysis, you may wish to dive deeper into these areas (and I hope you do!). The following resources may help you explore these and other topics in risk management.

### Commercial Software

-   [RiskLens](http://www.risklens.com/), founded by the original creator of the FAIR methodology

### Blogs/Books/Training

-   Russell C. Thomas's excellent and provocative blog post on systemic [Risk Management](http://exploringpossibilityspace.blogspot.com/2013/08/risk-management-out-with-old-in-with-new.html)
-   [Measuring and Managing Information Risk](https://smile.amazon.com/gp/product/0124202314)
-   [OpenFAIR certification](http://www.opengroup.org/certifications/openfair)
-   [Hubbard Decision Research calibration training](https://www.hubbardresearch.com/training/)

### Associations

-   [FAIR Institute](http://www.fairinstitute.org/)
-   [Society of Information Risk Analysts (SIRA)](https://www.societyinforisk.org/)

Contributing
------------

This project is governed by a [Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by these terms.

License
-------

The [MIT License](LICENSE) applies.
