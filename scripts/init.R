# init.R
#
# Ensure packages are available for running Scenario Explorer on Heroku
#

my_packages = c("flexdashboard", "ggplot2", "extrafont", "scales", "viridis",
                "stringi", "dplyr", "readr", "tidyr", "modeest", "shiny", "DT")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
