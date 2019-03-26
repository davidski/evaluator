library(rmarkdown)

# Heroku-assigned port to bind
port <- Sys.getenv('PORT')

# Heroku app mount point
setwd("/app")

# run Scenario Explorer
rmarkdown::run("explore_scenarios.Rmd",
               shiny_args = list(
                 host = '0.0.0.0',
                 port = as.numeric(port)
               )
)
