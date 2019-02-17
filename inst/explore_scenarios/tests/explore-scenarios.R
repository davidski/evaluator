app <- ShinyDriver$new("../explore_scenarios.Rmd", seed = 7767)
#app <- ShinyDriver$new(here::here("inst/explore_scenarios/explore_scenarios.Rmd", seed = 7767)
app$snapshotInit("explore-scenarios")

app$snapshot()
