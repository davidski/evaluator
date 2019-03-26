app <- ShinyDriver$new("../openfair_example.Rmd", seed = 95592)
app$snapshotInit("openfair_example")

app$setInputs(tefml = character(0))
app$setInputs(tefml = 30)
app$setInputs(iterations = 100)
app$setInputs(runmodel = "click")
app$snapshot()
