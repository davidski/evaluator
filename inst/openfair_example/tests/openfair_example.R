app <- ShinyDriver$new("../openfair_example.Rmd", seed = 96255)
app$snapshotInit("openfair_example")

app$setInputs(tefml = 30)
app$setInputs(N = 100)
app$setInputs(tefl = 9)
app$setInputs(runmodel = "click")
app$snapshot()
