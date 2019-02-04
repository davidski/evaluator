app <- ShinyDriver$new("../openfair_example.Rmd", seed = 96255, loadTimeout = 10000)
Sys.sleep(2) # let the app warm up

app$snapshotInit("openfair_example")

app$setInputs(tefml = 30)
app$setInputs(N = 100)
app$setInputs(tefl = 9)
app$setInputs(runmodel = "click")
app$snapshot()
