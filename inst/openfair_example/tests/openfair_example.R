app <- ShinyDriver$new("../openfair_example.Rmd", seed = 96255, loadTimeout = 10000)
# for local testing
#app <- ShinyDriver$new("here::here("inst/openfair_example/openfair_example.Rmd", seed = 96255, loadTimeout = 10000)
Sys.sleep(2) # let the app warm up

app$snapshotInit("openfair_example")

Sys.sleep(0.5) # slow down input speed
app$setInputs(tefml = 30)

Sys.sleep(0.5) # slow down input speed
app$setInputs(N = 100)

Sys.sleep(0.5) # slow down input speed
app$setInputs(tefl = 9)

Sys.sleep(0.5) # slow down input speed
app$setInputs(runmodel = "click")
app$snapshot()
