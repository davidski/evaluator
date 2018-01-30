context("Data Loads")
test_that("Template files can be copied", {
  res <- create_templates(tempdir())
  expect_equal(sum(res$copied), 5)
  unlink(file.path(tempdir(), "evaluator"), recursive = TRUE)
})
