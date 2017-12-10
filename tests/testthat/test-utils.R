context("Utilities")
test_that("Dollar Millions formats as expected", {
  expect_equal(dollar_millions(1.523 * 10^6), "$1.52M")
})
