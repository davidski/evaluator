context("Tidyrisk Factor class")

test_that("Element object can be created", {
  element <- tidyrisk_factor(NA, factor_label = "TF")
  expect_s3_class(element, "tidyrisk_factor")

})
test_that("Element object does not accept invalid OpenFAIR types", {
  expect_error(tidyrisk_factor(NA, factor_label = "ZZGO"))
})

test_that("Element object summary functions without samples", {
  element <- tidyrisk_factor(NA, factor_label = "LM")
  expect_is(summary(element), "list")
})
test_that("Element object summary functions with samples", {
  element <- tidyrisk_factor(c(1, 100), factor_label = "LM")
  expect_is(summary(element), "list")
})
test_that("Element object summary functions on non LM types", {
  element <- tidyrisk_factor(c(1, 100), factor_label = "TC")
  expect_is(summary(element), "list")
})

test_that("Factors can be logically compared", {
  tc_element <- tidyrisk_factor(c(10, 40, 100), factor_label = "TC")
  diff_element <- tidyrisk_factor(c(10, 30, 100), factor_label = "DIFF")
})
