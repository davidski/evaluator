context("Tidyrisk Scenario class")
test_that("Scenario object can be created", {
  tidyrisk_scenario(
    diff_params = list(list(
      "2"  = list(min = 70L, mode = 85, max = 98L, shape = 4L, func = "mc2d::rpert"),
      "5"  = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
      "7"  = list(min = 20L, mode = 30, max = 50L, shape = 4L, func = "mc2d::rpert"),
      "32" = list(min = 20L, mode = 30, max = 50L, shape = 4L, func = "mc2d::rpert"),
      "14" = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
      "15" = list(min = 50L, mode = 70, max = 84L, shape = 4L, func = "mc2d::rpert"),
      "16" = list(min = 0L,  mode = 10, max = 30L, shape = 4L, func = "mc2d::rpert")
    )),
    tef_params = list(list(min = 10L, mode = 24, max = 52L, shape = 4L, func = "mc2d::rpert")),
    tc_params = list(list(min = 33L, mode = 50, max = 60L, shape = 3L, func = "mc2d::rpert")),
    lm_params = list(list(min = 10000L, mode = 20000, max = 500000L, shape = 4L,
                          func = "mc2d::rpert"))
  ) -> scenario
  expect_s3_class(scenario, "tidyrisk_scenario")
})

