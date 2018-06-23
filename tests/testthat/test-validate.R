context("Validation")
qualitative_scenarios <- data.frame(scenario_id = c("1", "2"),
                                    scenario = c("Scenario A.", "Scenario B."),
                                    tcomm = c("Organizational Leadership", "Organizational Leadership"),
                                    tef = c("frequent", "frequent"),
                                    tc = c("medium", "medium"),
                                    lm = c("medium", "medium"),
                                    domain_id = c("ORG", "ORG"),
                                    controls = c("1, 5, 7, 32, 14",
                                                 "14, 15, 16"),
                                    stringsAsFactors = FALSE)
capabilities <- data.frame(capability_id = c("1", "5", "7", "32", "14", "15", "16"),
                           domain_id = c("ORG", "ORG", "ORG", "ORG", "ORG", "ORG", "ORG"),
                           capability = c("Capability 1.",  "Capability 5.", "Capability 7", "Capability 32.", "Capability 14.", "Capability 15.", "Capability 16."),
                           diff = c("5 - Optimized", "4 - Managed", "1 - Initial", "4 - Managed", "4 - Managed", "2 - Repeatable", "2 - Repeatable"),
                           stringsAsFactors = FALSE)
mappings <- data.frame(type = c("tef", "tef", "tef", "tc", "tc", "tc", "diff", "diff", "diff", "diff", "diff", "lm", "lm", "lm"),
                       label = c("frequent", "occasional", "rare", "high", "medium", "low", "5 - Optimized", "4 - Managed", "3 - Defined", "2 - Repeatable", "1 - Initial", "high", "medium", "low"),
                       l = c(10L, 1L, 0L, 50L, 33L, 0L, 70L, 50L, 33L, 20L, 0L, 1000000L, 10000L, 100L),
                       ml = c(24, 6, 0.1, 75, 50, 16, 85, 70, 50, 30, 10, 2e+06, 20000, 200),
                       h = c(52L, 12L, 1L, 98L, 60L, 30L, 98L, 84L, 60L, 50L, 30L, 5000000L, 500000L, 10000L),
                       conf = c(4L, 4L, 4L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 1L),
                       stringsAsFactors = FALSE)

test_that("Duplicate scenarios detected", {
  qualitative_scenarios <- data.frame(scenario_id = c("1", "1"),
                                      scenario = c("Scenario A.", "Scenario B."),
                                      tcomm = c("Organizational Leadership", "Organizational Leadership"),
                                      tef = c("frequent", "frequent"),
                                      tc = c("medium", "medium"),
                                      lm = c("medium", "medium"),
                                      domain_id = c("ORG", "ORG"),
                                      controls = c("1, 5, 7, 32, 14",
                                                   "14, 15, 16"),
                                      stringsAsFactors = FALSE)
  expect_warning(validate_scenarios(qualitative_scenarios, capabilities, domains, mappings),
                 regexp = "Duplicate scenarios")
})
test_that("Duplicate capabilities detected", {
  capabilities <- data.frame(capability_id = c("1", "5", "5", "32", "14", "15", "16"),
                             domain_id = c("ORG", "ORG", "ORG", "ORG", "ORG", "ORG", "ORG"),
                             capability = c("Capability 1.",  "Capability 5.", "Capability 7", "Capability 32.", "Capability 14.", "Capability 15.", "Capability 16."),
                             diff = c("5 - Optimized", "4 - Managed", "1 - Initial", "4 - Managed", "4 - Managed", "2 - Repeatable", "2 - Repeatable"),
                             stringsAsFactors = FALSE)
  expect_warning(validate_scenarios(qualitative_scenarios, capabilities, domains, mappings),
                 regexp = "Duplicate capabilities")
})


test_that("Invalid TEF labels are handled", {
  data(capabilities, qualitative_scenarios)
  bad_scenarios <- qualitative_scenarios
  bad_scenarios[1, "tef"] = "invalid"
  expect_warning(validate_scenarios(bad_scenarios, capabilities, domains, mappings),
               regexp = "qualitative TEF")

})

test_that("Invalid TC labels are handled", {
  data(capabilities, qualitative_scenarios)
  bad_scenarios <- qualitative_scenarios
  bad_scenarios[1, "tc"] = "invalid"
  expect_warning(validate_scenarios(bad_scenarios, capabilities, domains, mappings),
               regexp = "qualitative TC")

})

test_that("Invalid DIFF labels are handled", {
  data(capabilities, qualitative_scenarios)
  bad_capabilities <- capabilities
  bad_capabilities[1, "diff"] = "invalid"
  expect_warning(validate_scenarios(qualitative_scenarios, bad_capabilities, domains, mappings),
               regexp = "qualitative DIFF")

})

test_that("Invalid LMs labels are handled", {
  data(capabilities, qualitative_scenarios)
  bad_scenarios <- qualitative_scenarios
  bad_scenarios[1, "lm"] = "invalid"
  expect_warning(validate_scenarios(bad_scenarios, capabilities, domains, mappings),
               regexp = "qualitative LM")

})
