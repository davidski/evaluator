context("Encodings")
test_that("Scenario Encoding", {
  qualitative_scenarios <- data.frame(scenario_id = 1:2,
                                      scenario = c("Scenario A.", "Scenario B."),
                                      tcomm = c("Organizational Leadership", "Organizational Leadership"),
                                      tef = c("frequent", "frequent"),
                                      tc = c("medium", "medium"),
                                      lm = c("medium", "medium"),
                                      domain_id = c("ORG", "ORG"),
                                      controls = c("1, 5, 7, 32, 14",
                                                   "14, 15, 16"),
                                      stringsAsFactors = FALSE)
  capabilities <- data.frame(capability_id = c(1L, 5L, 7L, 32L, 14L, 15L, 16L),
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
  dat <- encode_scenarios(qualitative_scenarios, capabilities, mappings)
})
test_that("Control Encoding", {
  capability_ids <- "1, 7"
  capabilities <- data.frame(capability_id = c(1L, 5L, 7L, 32L, 14L, 15L, 16L),
                             domain_id = c("ORG", "ORG", "ORG", "ORG", "ORG", "ORG", "ORG"),
                             capability = c("Capability 1.",  "Capability 5.", "Capability 7.", "Capability 32.", "Capability 14.", "Capability 15.", "Capability 16."),
                             diff = c("5 - Optimized", "4 - Managed", "1 - Initial", "4 - Managed", "4 - Managed", "2 - Repeatable", "2 - Repeatable"),
                             stringsAsFactors = FALSE)
  mappings <- data.frame(type = c("diff", "diff"), label = c("5 - Optimized", "1 - Initial"),
                         l = c(70, 0), ml = c(80, 20), h = c(95, 30),
                         conf = 3, stringsAsFactors = FALSE)
  dat <- derive_controls(capability_ids, capabilities, mappings)
  expect_equal(length(dat), 2)
})
