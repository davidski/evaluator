windowsFonts(Verdana = "TT Verdana")
windowsFonts(Vladimir = "TT Vladimir Script")
windowsFonts(Impact = "TT Impact")
windowsFonts(Georgia = "TT Georgia")
windowsFonts(Futura = "TT Futura Std Book")
windowsFonts(Fette = "Fette Fraktur LT Std")
windowsFonts(Calibri = "TT Calibri Regular")
fontname <- "Calibri"

# HIMSS Graphic -----------------------------------------------------------

gg <- ggplot(results, aes(x = as.character(scenario_id), y = ale))
gg <- gg + scale_y_continuous(label = dollar)
gg <- gg + labs(x = "Risk Scenario", y = "Annual Loss")
gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.5)
gg <- gg + geom_boxplot(fill = "steelblue", alpha = 1/3,
                        outlier.color = alpha("black", 1/3))

gg <- gg + theme_minimal(base_family = fontname)
gg <- gg + theme(panel.grid.major = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())

gg <- gg + theme(axis.text.y = element_blank(),
                 axis.text.x = element_blank())
gg <- gg + labs(x = element_blank(), y = element_blank())
gg <- gg + theme(axis.title.y = element_text(angle = 0)) +
  ylab("Annual\nLoss")
gg <- gg + ggtitle("Risk Exposure Across 53 Scenarios")

#ggsave("scenarios.png", width = 9.71, height = 3.88, units = "in")
gg

# Capabilities Graphic ----------------------------------------------------

gg <- ggplot(capabilities, aes(y = diff, x = domain_id))
gg <- gg + scale_y_continuous(limits = c(0, 5))
gg + geom_point()

# Control Gaps ------------------------------------------------------------

gg <- ggplot(results, aes(y = mean_tc_exceedance, x = domain_id))
gg <- gg + theme_minimal()
gg + geom_boxplot()

# Loss Events -------------------------------------------------------------

gg <- ggplot(results, aes(y = loss_events, x = domain_id))
gg <- gg + theme_minimal()
gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.5)
gg + geom_boxplot()

# Big Losses --------------------------------------------------------------

filter(results, ale > 10 ^ 6) %>% group_by(scenario_id) %>% 
  summarize(mean_ale = mean(ale)) %>% ungroup() -> big_loss_scenarios

left_join(big_loss_scenarios, scenarios) %>% arrange(desc(mean_ale)) %>% 
  select(scenario_id, domain_id, scenario, mean_ale) %>% View()

# Live Risk Exposures -----------------------------------------------------

r2 <- results %>%
  mutate(scenario_id = factor(as.character(scenario_id), 
                              levels = scenario_order$scenario_id)) %>% 
  arrange(domain_id, scenario_id)
gg <- ggplot(r2, aes(x = scenario_id, y = ale))
gg <- gg + scale_y_continuous(label = dollar)
gg <- gg + labs(x = "Risk Scenario", y = "Annual Loss")
gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.5)
gg <- gg + geom_boxplot(fill = "steelblue", alpha = 1/3,
                        outlier.color = alpha("black", 1/3))

gg <- gg + theme_minimal(base_family = fontname)
gg <- gg + theme(panel.grid.major = element_blank())
gg <- gg + theme(panel.grid.minor = element_blank())

gg <- gg + theme(axis.text.x = element_blank())
gg <- gg + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
gg <- gg + labs(x = element_blank(), y = element_blank())
#gg <- gg + theme(axis.title.y = element_text(angle=0)) + ylab("Annual\nLoss")
gg <- gg + ggtitle("Annual Loss Exposure Across All Risk Scenarios")

ggsave("scenarios.png", width = 12, height = 2.5, units = "in")

gg

rm(r2)
