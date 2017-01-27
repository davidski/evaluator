# ---- domain_heatmap ---

dat <- domain_impact %>% 
  arrange(domain_id) %>% 
  mutate(full_label = paste0(domain_id, "\n", "$", round(var / 10^6), "M"))
dat$aux <- seq(1, 2*nrow(dat), by = 2)
gg <- ggplot(dat, aes(x = aux, y = 1)) 
gg <- gg + geom_tile(stat = "identity", color="white", aes(fill = var), 
                     width = 1)
gg <- gg + geom_text(aes(x = aux, y = 1, label = full_label), 
                     color = "white", size = 3)
gg <- gg + coord_equal()
gg <- gg + scale_fill_viridis(guide = FALSE)
gg <- gg + labs(x = NULL, y = NULL,
                title = "Value at Risk by Domain",
                caption = "Source: Evaluator toolkit") 
gg <- gg + theme_evaluator() 
gg <- gg + theme(axis.text = element_blank())
gg <- gg + theme(panel.grid = element_blank())
gg
