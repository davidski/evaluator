# ---- domain_heatmap ---

dat <- domain_impact %>% mutate(full_label = paste0(domain_id, "\n", "$", 
                                                    round(var / 10^6), "M"))
gg <- ggplot(dat, aes(x = domain, y = 1, fill = var)) 
gg <- gg + geom_tile(color="white")
gg <- gg + geom_text(aes(x = domain, y = 1, label = full_label), 
                     color = "white", size = 3)
gg <- gg + coord_equal()
gg <- gg + scale_fill_viridis(guide = guide_legend(title=NULL), 
                              labels = dollar_millions)
gg <- gg + labs(x = NULL, y=NULL,
                title = "Value at Risk by Domain",
                caption = "Source: Evaluator toolkit") 
gg <- gg + theme_evaluator() 
gg <- gg + theme(axis.text = element_blank(), panel.grid = element_blank())
gg