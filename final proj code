dataF1 <- dataF %>% filter(year >= 2000 & year <= 2010)
gdp_plot1 <- ggplot(dataF1, aes(x = gdp, y = reorder(country, -rank), fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 20 Countries by GDP: 2000 - 2010", y = "Country", x = "GDP (in billions of USD)") +
  geom_text(aes(x = gdp, y = country, label = Value_lbl, hjust = 1), size = 1) +
  facet_wrap(~ year, ncol = 6, scales = "free_y") +
  theme_modern_rc() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust = 0),
        axis.title.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"))

# Display the plot
gdp_plot1 

dataF2 <- dataF %>% filter(year >= 2011 & year <= 2020)
gdp_plot2 <- ggplot(dataF2, aes(x = gdp, y = reorder(country, -rank), fill = country)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 20 Countries by GDP Per Capita: 2011 - 2020", y = "Country", x = "GDP Per Capita (in billions of USD)") +
  geom_text(aes(x = gdp, y = country, label = Value_lbl, hjust = 1), size = 1) +
  facet_wrap(~ year, ncol = 5, scales = "free_y") +
  theme_modern_rc() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(hjust = 0),
        axis.title.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "white")) 
        
# Display the plot
gdp_plot2

dataF <- dataF %>% filter(year >= 2000 & year <= 2020)
dataF$gdp %>% range() -> min_max
dataF %>%
  ggplot(aes(rank, group = country, fill = country, color = country)) +
  geom_tile(aes(y = gdp/2, height = gdp, width = 0.75), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size = 4.5) +
  geom_text(aes(y = gdp, label = Value_lbl, hjust = 0, size = 4, fontface = "bold")) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  labs(x = NULL) +
  labs(y = NULL) +
  guides(color = FALSE, fill = FALSE) +
  theme_modern_rc() + 
  theme(panel.grid = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(axis.title = element_blank()) + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(size = 23)) + 
  theme(plot.caption = element_text(color = "white", size = 14)) + 
  theme(plot.subtitle = element_text(color = "white", size = 16)) + 
  theme(plot.margin = unit(c(1, 1, 1, 3.5), "cm")) -> plots
  
animate(plots, nframes = 200, fps = 25)
