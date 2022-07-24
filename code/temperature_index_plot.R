library(tidyverse)

# read data, set NA, and skip first line
read_csv("data/GLB.Ts+dSST.csv", na = "***", skip = 1) %>% 
  # select variables of interest and rename
  select(year = Year, t_diff = `J-D`) %>% 
  # plot
  ggplot(aes(x = year, y = t_diff)) +
  geom_line(aes(color = "1"), size = 0.5, show.legend = FALSE) +
  geom_point(fill = "white", aes(color = "1"), shape = 21, show.legend = TRUE) +
  # remove the SE, set color, thickness, and wiggliness
  geom_smooth(se = FALSE, aes(color = "2"), size = 1, span = 0.15, show.legend = FALSE) +
  # adjust the axis, get rid of the space on the ends
  scale_x_continuous(breaks = seq(1880, 2023, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0, 0)) +
  scale_color_manual(name = NULL, 
                     breaks = c(1, 2), 
                     values = c("gray", "black"),
                     labels = c("Annual mean", "Lowess smoothing"),
                     # override the circle shape and use the square shape
                     guide = guide_legend(override.aes = list(shape = 15, size = 4))) +
  labs(
    x = "YEAR", y = "Temperature anomaly (C)",
    title = "GLOBAL LAND-OCEAN\nTEMPERATUE INDEX",
    subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS"
  ) +
  theme_light() +
  theme(
    axis.ticks = element_blank(),
    # move the title to the left
    plot.title.position = "plot",
    # change the bottom margin of the title, the color, and fontface
    plot.title = element_text(margin = margin(b = 10), color = "darkred", face = "bold"),
    plot.subtitle = element_text(size = 8, margin = margin(b = 15)),
    legend.position = c(.15, .9),
    # no legend title
    legend.title = element_text(size = 0),
    # space within legend
    legend.key.height = unit(10, "pt"),
    # adjust the margin around
    legend.margin = margin(0, 0, 0, 0)
  )


ggsave("figures/temperature_index.png", width = 6, height = 5)
