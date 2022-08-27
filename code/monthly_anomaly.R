library(tidyverse)
library(glue)
library(gganimate)

month_anom <- 
  read_table("data/merra2_seas_anom.txt", skip = 3) %>% 
  select(month = Month, seas_anom) %>% 
  mutate(month = as.numeric(month),
         month = month.abb[month])

t_data <- 
  read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na() %>% 
  inner_join(., month_anom, by = "month") %>% 
  # minur 0.7 which is the baseline
  mutate(month_anom = t_diff + seas_anom - 0.7,
         month = factor(month, levels = month.abb)) %>% 
  group_by(year) %>% 
  mutate(ave = mean(month_anom)) %>% 
  ungroup() %>% 
  mutate(ave = ifelse(year == 2022, max(abs(ave)), ave))

annotation <- 
  t_data %>% 
  slice_tail(n = 1)

p <- 
  t_data %>% 
  ggplot(aes(x = month, y = month_anom, group = year, color = ave)) +
  geom_line() +
  scale_color_gradient2(low = "darkblue", mid = "white", high = "darkred",
                        midpoint = 0, guide = NULL) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-3, 2, by = 1)) +
  labs(x = NULL, y = NULL, 
       title = "Temperature Anomaly (\u00B0C)",
       subtitle = "Difference from 1980-2015 annual mean") +
  theme(
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted",
                                      size = 0.25),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )

p +
  geom_point(data = annotation, aes(x = month, y = month_anom),
             size = 5) +
  geom_text(data = annotation, aes(x = 5.8, y = 2), 
            label = glue("{annotation$month} {annotation$year}"),
            hjust = 1) 

ggsave("figures/monthly_anomaly.png", width = 6, height = 4)

a <- p +
  geom_label(aes(x = 7, y = 0, label = year), 
             fontface = "bold", label.size = 0) +
  transition_manual(year, cumulative = TRUE)

animate(a, width = 6, height = 4, unit = "in", res = 300)
anim_save("figures/monthly_anomaly.gif")