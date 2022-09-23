# How to make Bar Chart with GGPLOT2

## Packages
```r
library(ggplot2)
library(tidyverse)
library(magick)
library(grid)
library(patchwork)
library(extrafont)
```

## Data
I attached the data I will use in the data folder.
```r
logo <- image_read("https://smarterscout.com/assets/images/smarterscout-client.png")
logo <- rasterGrob(logo, interpolate=TRUE)

df <- read.csv("df.csv", stringsAsFactors = F)
str(df)
```
![df_str](https://user-images.githubusercontent.com/65786664/191927104-27c85633-e3ce-4c69-bbdc-fe6ca835f405.JPG)

## Data Preparation
```r
df <- df %>%
  mutate(stats = case_when(stats == "link_up_rating" ~ "Link-up passing",
                           stats == "progressive_pass" ~ "Progressive passing",
                           stats == "ball_retention" ~ "Ball retention\nability*",
                           stats == "carry_dribble_rating" ~ "Carry & dribble\nvolume",
                           stats == "receive_in_box" ~ "Receptions in\n opp. box",
                           stats == "shoot" ~ "Shot\nvolume",
                           stats == "rate_involvement_in_moves_ending_in_a_shot" ~ "% involvement\nin moves ending in a shot",
                           stats == "attacking_output" ~ "Attacking\noutput*",
                           stats == "disrupt_opp_moves" ~ "Disrupting\nopp. moves",
                           stats == "recover" ~ "Ball\nrecoveries",
                           stats == "aerial" ~ "Aerial duels",
                           stats == "defending_quantity" ~ "Defending\nquantity*",
                           stats == "defending_quality" ~ "Defending\nquality*")) %>%
  # For numbers less than 2 to appear properly on the chart
  mutate(numbers2 = ifelse(numbers < 2, 2, numbers)) 
  
df$stats <- factor(df$stats, levels = c("Defending\nquality*", "Defending\nquantity*", "Aerial duels", "Ball\nrecoveries", 
                                        "Disrupting\nopp. moves", "Attacking\noutput*", "% involvement\nin moves ending in a shot",
                                        "Shot\nvolume", "Receptions in\n opp. box", "Ball retention\nability*", "Progressive passing",
                                        "Link-up passing", "Carry & dribble\nvolume"))
```

## Codes
```r
p <- ggplot(df, aes(x = numbers, y = stats)) +
  
  # X Axis Panel Grid
  geom_vline(xintercept = c(0, 20, 40, 60, 80, 100), color = "#626262") +
  geom_vline(xintercept = 50, color = "#767676", linetype = 2) +
  
  # Bar Part
  geom_bar(aes(fill = part, color = part), stat = "identity", width = 0.8) +
  geom_point(aes(x = numbers2, color = part), size = 20, shape = 21, fill = "#34363b", stroke = 2) +
  geom_text(aes(x = numbers2, label = numbers), family = "Teko", fontface = 2, 
            color = "white", size = 9) +
  
  # Stat Type Labels
  geom_rect(aes(xmin = -3, xmax = 0, ymin = -Inf, ymax = Inf), color = "#626262", inherit.aes = F) +
  geom_rect(aes(xmin = -3, xmax = 0, ymin = 13.4, ymax = 9.6), color = "#70FFFA", fill = "#70FFFA") +
  geom_rect(aes(xmin = -3, xmax = 0, ymin = 9.4, ymax = 5.6), color = "#FFE570", fill = "#FFE570") +
  geom_rect(aes(xmin = -3, xmax = 0, ymin = 5.4, ymax = 0.6), color = "#FF9970", fill = "#FF9970") +
  geom_text(aes(x = -1.5, y = 11.5, label = "P\nO\nS\nS\nE\nS\nS\nI\nO\nN"), 
            hjust = 0.5, vjust = 0.5, family = "Teko", fontface = 2, color = "#34363b", size = 10, lineheight = 0.7) +
  geom_text(aes(x = -1.5, y = 7.5, label = "A\nT\nT\nA\nC\nK\nI\nN\nG"), 
            hjust = 0.5, vjust = 0.5, family = "Teko", fontface = 2, color = "#34363b", size = 10, lineheight = 0.7) +
  geom_text(aes(x = -1.5, y = 3, label = "D\nE\nF\nE\nN\nS\nI\nV\nE"), 
            hjust = 0.5, vjust = 0.5, family = "Teko", fontface = 2, color = "#34363b", size = 10, lineheight = 0.7) +
  
  # 50% percentile line
  geom_vline(xintercept = 0, color = "#626262", linetype = 2) +
  
  # Scale X & Color & Fill
  scale_x_continuous(limits = c(-3, 100), breaks = c(0, 25, 50, 75, 100)) +
  scale_color_manual(values = c("#FFE570", "#FF9970", "#70FFFA")) +
  scale_fill_manual(values = c("#FFE570", "#FF9970", "#70FFFA")) +
  
  labs(title = "Mohamed Salah - Liverpool",
       subtitle = "2021-22 | 2787 minutes at RW | 30 years",
       tag = "*Adjusted for England Premier League",
       caption = "Sezer Unar (@unarsezer)") +
  coord_cartesian(clip = "off") +
  
  # Theme
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        plot.margin = margin(3,1,3,1, "cm"),
        plot.title = element_text(family = "Teko", face = 2, color = "gray", size = 50, hjust = 0.5, vjust = 5),
        plot.subtitle = element_text(family = "Teko", color = "gray", size = 30, hjust = 0.5, vjust = 5),
        plot.title.position = "plot",
        plot.tag.position = "bottom",
        plot.tag = element_text(family = "Teko", color = "gray", size = 20, hjust = 0.5),
        plot.caption = element_text(color = "gray", family = "Teko", size = 25, vjust = -15),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(color = "gray", size = 22, family = "Teko", face = 2, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

p <- p + inset_element(logo, 0.05, -0.06, 0.25, 0.14, align_to = 'full') + theme_void() +
  plot_annotation(theme = theme(plot.background = element_rect(color = "#34363b", fill = "#34363b")))

ggsave(p, file = "smarter_profile.png", width = 17, height = 17, dpi = 300)
```
![smarter_profile_github](https://user-images.githubusercontent.com/65786664/191927465-9efaf04b-56df-4d21-ad92-f9fc540c5696.png)
