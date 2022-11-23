# How to make Two Dimentional Heat Map with GGPLOT2

## Packages & Functions
```r
library(tidyverse)
library(ggplot2)
library(ggsoccer)
library(extrafont)
library(ggtext)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
```
## Data 
I attached the data I used, you can find it in the "data" file.
```r
player_image <- get_png("arao.png")
my_color_pal <- c("#20e2ff","#53e5fa","#86e7f6","#b9eaf1","#ececec","#F1D2B4","#F6B97C","#FA9F44","#FF850C")

df <- read.csv("df.csv", stringsAsFactors = F)
head(df)
```
![head_df](https://user-images.githubusercontent.com/65786664/203540561-78855df7-9327-4c26-b79a-3926c32b6590.JPG)
## Data Preparation
```r
# To create heat map dataset for progressive passes. The values will represent the colors
pass_df <- df %>%
  filter(eventType == "Pass" & outcome == "Successful" & isProgPass == 1)

pass_df <- MASS::kde2d(pass_df$x, pass_df$y, n = c(40, 27), h = 13,
                       lims = c(c((100 / 40) / 2, 100 - (100 / 40) / 2), c((100 / 27) / 2, 100 - (100 / 27) / 2)))

pass_df <- data.frame(expand.grid(x = pass_df$x, y = pass_df$y), color = as.vector(pass_df$z)) 

# To create heat map dataset for all actions. The values will represent the size
df <- MASS::kde2d(df$x, df$y, n = c(40, 27), h = 13,
                    lims = c(c((100 / 40) / 2, 100 - (100 / 40) / 2), c((100 / 27) / 2, 100 - (100 / 27) / 2)))
df <- data.frame(expand.grid(x = df$x, y = df$y), size = as.vector(df$z)) 

# Let's merge them
graph_data <- merge(df, pass_df, by = c("x", "y"))
```
## Graph Code
```r
p <- ggplot(graph_data) +
  annotate_pitch(dimensions = pitch_opta, colour = "#808080", fill = "#2B2D32", limits = F) +
  theme_pitch(aspect_ratio = 93.16/105) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(-12, 125)) +
  
  # Heat map part
  geom_tile(aes(x, y), fill = "transparent", color = "#6E6F6F", linetype = 2) +
  geom_point(aes(x, y, size = size, color = color)) +
  
  # Size and Colors
  scale_size_continuous(range = c(-3, 13), breaks = c(min(graph_data$size), mean(graph_data$size), max(graph_data$size)),
                        labels = c("Less", "", "More")) +
  scale_color_gradientn(colours = my_color_pal, breaks = c(min(graph_data$color), mean(graph_data$color), max(graph_data$color)),
                        labels = c("Less", "", "More")) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, label.position = "bottom",
                              override.aes = list(size = 11, color = c("#20e2ff", "#ececec", "#FF850C"))),
         size = guide_legend(title.position = "top", title.hjust = 0.5, label.position = "bottom", keywidth = unit(1, "cm"),
                             override.aes = list(size = c(4, 8, 11), color = "gray"))) +
  labs(size = "Action Freq.",
       color = "Prog. Pass Freq.") +
  
  # Player ID
  geom_point(aes(x = 7, y = 114), shape = 21, fill = "#2B2D32", color = "#34363b", size = 68) +
  geom_rect(aes(xmin = 15, xmax = 67, ymin = 103, ymax = 125), color = "#2B2D32", fill = "#2B2D32", size = 0.35) +
  annotation_custom(player_image, xmin = 1, xmax = 13, ymin = 104, ymax = 124) +
  annotate(geom = "text", x = 16, y = 120, label= "Willian Arão", family = "Teko", fontface = 2, color = "#ececec",
           hjust = "left", size = 18) +
  annotate(geom = "text", x = 16, y = 112, label = "Compared to the zones on the heatmap,\nwhere does he prefer to make his progressive passes?",
           family = "Teko", color = "gray", size = 8, hjust = "left", lineheight = 0.8) +
  annotate(geom = "text", x = 16, y = 106, label = "Turkish Süper Lig | 2022/23 Season | 21 November 2022", 
           family = "Teko", hjust = "left", color = "gray", size = 6) +
  
  # Legend
  geom_rect(aes(xmin = 68, xmax = 100, ymin = 103, ymax = 118), color = "#2B2D32", fill = "#2B2D32", size = 0.35) +
  
  # Caption
  geom_rect(aes(xmin = 68, xmax = 100, ymin = 119, ymax = 125), color = "#2B2D32", fill = "#2B2D32", size = 0.35) +
  geom_richtext(aes(x = 84, y = 122, label = "Data Source: **OPTA** | Data Viz: **Sezer Unar**"),
                family = "Teko", color = "#808080", hjust = 0.5, size = 6, fill = NA, label.color = NA) +
  
  # Prog Pass Definition
  geom_rect(aes(xmin = 0, xmax = 100, ymin = -4, ymax = -12), color = "#2B2D32", fill = "#2B2D32") +
  annotate(geom = "text", x = 50, y = -8, 
           label = "Progressive Pass: Completed open-play pass that moves at least 25% closer to the goal from its origin.",
           family = "Teko", color = "#808080", size = 7, hjust = 0.5) +
  
  # Direction of Play
  geom_point(aes(x = 30, y = -2), color = "#808080", size = 5) +
  geom_segment(aes(x = 30, xend = 70, y = -2, yend = -2), color = "#808080", size = 1, lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.30, "inches"), type = "closed", angle = 15)) +
  
  # Goals (Extra)
  geom_segment(aes(x = 0, xend = 0, y = 44.2, yend = 55.8), color = "#808080", size = 2) +
  geom_segment(aes(x = 100, xend = 100, y = 44.2, yend = 55.8), color = "#808080", size = 2) +
  
  theme(legend.position = c(0.805, 0.855),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(family = "Teko", face = 2, color = "#808080", size = 15),
        legend.text = element_text(family = "Teko", color = "#808080", size = 14),
        legend.margin = margin(t = 0, b = 0, r = 0.5, l = 0.5, "cm"),
        plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"))

ggsave(p, file = "heatmap_trial.png", width = 10.5*1.5, height = 9.316*1.5, dpi = 300)
```
![heatmap_trial](https://user-images.githubusercontent.com/65786664/203541134-205a203c-2535-4774-884c-9a58d495fb27.png)
# Don't forget to tag me if you use the code!
