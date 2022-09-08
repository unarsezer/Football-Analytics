# How to make Venn Diagram with GGPLOT2

## Packages
```r
library(ggplot2) 
library(tidyverse)
library(geomtextpath)
library(extrafont)
library(readxl)
```
## Functions
We have two functions to create the circle shape and add the league logo.
```r
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
```
## Data
I attached the data I will use in the data folder.
```r
league_logo <- get_png("logo.png")

circle_run <- circleFun(c(1.00,1.0), diameter = 1, npoints = 500)
circle_shot <- circleFun(c(0.50,1.0), diameter = 1, npoints = 500)
cricle_pass <- circleFun(c(0.75,1.5), diameter = 1, npoints = 500)

df <- read.csv("venn_df.csv", stringsAsFactors = F)
str(df)
```
We have variables where the relevant metric returns as 1 for those in the top 40% in the percentile, and 0 otherwise.

![str_df](https://user-images.githubusercontent.com/65786664/189115656-cf746351-6484-4788-a788-676c6112c8b8.JPG)

## Codes
```r
p <- ggplot() +
  coord_equal(xlim = c(-0.20, 1.80), ylim = c(0.20, 2.3)) +
  
  # Prog Pass
  geom_text(data = df %>% filter(shot == 0 & prog_run == 0 & prog_pass == 1), 
            aes(0.75, 1.75, label = paste(player, collapse = "\n")), 
            family = "Teko", color = "#ececec", size = 7) +
  # Shot Volume 
  geom_text(data = df %>% filter(shot == 1 & prog_run == 0 & prog_pass == 0),
            aes(0.25, 0.90, label = paste(player, collapse = "\n")),
            family = "Teko", color = "#ececec", size = 7) +
  # Prog Run 
  geom_text(data = df %>% filter(shot == 0 & prog_run == 1 & prog_pass == 0),
            aes(1.25, 0.90, label = paste(player, collapse = "\n")),
            family = "Teko", color = "#ececec", size = 7) +
  # Shot Volume + Prog Pass
  geom_text(data = df %>% filter(shot == 1 & prog_run == 0 & prog_pass == 1),
            aes(0.45, 1.35, label = paste(player, collapse = "\n")),
            family = "Teko", color = "#ececec", size = 7) +
  # Prog Run + Prog Pass
  geom_text(data = df %>% filter(shot == 0 & prog_run == 1 & prog_pass == 1),
            aes(1.05, 1.35, label = paste(player, collapse = "\n")),
            family = "Teko", color = "#ececec", size = 7) +
  # Prog Run + Shot Volume
  geom_text(data = df %>% filter(shot == 1 & prog_run == 1 & prog_pass == 0),
            aes(0.75, 0.82, label = paste(player, collapse = "\n")),
            family = "Teko", color = "#ececec", size = 6) +
  
  # Circles
  geom_polygon(data = circle_run, aes(x, y), fill = "#FFE570", alpha = 0.4, color = "#FFE570", size = 4) +
  geom_polygon(data = circle_shot, aes(x, y), fill = "#70FFFA", alpha = 0.4, color = "#70FFFA", size = 4) +
  geom_polygon(data = cricle_pass, aes(x, y), fill = "#A7FF70", alpha = 0.4, color = "#A7FF70", size = 4) +
  
  # Elite Winger
  geom_text(data = df %>% filter(shot == 1 & prog_run == 1 & prog_pass == 1),
            aes(0.75, 1.2, label = paste(player, collapse = "\n")),
            family = "Teko", fontface = 2, color = "#34363b", size = 7) +
  
  # Circle Titles
  geom_textpath(data = rbind(as.data.frame(apply(head(circle_run, 75), 2, rev)), as.data.frame(apply(tail(circle_run, 175), 2, rev))), 
                aes(x, y, label = "PROGRESSIVE RUN"), text_only = TRUE, family = "Teko", fontface = 2,  size = 15, 
                vjust = -0.25, color = "#FFE570") +
  geom_textpath(data = tail(circle_shot, 400), aes(x, y, label = "SHOT VOLUME"), text_only = TRUE, family = "Teko", fontface = 2,
                size = 15, vjust = 1.25, color = "#70FFFA") +
  geom_textpath(data = cricle_pass %>% head(249) %>% rev(), aes(x, y, label = "PROGRESSIVE PASS"), 
                text_only = TRUE, family = "Teko", fontface = 2, size = 15, vjust = 1.25, color = "#A7FF70") +
  
  # Prog Run Metric Explanation
  annotate(geom = "text", x = 1.78, y = 0.30, label = "a continuous ball control by\none player attempting to draw the team\nsignificantly closer to the opponent goal.", hjust = "right", vjust = "top",
           family = "Teko", color = "#FFE570", size = 6) +
  geom_curve(aes(x = 1.55, y = 0.60, xend = 1.60, yend = 0.38), curvature = -0.3, size = 1,
             arrow = arrow(length = unit(2, "mm")), color = "#FFE570", ncp = 10) +
  
  # Prog Pass Metric Explanation
  annotate(geom = "text", x = 1.78, y = 1.75, label = "a forward pass that\nattempts to advance a\nteam significantly closer\nto the opponent’s goal.",
           family = "Teko", color = "#A7FF70", hjust = "right", vjust = "top", size = 6) +
  geom_curve(aes(x = 1.2, y = 1.97, xend = 1.52, yend = 1.80), curvature = -0.3,
             arrow = arrow(length = unit(2, "mm")), color = "#A7FF70", ncp = 10, size = 1) +
  
  # Shot Volume Metric Explanation
  annotate(geom = "text", x = -0.23, y = 1.5, label = "an attempt towards the\nopposition's goal\nwith the intention\nof scoring.", 
           family = "Teko", color = "#70FFFA", hjust = "left", vjust = "top", size = 6) +
  geom_curve(aes(x = -0.12, y = 0.97, xend = -0.13, yend = 1.25), curvature = -0.3, size = 1,
             arrow = arrow(length = unit(2, "mm")), color = "#70FFFA", ncp = 10) +
  
  # Title
  annotate(geom = "text", x = -0.20, y = 2.25, label = "WINGERS", 
           family = "Teko", fontface = 2, color = "#ececec", size = 25, hjust = "left") +
  
  # Subtitle
  annotate(geom = "text", x = -0.20, y = 2.15, label = "Playing More Than 1800 minutes", family = "Teko", 
           color = "#ececec", size = 7, hjust = "left") +
  annotate(geom = "text", x = -0.20, y = 2.10, label = "21/22 | Türkiye Süper Lig", family = "Teko", 
           color = "#ececec", size = 7, hjust = "left") +
  
  # Tag
  annotate(geom = "text", x = -0.20, y = 0.20, label = "Top %40 of Players For Each Metric",
           family = "Teko", color = "#ececec", size = 8, hjust = "left") +
  
  # Caption
  annotate(geom = "text", x = 1.80, y = 2.2, label = "Data Source: Wyscout\nData Viz: Sezer Unar (@unarsezer)", 
           color = "#ececec", family = "Teko",
           hjust = "right", vjust = "top", size = 6) +
  
  # League Logo
  annotation_custom(league_logo, xmin = -0.20, xmax = 0.10, ymin = 1.75, ymax = 2.05) +
  
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") 


ggsave(p, file = "venn.png", width = 15, height = 15, dpi = 400)
```
![deneme](https://user-images.githubusercontent.com/65786664/189118255-18ba79f8-1a33-4a82-879c-dece2dd37d9f.png)

## Don't forget to tag me if you use the code
