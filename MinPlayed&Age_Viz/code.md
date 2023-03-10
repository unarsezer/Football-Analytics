# How to make a Minutes Played & Age Viz with GGPLOT2

## Packages
```r
library(tidyverse)
library(worldfootballR)
library(extrafont)
library(ggpattern)
library(ggrepel)
library(ggtext)
library(stringi)
```

## Data
I use "worldfootballR" package to get the data.
```r
link <- "https://fbref.com/en/squads/ecd11ca2/Galatasaray-Stats"

df <- fb_team_player_stats(link, stat_type = "standard")

#I use this function to translate non-English characters into English, otherwise the player is ignored when filtering.
df$Player <- stringi::stri_trans_general(df$Player,"latin-ascii")
```

## Data Preparation
```r
#Total minutes played by the team in the league
max_time <- 1980

df <- df %>%
  select(Player, Age, Min_Playing_Time) %>%
  separate(Age, into = c("Age", "Day")) %>%
  mutate(Age = as.numeric(Age),
         Day = as.numeric(Day)) %>%
  mutate(Day = Day / 365) %>%
  mutate(Age = Age + Day,
         playing_rate = Min_Playing_Time / max_time) %>%
  filter(is.na(Min_Playing_Time) == F) %>%
  filter(!(Player %in% c("Alexandru Cicaldau", "Patrick van Aanholt", "Haris Seferovic", "Emre Akbaba", "Emre Kilinc",
                         "Taylan Antalyali", "Kaan Ayhan")))

df <- df %>%
  mutate(Player = case_when(Player == "Victor Nelsson" ~ "V. Nelsson",
                            Player == "Fernando Muslera" ~ "Muslera",
                            Player == "Sacha Boey" ~ "S. Boey",
                            Player == "Kerem Aktürkoğlu" ~ "K. Aktürkoğlu",
                            Player == "Abdülkerim Bardakcı" ~ "A. Bardakcı",
                            Player == "Sérgio Oliveira" ~ "S. Oliveira",
                            Player == "Dries Mertens" ~ "D. Mertens",
                            Player == "Lucas Torreira" ~ "L. Torreira",
                            Player == "Mauro Icardi" ~ "M. Icardi",
                            Player == "Milot Rashica" ~ "M. Rashica",
                            Player == "Yunus Akgün" ~ "Y. Akgün",
                            Player == "Leo Dubois" ~ "L. Dubois",
                            Player == "Bafétimbi Gomis" ~ "B. Gomis",
                            Player == "Barış Alper Yılmaz" ~ "B. Yılmaz",
                            Player == "Fredrik Midtsjø" ~ "Midtsjø",
                            Player == "Emre Taşdemir" ~ "E. Taşdemir",
                            Player == "Berkan Kutlu" ~ "B. Kutlu",
                            Player == "Emin Bayram" ~ "E. Bayram",
                            Player == "Kazimcan Karatas" ~ "K. Karatas",
                            Player == "Juan Mata" ~ "J. Mata",
                            Player == "Okan Kocuk" ~ "O. Kocuk",
                            Player == "Yusuf Demir" ~ "Y. Demir",
                            Player == "Hamza Akman" ~ "H. Akman",
                            Player == "Ozgur Baran Aksaka" ~ "O. Aksaka",
                            T ~ Player))
```

## ggplot2 code
```r
p <- ggplot(df, aes(x = Age, y = playing_rate, label = Player)) +
  
  scale_x_continuous(limits = c(17, 36), breaks = seq(17, 36)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, length = 5), labels = c("0%", "25%", "50%", "75%", "100%"),
                     sec.axis = sec_axis( trans=~.*max_time, breaks = seq(0, max_time, length = 5), name="Minutes Played")) +
  
  geom_vline(xintercept = c(17,23,24,29,30,36), color = "#898989", linetype = 2, linewidth = 0.3) +
  geom_vline(xintercept = c(19,20,21,22,25,26,27,28,31,32,33,34,35), color = "#898989", linetype = 2, linewidth = 0.1) +
  
  # Prime Age
  annotate(geom = "rect", xmin = 24, xmax = 29, ymin = -Inf, ymax = Inf, fill = "#3AFFF6", alpha = 0.2) +
  annotate(geom = "text", x = 26.5, y = 0.5, label = "PEAK", family = "Teko", fontface = 2, angle = 90, hjust = 0.5, size = 25,
           color = "#3AFFF6", alpha = 0.3) +
  
  # Youth Age
  annotate(geom = "rect", xmin = 17, xmax = 23, ymin = -Inf, ymax = Inf, fill = "#C8A2C8", alpha = 0.2) +
  annotate(geom = "text", x = 20, y = 0.5, label = "YOUTH", family = "Teko", fontface = 2, angle = 90, hjust = 0.5, size = 25,
           color = "#C8A2C8", alpha = 0.3) +
  
  # Experienced Age
  annotate(geom = "rect", xmin = 30, xmax = 36, ymin = -Inf, ymax = Inf, fill = "#FFC63A", alpha = 0.2) +
  annotate(geom = "text", x = 33, y = 0.5, label = "EXPERIENCE", family = "Teko", fontface = 2, angle = 90, hjust = 0.5, size = 25,
           color = "#FFC63A", alpha = 0.3) +
  
  # Youth & Prime
  geom_rect_pattern(aes(xmin = 23, xmax = 24, ymin = -Inf, ymax = Inf), 
                    pattern_fill = "#3AFFF6", fill = "transparent", color = "transparent",
                    pattern_density = 0.15, pattern_spacing = 0.02, pattern_alpha = 0.005, pattern_angle = 45) +
  geom_rect_pattern(aes(xmin = 23, xmax = 24, ymin = -Inf, ymax = Inf), 
                    pattern_fill = "#C8A2C8", fill = "transparent", color = "transparent",
                    pattern_density = 0.15, pattern_spacing = 0.02, pattern_alpha = 0.01, pattern_angle = 315) +
  
  # Prime & Experience
  geom_rect_pattern(aes(xmin = 29, xmax = 30, ymin = -Inf, ymax = Inf), 
                    pattern_fill = "#3AFFF6", fill = "transparent", color = "transparent",
                    pattern_density = 0.15, pattern_spacing = 0.02, pattern_alpha = 0.005, pattern_angle = 45) +
  geom_rect_pattern(aes(xmin = 29, xmax = 30, ymin = -Inf, ymax = Inf), 
                    pattern_fill = "#FFC63A", fill = "transparent", color = "transparent",
                    pattern_density = 0.15, pattern_spacing = 0.02, pattern_alpha = 0.005, pattern_angle = 315) +
  
  
  geom_point(shape = 21, fill = "yellow", color = "#dedede", size = 7) +
  geom_text_repel(family = "Teko", color = "#dedede", fontface = 2, size = 6, point.size = 7) +
  
  labs(y = "Percentage of Minutes Played",
       x = "Age",
       title = "PLAYERS' PLAYING TIME IN GALATASARAY",
       subtitle = "2022/23 Season | Turkish Süper Lig | 10 Mar 2023",
       caption = "Data Source: **OPTA Via Fbref** | Data Viz: **Sezer Unar (@unarsezer)**") +
  
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        plot.title = element_text(family = "Teko", color = "#dedede", face = 2, size = 40, hjust = 0.5),
        plot.subtitle = element_text(family = "Teko", color = "#dedede", size = 22, hjust = 0.5, 
                                     margin = margin(t=0.1, b= 0.9, l=0,r=0,"cm")),
        plot.caption = element_markdown(family = "Teko", color = "#dedede", size = 15, 
                                        margin = margin(t=0.2,b=0,l=0,r=0,"cm")),
        plot.margin = margin(2,2,2,2,"cm"),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#898989", linetype = 2, linewidth = 0.3),
        axis.title.x = element_text(family = "Teko", color = "#dedede", face = 2, size = 20),
        axis.title.y = element_text(family = "Teko", color = "#dedede", face = 2, size = 20),
        axis.text.x = element_text(family = "Teko", color = "#dedede", size = 19, margin = margin(t=0.4,b=0.3,l=0,r=0,"cm")),
        axis.text.y = element_text(family = "Teko", color = "#dedede", size = 19),
        axis.ticks = element_blank())

ggsave(p, file = "age_plot.png", width = 18, height = 13, dpi = 400)
```
![age_plot2](https://user-images.githubusercontent.com/65786664/224368484-a9e57319-31a0-4033-8186-c467c345303a.png)
# Don't forget to tag me if you use the code!
