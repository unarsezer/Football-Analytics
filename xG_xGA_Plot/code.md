# How to make xG & xGA Chart with GGPLOT2
The data I use belongs to FotMob and worldfootball package has been used.
## Packages & Functions
```r
library(tidyverse)
library(ggplot2)
library(extrafont)
library(worldfootballR)
library(ggtext)
library(ggblur)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
```
## Data Preparation
```r
league_logo <- get_png("epllogo.png")

# finding the TOP 10 teams
top10 <- fotmob_get_league_tables(country = "ENG", league_name = "Premier League")
top10 <- top10 %>% filter(table_type == "all")
top10 <- top10$table_short_name

# Preparing Graph Data
df <- fotmob_get_league_matches(country = "ENG", league_name = "Premier League") %>%
  dplyr::select(match_id = id, home, away, status) %>%
  tidyr::unnest_wider(c(home, away, status), names_sep = "_")
  
df <- df %>% 
  filter(status_started == T) %>%
  select(match_id, home_id, home_shortName, away_id, away_shortName, status_scoreStr) %>%
  rename(home_name = home_shortName, away_name = away_shortName) %>%
  separate(col = status_scoreStr, into = c("home_score", "away_score"), sep = " - ") %>%
  mutate(home_score = as.integer(home_score),
         away_score = as.integer(away_score)) %>%
  mutate(home_result = case_when(home_score > away_score ~ "win",
                                 home_score < away_score ~ "loss",
                                 T ~ "draw"),
         away_result = case_when(away_score > home_score ~ "win",
                                 away_score < home_score ~ "loss",
                                 T ~ "draw"))

home_df <- df %>% 
  select(match_id, home_name, home_id, away_name, home_result) %>%
  mutate(h_a = "h", .after = away_name) %>%
  rename(team = home_name, id = home_id, opp = away_name, result = home_result)

away_df <- df %>% 
  select(match_id, away_name, away_id, home_name, away_result) %>%
  mutate(h_a = "a", .after = home_name) %>%
  rename(team = away_name, id = away_id, opp = home_name, result = away_result)

graph_data <- rbind(home_df, away_df) %>% arrange(match_id)

# xG Data (It can take some time)
xg_data <- fotmob_get_match_details(unique(graph_data$match_id))
xg_data <- xg_data %>% 
  filter(is_own_goal == F) %>%
  group_by(match_id, team_id) %>%
  summarise(xg = sum(expected_goals), .groups = "drop") %>%
  rename(id = team_id)
  
# Merge Graph Data and xG Data
graph_data <- merge(graph_data, xg_data, by = c("match_id", "id"), all = T) %>%
  group_by(match_id) %>% mutate(xga = sum(xg) - xg) %>% ungroup() %>%
  mutate(image = paste0("https://images.fotmob.com/image_resources/logo/teamlogo/", id, ".png")) %>%
  mutate(strip_name = paste0(team, " <img src='", image, "' width='20'></img>"))
  
graph_data$team <- factor(graph_data$team, levels = top10)

strip_names <- unique(graph_data$strip_name)
names(strip_names) <- unique(graph_data$team)
```
## Graph Code
```r
# Graph 1
p <- ggplot(graph_data %>% filter(team %in% head(top10, 10)), aes(xga, xg)) +
  scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4)) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4)) +
  
  # Desing
  geom_polygon(data = data.frame(x = c(-Inf, Inf, Inf), y = c(-Inf, -Inf, Inf)),
               aes(x, y), fill = "#2A2C30", alpha = 1) +
  geom_segment(data = data.frame(x = c(0, 1, 2, 3, 4), y = -Inf, xend = c(0, 1, 2, 3, 4), yend = c(0, 1, 2, 3, 4)),
               aes(x=x, y=y, xend=xend, yend=yend), color = "#4D4D4D", linetype = 2, size = 0.4, inherit.aes = F) +
  geom_segment(data = data.frame(x = Inf, y = c(0, 1, 2, 3, 4), xend = c(0, 1, 2, 3, 4), yend = c(0, 1, 2, 3, 4)),
               aes(x=x, y=y, xend=xend, yend=yend), color = "#4D4D4D", linetype = 2, size = 0.4, inherit.aes = F) +
  geom_segment(aes(x = -Inf, xend = Inf, y = -Inf, yend = -Inf), color = "#D0CFCF", arrow = arrow(angle = 90, length = unit(0.05, "inches")),
               lineend = "round", linejoin = "round") +
  geom_segment(aes(x = -Inf, xend = -Inf, y = -Inf, yend = Inf), color = "#D0CFCF", arrow = arrow(angle = 90, length = unit(0.05, "inches")),
               lineend = "round", linejoin = "round") +
  
  # Axis Text
  geom_text(data = data.frame(x = -Inf, y = c(0,1,2,3,4)), aes(x, y, label = y), family = "Teko", color = "#D0CFCF", hjust = 1.5) +
  geom_text(data = data.frame(x = c(0,1,2,3,4), y = -Inf), aes(x, y, label = x), family = "Teko", color = "#D0CFCF", vjust = 1.5) +
  annotate(geom = "text", x = 4, y = 0, label = "xGA", family = "Teko", fontface = 2, color = "#D0CFCF", size = 4.5, hjust = 0.7, vjust = 1) +
  annotate(geom = "text", x = 0, y = 4, label = "xG", family = "Teko", fontface = 2, color = "#D0CFCF", size = 4.5, hjust = 0.5, vjust = 0) +
  
  # Main Part
  geom_segment(aes(x = -Inf, y = -Inf, xend = Inf, yend = Inf), color = "#D0CFCF", lineend = "round", linejoin = "round", linetype = 2) +
  geom_point(data = graph_data %>% select(!team), color = "#616368", size = 2, alpha = 0.75) +
  geom_point_blur(aes(color = result), size = 3, blur_size = 10, blur_steps = 10) +
  facet_wrap( ~team, nrow = 2, strip.position = "top", labeller = labeller(team = strip_names)) +
  
  scale_color_manual(values = c("#EFE511", "#EC6D2D", "#0DD08F")) +
  
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.title = element_text(family = "Teko", face = 2, color = "#D0CFCF", size = 45, hjust = 0.35),
        plot.subtitle = element_text(family = "Teko", color = "#D0CFCF", size = 15, margin = margin(0,0,30,0), hjust = 0.20),
        plot.caption = element_markdown(family = "Teko", color = "#D0CFCF", size = 10, lineheight = 1.25),
        panel.background = element_rect(fill = "#34363b", color = "transparent"),
        panel.grid.major.x = element_line(size = 0.4, color = "#6A6A6A", linetype = 2),
        panel.grid.major.y = element_line(size = 0.4, color = "#6A6A6A", linetype = 2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(3, "lines"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "#D0CFCF", size = 0.7),
        strip.background = element_blank(),
        strip.text = element_markdown(family = "Teko",  color = "#D0CFCF", size = 20, margin = margin(0,0,0.5,0,"cm"), lineheight = 1.25,
                                      hjust =1),
        legend.position = "none")
     
# Graph 2
final_p <- ggplot() +
  scale_x_continuous(limits = c(0, 18), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
  
  # Adding the main plot
  annotation_custom(grob = ggplotGrob(p), xmin = 0, xmax = 18, ymin = 0.75, ymax = 8.75) +
  
  # Adding title and subtitle
  geom_segment(aes(x = 0.2, xend = 17.8, y = 8.6, yend = 8.6), color = "#6a6a6a", size = 0.25) +
  annotation_custom(league_logo, xmin = 0.5, xmax = 1.5, ymin = 8.75, ymax = 9.75) +
  annotate(geom = "text", x = 1.7, y = 9.4, label = "TOP 10 Of English Premier League With xG and xGA",
           family = "Teko", fontface = 2, color = "#ececec", size = 10, hjust = "left") +
  annotate(geom = "text", x = 1.7, y = 9, label = "Each point represents a single match | 2022-23 Season | the 7th of October 2022",
           family = "Teko", fontface = 1, color = "#D0CFCF", size = 5.5, hjust = "left") +
  
  # Adding legend of colors
  geom_segment(aes(x = 0.2, xend = 17.8, y = 0.7, yend = 0.7), color = "#6A6A6A", size = 0.25) +
  geom_richtext(aes(x = 0.7, y = 0.4, 
                    label = "<span style = 'color:#0DD08F;'>**WINS**</span> | <span style = 'color:#EFE511;'>**DRAWS**</span> | <span style = 'color:#EC6D2D;'>**LOSES**</span>"),
                family = "Teko", color = "#D0CFCF", size = 10, hjust = "left", fill = NA, label.color = NA) +
  
  # Caption
  geom_richtext(aes(x = 17.3, y = 0.4,
                    label = "Data Source: **FotMob** | Inspired By: **@sonofacorner & @casualfantasypl** | Data Viz: **Sezer Unar (@unarsezer)**"),
                family = "Teko", color = "#D0CFCF", size = 4, hjust = "right", fill = NA, label.color = NA) +
  
  theme_void() +
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"))

ggsave(final_p, file = "xg_xga.png", width = 18, height = 10, dpi = 400)
```
![xg_xga_github](https://user-images.githubusercontent.com/65786664/194592679-5bef888e-dd8e-4f52-b901-f3007a579ee7.png)
# Don't forget to tag me if you use the code!
