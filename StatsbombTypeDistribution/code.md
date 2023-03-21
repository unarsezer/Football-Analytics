## Packages
```r
library(worldfootballR)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggnewscale)
library(ggtext)
```

## Data Preparation
### I will download data from FBREF using the worldfootballR package.
```r
data_playing_time <- load_fb_big5_advanced_season_stats(stat_type = "playing_time", team_or_player = "player")

# I'm doing this graph for Haaland, so I'm removing everyone except the forwards. 
# I'm also creating a striker template by selecting players who played in the EPL in the last 5 years. 

data_playing_time <- data_playing_time %>%
  filter(Season_End_Year %in% c(2018:2023)) %>%
  filter(Comp == "Premier League") %>%
  select(Season_End_Year, Squad, Player, Min_Playing.Time, Url) %>%
  filter(is.na(Min_Playing.Time) == F)

mapped_players <- player_dictionary_mapping() %>%
  rename(Url = UrlFBref) %>%
  select(Url, TmPos)

data <- left_join(data_playing_time, mapped_players, by = "Url") %>%
  filter(TmPos == "Centre-Forward" & Min_Playing.Time >= 1200) 

# I choose which metrics are important for a striker.

data_shooting <- load_fb_big5_advanced_season_stats(stat_type = "shooting", team_or_player = "player")
data_misc <- load_fb_big5_advanced_season_stats(stat_type = "misc", team_or_player = "player")
data_passing <- load_fb_big5_advanced_season_stats(stat_type = "passing", team_or_player = "player")
data_possession <- load_fb_big5_advanced_season_stats(stat_type = "possession", team_or_player = "player")

data_shooting <- data_shooting %>%
  filter(Season_End_Year %in% c(2018:2023)) %>%
  select(Season_End_Year, Squad, Player, Sh_per_90_Standard, npxG_Expected, npxG_per_Sh_Expected, Url)

data_misc <- data_misc %>%
  filter(Season_End_Year %in% c(2018:2023)) %>%
  select(Season_End_Year, Squad, Player, Won_Aerial, Won_percent_Aerial, Url)

data_passing <- data_passing %>%
  filter(Season_End_Year %in% c(2018:2023)) %>%
  select(Season_End_Year, Squad, Player, xA, KP, Url)

data_possession <- data_possession %>%
  filter(Season_End_Year %in% c(2018:2023)) %>%
  select(Season_End_Year, Squad, Player, `Att Pen_Touches`, Succ_Take, PrgR_Receiving, Url)

data <- left_join(data, data_shooting, by = c("Season_End_Year", "Squad", "Player", "Url"))
data <- left_join(data, data_misc, by = c("Season_End_Year", "Squad", "Player", "Url"))
data <- left_join(data, data_passing, by = c("Season_End_Year", "Squad", "Player", "Url"))
data <- left_join(data, data_possession, by = c("Season_End_Year", "Squad", "Player", "Url"))

data <- data %>%
  rename(mins_played = Min_Playing.Time,
         season_end_year = Season_End_Year,
         team = Squad,
         touch_in_box = `Att Pen_Touches`) %>%
  select(season_end_year, team, Player, mins_played, npxG_Expected, Sh_per_90_Standard, npxG_per_Sh_Expected, touch_in_box, 
         xA, KP, Succ_Take, PrgR_Receiving, Won_Aerial, Won_percent_Aerial) %>%
  # Standardize the data per 90 minutes
  mutate_at(c(5,8:13), function(x){(x / .$mins_played) * 90}) %>%
  # Creating a special ID for each row
  mutate(id = paste(season_end_year, team, Player, sep = "_"), .before = "season_end_year")
```
## What the dataset looks like
```r
str(data)
head(data, 30)
```
![data_str](https://user-images.githubusercontent.com/65786664/226616955-651cd686-818e-470a-9d38-21aab320c704.JPG)
![data_head](https://user-images.githubusercontent.com/65786664/226617406-b7a13a4c-a3bb-478f-aa85-1c916b4ea063.JPG)

## Distribution plot function
### Among the packages available in R, I couldn't find a function that paints a distribution with a gradient color up to the target point and leaves the rest in a solid color. That's why I decided to write this function. Or am I missing it? If you find it, please let me know.
```r
stylish_dist_viz <- function(data, selected_column, selected_id, variables, variable_labels, font, 
                             color_0quantile = "#15173f", color_25quantile = "#3076b3", color_50quantile = "#f1dd76",
                             color_75quantile = "#e76e29", color_100quantile = "#86000c") {
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}

  df <- data %>%
    select(variables, selected_column)
  
  if (length(selected_id) != 1) {
    print("Select only 1 id")
  }

  temp_col <- which(names(df) == selected_column)
  temp_row <- which(df[,temp_col] == selected_id)

  selected_df <- df[temp_row,]

  density_data <- apply(df[,-temp_col], 2, function(x){density(x, n = 3000)})

  last_df <- data.frame()
  for (i in 1:length(variables)){
    t <- data.frame(x = density_data[[i]]$x, y = density_data[[i]]$y, 
                    xend = density_data[[i]]$x, yend = 0,
                    column_name = names(density_data[i]),
                    min_value = min(df[i]),
                    max_value = max(df[i])) %>%
      filter(x > min(df[i]) & x < max(df[i]))
  
    prdc_color <- c(colorRampPalette(c(color_0quantile, color_25quantile))(nrow(filter(t, x > quantile(df[i][[1]], 0) & x < quantile(df[i][[1]], 0.25)))),
                  colorRampPalette(c(color_25quantile, color_50quantile))(nrow(filter(t, x > quantile(df[i][[1]], 0.25) & x < quantile(df[i][[1]], 0.5)))),
                  colorRampPalette(c(color_50quantile, color_75quantile))(nrow(filter(t, x > quantile(df[i][[1]], 0.5) & x < quantile(df[i][[1]], 0.75)))),
                  colorRampPalette(c(color_75quantile, color_100quantile))(nrow(filter(t, x > quantile(df[i][[1]], 0.75) & x < quantile(df[i][[1]], 1)))))
  
    t$colors <- prdc_color
  
    t <- t %>%
      mutate(colors = ifelse(x <= selected_df[names(density_data[i])][[1]], colors, "gray"),
             isColorful = ifelse(x <= selected_df[names(density_data[i])][[1]], 1, 0)) %>%
      mutate(x = range01(x),
            y = (range01(y) / 1.5) + (i-1),
            xend = range01(xend),
            yend = (0 / 1.5) + (i-1))
  
    if (length(last_df) == 0) {
      last_df <- t
    } else {
      last_df <- rbind(last_df, t)
    }
  }
  
  #
  border_df <- last_df %>% group_by(column_name) %>%
    summarise(x = min(x[isColorful == 0]), min_y = min(y), max_y = max(y))
  
  #
  legend_df <- data.frame(x = seq(0, 1, length = 300), y = -0.8, 
                          color = colorRampPalette(c(color_0quantile, color_25quantile, color_50quantile, color_75quantile, color_100quantile))(300)) 
  
  #
  labels_df <- last_df %>% 
    group_by(column_name) %>%
    summarise(y = min(y), min_value = min(min_value), max_value = max(max_value)) %>%
    ungroup()
  
  
  # 
  player_df <- df %>%
    pivot_longer(-selected_column, names_to = "column_name", values_to = "metric") %>%
    group_by(column_name) %>%
    mutate(percentile = round(percent_rank(metric)*100, digits = 0)) %>%
    ungroup() %>%
    filter(.[,1] == selected_id) %>%
    mutate(metric = round(metric, digits = 2))
  
  player_df_color <- last_df %>% 
    filter(isColorful == 1) %>%
    group_by(column_name) %>%
    mutate(temp_id = n():1) %>%
    ungroup() %>%
    filter(temp_id == 1) %>%
    select(column_name, colors, yend)
  
  player_df <- merge(player_df, player_df_color, by = "column_name") %>%
    mutate(label = paste0(metric, " | <span style = 'color:", colors, ";'>", percentile, "%</span>"))
  
  list(geom_segment(data = last_df, aes(x = x, y = y, xend = xend, yend = yend, color = colors)),
       scale_color_identity(),
       geom_segment(data = border_df, aes(x = x, xend = x, y = min_y, yend = max_y+0.1), color = "#34363b", linetype = 2, linewidth = 0.5),
       geom_segment(data = border_df, aes(x = x, xend = x, y = min_y-0.03, yend = min_y-0.02), 
                    arrow = arrow(length = unit(0.08, "inches"), type = "closed", angle = 30),
                    color = "#34363b"),
       
       scale_y_continuous(limits = c(-1, max(last_df$yend)+0.8), breaks = seq(0.1, max(last_df$yend)+0.1), labels = variable_labels),
       new_scale("color"),
       
       # Legend
       annotate(geom = "text", x = 0.5, y = -0.65, label = "Percentile Colouring", family = font, color = "#34363b", 
           size = 5, vjust = "bottom"),
       geom_tile(data = legend_df, aes(x, y, fill = color, color = color), height = 0.2),
       scale_fill_identity(),
       scale_color_identity(),
       annotate(geom = "text", x = 0, y = -1, label = "0%", family = font),
       annotate(geom = "text", x = 0.25, y = -1, label = "25%", family = font),
       annotate(geom = "text", x = 0.50, y = -1, label = "50%", family = font),
       annotate(geom = "text", x = 0.75, y = -1, label = "75%", family = font),
       annotate(geom = "text", x = 1, y = -1, label = "100%", family = font),
       
       
       geom_text(data = labels_df, aes(x = 0, y = y-0.2, label = round(min_value, digits = 2)), color = "#888888", family = font),
       geom_text(data = labels_df, aes(x = 1, y = y-0.2, label = round(max_value, digits = 2)), color = "#888888", family = font),
       
       geom_richtext(data = player_df, aes(x = 1.05, y = yend+0.1, label = label), color = "#34363b", family = font,
                     fill = NA, label.color = NA, hjust = "left", size = 5, fontface = 2))
}
```
## Function's guide
### Parameters
#### **data**: The data set you want to visualize
#### variables: What metrics will you use for visualization? Their ranking is important. The first one you write appears at the bottom. Example, c("xG", "xA", "key_pass")
#### variable_labels: What name do you want the metrics you write in the "variable" parameter to appear on the chart? You must use the same order as in the "variable" parameter! Example, c("Expected Goal", "Expected Assist", "Key Pass")
#### selected_column: While preparing the data, we created a variable with "unique" names. Here you write the name of that column. Example, c("id")
#### selected_id: Enter the name of the player whose graph we want to see in the column of "unique" names. Example, c("2023_Manchester City_Erling Haaland")
#### font: The font name you want to use
#### You can use the parameters "color_0quantile", "color_25quantile", "color_50quantile", "color_75quantile", "color_100quantile" to change the colors you want to use. If you do not use these parameters, the colors selected as defoult will be used.

## Graph Code
```r
p <- ggplot() +
  stylish_dist_viz(data = data, selected_column = "id", selected_id = "2023_Manchester City_Erling Haaland", 
                   variables = rev(c("npxG_Expected", "Sh_per_90_Standard", "npxG_per_Sh_Expected", "touch_in_box", "xA", "KP", 
                                     "Succ_Take", "PrgR_Receiving", "Won_Aerial", "Won_percent_Aerial")),
                   variable_labels = rev(c("NP xG", "Shots", "xG/Shots", "Touch In Box", "xA", "Key Passes", "Successful\nTake-on",
                                           "Progressive Pass\nReceiving", "Aerial Duel\nWon", "Aerial Duel\nWon %")),
                   font = "Arial") +
  scale_x_continuous(limits = c(-0, 1.2)) +
  labs(caption = "Data Source: **FBREF** | Inspired by: **StatsBomb** | Data Viz: **Sezer Unar (@unarsezer)**",
       title = "Erling Haaland",
       subtitle = "2022-23 | Manchester City | Forward Template") +
  theme(plot.margin = margin(t=2,b=2,l=2,r=2,"cm"),
        plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        plot.title.position = "plot",
        plot.title = element_text(family = "Arial", face = 2, color = "#34363b", size = 35, hjust = 0.5),
        plot.subtitle = element_text(family = "Arial", color = "#34363b", size = 20, hjust = 0.5, margin = margin(t=0.2,b=-0.1,l=0,r=0,"cm")),
        plot.caption = element_markdown(family = "Arial", color = "#34363b", size = 12),
        panel.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        panel.grid = element_blank(),
        axis.text.y = element_text(family = "Arial", color = "#34363b", size = 17, face = 2, lineheight = 0.85),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank())

ggsave(p, file = "distribution_chart.png", width = 12, height =15, dpi = 400)
```
![denee2](https://user-images.githubusercontent.com/65786664/226629681-dbf75d41-f0d7-4066-a083-6cb6c49d7eaa.png)
# Don't forget to tag me if you use the code!
