# How to make xG Lollipop Chart with GGPLOT2
There are 3 parts of the code that you have to manually change according to the match. These are match id, team logos and colors.
## Packages & Functions
```r
library(tidyverse)
library(ggsvg)
library(ggplot2)
library(DescTools)
library(jsonlite)
library(grid)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
```
## Icons
```r
goal_icon = "goal.svg"
goal_icon = paste(readLines(goal_icon), collapse = "\n")

yellow_card = "yellow.svg"
yellow_card = paste(readLines(yellow_card), collapse = "\n")

d_yellow_card = "double_yellow.svg"
d_yellow_card = paste(readLines(d_yellow_card), collapse = "\n")

red_card = "red.svg"
red_card = paste(readLines(red_card), collapse = "\n")

subs = "subs.svg"
subs = paste(readLines(subs), collapse = "\n")

penalty = "penalty.svg"
penalty = paste(readLines(penalty), collapse = "\n")

video_referee = "var.svg"
video_referee = paste(readLines(video_referee), collapse = "\n")
```
## Data Preparation
We will get all the data we will use in this chart from FotMob.<br>We will prepare two datasets. One for the shots, the other for the game flow such as cards, subs and VAR.
```r
# FotMob match id
match_id <- "3900987"

df <- jsonlite::fromJSON(paste0("https://www.fotmob.com/api/matchDetails?matchId=", match_id))

home_logo <- get_png("manu_logo.png")
home_team <- df$general$homeTeam$name
home_id <- df$general$homeTeam$id
home_color <- "#E65B51" #df$general$teamColors$home
home_color2 <- MixColor(col1 = home_color, col2 = "#FFFFFF")

away_logo <- get_png("arsenal_logo.png")
away_team <- df$general$awayTeam$name
away_id <- df$general$awayTeam$id
away_color <- "#3D7CCB" #df$general$teamColors$away
away_color2 <- MixColor(col1 = away_color, col2 = "#FFFFFF")

score <- df$header$status$scoreStr

shots <- df$content$shotmap$shots %>%
  select(eventType, teamId, period, min, minAdded, expectedGoals, expectedGoalsOnTarget, isOwnGoal) %>%
  mutate(minAdded = ifelse(is.na(minAdded) == T, 0, minAdded), .after = "minAdded") 
  
game_flow <- data.frame(type = NA, time = NA, overloadTime = NA, goalDescription = NA,
                       isHome = NA, card = NA, minutesAddedStr = NA,
                       halfStrShort = NA, pendingDecision = NA, decision = NA) %>%
  nest(VAR = c(pendingDecision, decision)) %>%
  mutate(VAR = as.data.frame(VAR))
  
game_flow <- bind_rows(game_flow, df$content$matchFacts$events$events) %>%
  select(type, time, overloadTime, isHome, card, minutesAddedStr, halfStrShort, VAR, goalDescription) %>%
  filter(is.na(type) == F) %>%
  mutate(minutesAddedStr = gsub("\\D+", "", minutesAddedStr),
         minutesAddedStr = as.integer(minutesAddedStr)) %>%
  unnest(VAR) %>%
  mutate(goalDescription = ifelse(is.na(goalDescription) == T, "Empty", goalDescription),
         card = ifelse(is.na(card) == T, "Empty", card),
         overloadTime = ifelse(is.na(overloadTime) == T, 0, overloadTime)) %>%
  mutate(type = case_when(card != "Empty" ~ card,
                          T ~ type),
         shot.penalty = case_when(goalDescription == "Penalty" ~ T,
                                  type == "PenaltyMissed" ~ T,
                                  T ~ F),
         shot.owngoal = case_when(goalDescription == "Own goal" ~ T,
                                  T ~ F)) %>%
  select(!c(card, pendingDecision, goalDescription)) %>%
  filter(type != "Half") %>%
  mutate(period = ifelse(time <= 45, "FirstHalf", "SecondHalf"))
  
# Finding Added Times
first_half_added_time = filter(game_flow, type == "AddedTime" & time == 45)$minutesAddedStr
first_half_added_time = ifelse(length(first_half_added_time) == 0, 0, first_half_added_time)

second_half_added_time = filter(game_flow, type == "AddedTime" & time == 90)$minutesAddedStr
second_half_added_time = ifelse(length(second_half_added_time) == 0, 0, second_half_added_time)

# Finding max time of each period
max_first_half = max(filter(shots, period == "FirstHalf")$minAdded, filter(game_flow, period == "FirstHalf")$overloadTime, first_half_added_time)
max_second_half = max(filter(shots, period == "SecondHalf")$minAdded, filter(game_flow, period == "SecondHalf")$overloadTime, second_half_added_time)
extended_time = 90 + max_first_half + max_second_half

# Finding each event's extended minute
shots <- shots %>%
  mutate(extendedMinute = case_when(period == "FirstHalf" ~ min + minAdded,
                                    T ~ min + minAdded + max_first_half), .after = "minAdded") %>%
  mutate(expectedGoalsOnTarget = ifelse(is.na(expectedGoalsOnTarget) == T, 0, expectedGoalsOnTarget),
         xG = ifelse(teamId == home_id, expectedGoals, -expectedGoals),
         side = ifelse(teamId == home_id, "home", "away")) %>%
  # Let's fix values with xG greater than 0.60 to 0.60
  mutate(xG = case_when(teamId == home_id & xG > 0.60 ~ 0.60,
                        teamId == away_id & xG < -0.60 ~ -0.60,
                        T ~ xG))

## We are adding the icons to the game_flow, determine the size and y locations on the graph
game_flow <- game_flow %>%
  filter(type != "AddedTime") %>%
  mutate(extendedMinute = case_when(period == "FirstHalf" ~ time + overloadTime,
                                    T ~ time + overloadTime + max_first_half), .after = "overloadTime",
         y = case_when(isHome == T & type != "VAR" ~ 0.745,
                       isHome == F & type != "VAR" ~ -0.745,
                       isHome == T & type == "VAR" ~ 0.765,
                       isHome == F & type == "VAR" ~ -0.76,
                       T ~ 0),
         y2 = case_when(isHome == T & type == "VAR" ~ 0.715,
                        isHome == F & type == "VAR" ~ -0.715,
                        T ~ y),
         icon = case_when(type == "Yellow" ~ yellow_card,
                          type == "YellowRed" ~ d_yellow_card,
                          type == "Red" ~ red_card,
                          shot.penalty == T  ~ penalty,
                          shot.owngoal == T ~ goal_icon,
                          type == "VAR" ~ video_referee,
                          T ~ subs),
         size = case_when(type == "Yellow" ~ 5,
                          type == "YellowRed" ~ 7,
                          type == "Red" ~ 5,
                          shot.penalty == T ~ 7,
                          shot.owngoal == T ~ 1,
                          type == "VAR" ~ 9,
                          T ~ 8),
         fill = case_when(isHome == T ~ home_color,
                          T ~ away_color),
         fill2 = case_when(isHome == T ~ home_color2,
                           T ~ away_color2)) %>%
  mutate(y = case_when(shot.owngoal == T & isHome == T ~ -0.015,
                       shot.owngoal == T & isHome == F ~ 0.015,
                       T  ~ y),
         fill = case_when(shot.owngoal == T ~ "#D04E4E",
                          T ~ fill)) %>%
  separate(decision, into = c("label", "unneed"), sep = "-") %>%
  mutate(label = ifelse(is.na(label) == T, "", label)) %>%
  filter(!c(type == "Goal" & shot.penalty == F & shot.owngoal == F))                        
```
## Graph Code
```r
p <- ggplot() +
  scale_x_continuous(limits = c(-3, extended_time+3), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.98,1.15), breaks = c(-0.6,-0.45,-0.3, -0.15, 0.15, 0.3, 0.45, 0.6), expand = c(0,0)) +
  
  # Home & Away Background
  geom_rect(aes(xmin = 0, xmax = extended_time, ymin = 0, ymax = 0.65), fill = home_color, alpha = 0.15) +
  geom_rect(aes(xmin = 0, xmax = extended_time, ymin = 0, ymax = -0.65), fill = away_color, alpha = 0.15) +
  
  geom_rect(aes(xmin = 0, xmax = extended_time, ymin = 0.69, ymax = 0.8), fill = home_color, alpha = 0.15) +
  geom_rect(aes(xmin = 0, xmax = extended_time, ymin = -0.69, ymax = -0.8), fill = away_color, alpha = 0.15) +
  
  # Y Axis Lejant
  geom_rect(aes(xmin = 0, xmax = -3, ymin = -0.65, ymax = 0.65), fill = "#34363b") +
  geom_rect(aes(xmin = extended_time, xmax = extended_time+3, ymin = -0.65, ymax = 0.65), fill = "#34363b") +
  geom_text(aes(x = -1.75, y = 0, label = "Expected Goal"), color = "#8C8C8C", angle = 90, 
            hjust = 0.5, family = "Teko", fontface = 2, size = 6) +
  geom_text(aes(x = -1.75, y = 0.50, label = "MORE"), color = "#8C8C8C", angle = 90, 
            hjust = 0.5, family = "Teko", fontface = 2, size = 6) +
  geom_text(aes(x = -1.75, y = -0.50, label = "MORE"), color = "#8C8C8C", angle = 90, 
            hjust = 0.5, family = "Teko", fontface = 2, size = 6) +
  geom_rect(aes(xmin = -3, xmax = -0.5, ymin = 0, ymax = 0.65), fill = home_color, alpha = 0.15) +
  geom_rect(aes(xmin = -3, xmax = -0.5, ymin = 0, ymax = -0.65), fill = away_color, alpha = 0.15) +
  geom_segment(aes(x = -1.75, xend = -1.75, y = 0.15, yend = 0.40), arrow = arrow(length = unit(0.08, "inches")), 
               lineend = "round", linejoin = "round", color = home_color, size = 1) +
  geom_segment(aes(x = -1.75, xend = -1.75, y = -0.15, yend = -0.40), arrow = arrow(length = unit(0.08, "inches")), 
               lineend = "round", linejoin = "round", color = away_color, size = 1) +
  
  geom_rect(aes(xmin = extended_time+0.5, xmax = extended_time+3, ymin = 0.54, ymax = 0.8), fill = home_color, alpha = 0.15) +
  geom_rect(aes(xmin = extended_time+0.5, xmax = extended_time+3, ymin = -0.54, ymax = -0.8), fill = away_color, alpha = 0.15) +
  geom_text(aes(x = extended_time + 1.75, y = 0.67, label = "HOME TEAM"), color = home_color, family = "Teko", fontface = 2,
            angle = 270, hjust = 0.5, size = 5) +
  geom_text(aes(x = extended_time + 1.75, y = -0.67, label = "AWAY TEAM"), color = away_color, family = "Teko", fontface = 2,
            angle = 270, hjust = 0.5, size = 5) +
  
  # 0 xG Line and Half Time Line
  geom_segment(aes(x = 0, xend = extended_time, y = 0, yend = 0), linetype = 2, color = "#ececec") +
  geom_segment(aes(x = 45.5+max_first_half, xend = 45.5+max_first_half, y = 0.8, yend = -0.8), color = "#34363b", size = 1) +
  
  # X Axis Label
  geom_text(data = data.frame(x = c(seq(5, 45, by = 5), seq(46+max_first_half+5, 46+max_first_half+45, by = 5)), 
                              y = 0.67, l = seq(5, 90, by = 5)), 
            aes(x, y, label = l), color = "#68686A", family = "Teko", fontface = 2, alpha = 0.9, size = 6) +
  geom_text(data = data.frame(x = c(seq(5, 45, by = 5), seq(46+max_first_half+5, 46+max_first_half+45, by = 5)), 
                              y = -0.67, l = seq(5, 90, by = 5)), 
            aes(x, y, label = l), color = "#68686A", family = "Teko", fontface = 2, alpha = 0.9, size = 6) +
  
  # Shots with xG (Lollipop Part)
  geom_segment(data = shots %>% filter(isOwnGoal == F), 
               aes(x = extendedMinute, xend = extendedMinute, y = 0, yend = xG, color = side), size = 1) +
  geom_point(data = shots %>% filter(isOwnGoal == F),
             aes(x = extendedMinute, y = xG, color = side), shape = 21, stroke = 1, fill = "#34363b", size = 6) +
  
  scale_color_manual(values = c(away_color, home_color)) +
  
  # Goal Icon Home
  geom_point_svg(data = shots %>% filter(eventType == "Goal" & isOwnGoal == F & side == "home"), 
                 aes(x = extendedMinute, y = xG), css("path", fill = "#ececec"), svg = goal_icon, size = 4, show.legend = F) +
  
  # Goal Icon Away
  geom_point_svg(data = shots %>% filter(eventType == "Goal" & isOwnGoal == F & side == "away"), 
                 aes(x = extendedMinute, y = xG), css("path", fill = "#ececec"), svg = goal_icon, size = 4, show.legend = F) +
  
  # Cards & Subs & VAR Icons
  geom_point_svg(data = game_flow, aes(extendedMinute, y = y, svg = icon, size = size, 
                                       css("path", fill = fill), 
                                       css("path:nth-child(1)", fill = fill), css("path:nth-child(3)", fill = fill2),
                                       css("path:nth-child(4)", fill = fill), css("path:nth-child(5)", fill = fill2),
                                       css("path:nth-child(6)", fill = fill), css("path:nth-child(7)", fill = fill2)),
                 css("path:nth-child(8)", fill = "#34363b"), css("path:nth-child(9)", fill = "#34363b")) +
  scale_size_binned(range = c(5, 9)) +
  
  # VAR Decision
  geom_text(data = game_flow, aes(x = extendedMinute, y = y2, label = label), family = "Teko", color = "gray") +
  
  # Title
  annotation_custom(home_logo, xmin = -10, xmax = 20, ymin = 0.85, ymax = 1.15) +
  annotation_custom(away_logo, xmin = extended_time-20, xmax = extended_time+10, ymin = 0.85, ymax = 1.15) +
  
  geom_text(aes(x = extended_time / 2 - 9, y = 1.09, label =home_team), 
            family = "Teko", fontface = 2, color = home_color, size = 11, hjust = "right") +
  geom_text(aes(x = extended_time / 2 + 9, y = 1.09, label =away_team), 
            family = "Teko", fontface = 2, color = away_color, size = 11, hjust = "left") +
  
  geom_text(aes(x = extended_time / 2, y = 1.09, label = score), family = "Teko", fontface = 2, color = "gray", size = 18) +
  geom_text(aes(x = extended_time / 2, y = 0.97, label = "xG"), family = "Teko", color = "gray", size = 8, fontface = 2) +
  geom_text(aes(x = extended_time / 2, y = 0.90, label = "xGOT"), family = "Teko", color = "gray", size = 8, fontface = 2) +
  
  geom_text(data = shots %>% filter(side == "home") %>% summarise(xg = round(sum(expectedGoals, na.rm = T), digits = 2)),
            aes(x = extended_time / 2 - 5, y = 0.97, label = xg), family = "Teko", size = 7, hjust = "right", color = "gray") +
  geom_text(data = shots %>% filter(side == "away") %>% summarise(xg = round(sum(expectedGoals, na.rm = T), digits = 2)),
            aes(x = extended_time / 2 + 5, y = 0.97, label = xg), family = "Teko", size = 7, hjust = "left", color = "gray") +
  geom_text(data = shots %>% filter(side == "home") %>% summarise(xgot = round(sum(expectedGoalsOnTarget), digits = 2)),
            aes(x = extended_time / 2 - 5, y = 0.90, label = xgot), family = "Teko", size = 7, hjust = "right", color = "gray") +
  geom_text(data = shots %>% filter(side == "away") %>% summarise(xgot = round(sum(expectedGoalsOnTarget), digits = 2)),
            aes(x = extended_time / 2 + 5, y = 0.90, label = xgot), family = "Teko", size = 7, hjust = "left", color = "gray") +
  
  # Legand
  geom_rect(aes(xmin = 0, xmax = extended_time, ymin = -0.815, ymax = -0.98), fill = "black", alpha = 0.15) +
  
  geom_point_svg(aes(x = 3, y = -0.86), svg = goal_icon, css("path", fill = "#ececec"), size = 5) +
  geom_point_svg(aes(x = 8, y = -0.86), svg = goal_icon, css("path", fill = "#D04E4E"), size = 5) +
  geom_point_svg(aes(x = 13, y = -0.86), svg = penalty, css("path", fill = "gray"), size = 5) +
  geom_point_svg(aes(x = 18, y = -0.86), svg = yellow_card, size = 5) +
  geom_point_svg(aes(x = 23, y = -0.86), svg = d_yellow_card, size = 5.5) +
  geom_point_svg(aes(x = 28, y = -0.86), svg = red_card, size = 6) +
  geom_point_svg(aes(x = 33, y = -0.86), svg = subs, size = 6.3) +
  geom_point_svg(aes(x = 38, y = -0.86), svg = video_referee, size = 8, css("path", fill = "#ececec")) +
  
  geom_text(aes(x = 3, y = -0.90, label = "Goal"), family = "Teko", color = "#68686A", hjust = 0.5, vjust = "top", size = 4.5) +
  geom_text(aes(x = 8, y = -0.90, label = "Own\nGoal"), family = "Teko", color = "#68686A", hjust = 0.5, vjust = "top", 
            lineheight = 0.8, size = 4.5) +
  geom_text(aes(x = 13, y = -0.90, label = "Penalty"), family = "Teko", color = "#68686A", 
            hjust = 0.5, vjust = "top", size = 4.5) +
  geom_text(aes(x = 18, y = -0.90, label = "Yellow\nCard"), family = "Teko", color = "#68686A", hjust = 0.5, 
            vjust = "top", lineheight = 0.8, size = 4.5) +
  geom_text(aes(x = 23, y = -0.90, label = "Second\nYellow"), family = "Teko", color = "#68686A", hjust = 0.5, 
            vjust = "top", lineheight = 0.8, size = 4.5) +
  geom_text(aes(x = 28, y = -0.90, label = "Red\nCard"), family = "Teko", color = "#68686A", hjust = 0.5, 
            vjust = "top", lineheight = 0.8, size = 4.5) +
  geom_text(aes(x = 33, y = -0.90, label = "Subs"), family = "Teko", color = "#68686A", hjust = 0.5, vjust = "top", size = 4.5) +
  geom_text(aes(x = 38, y = -0.90, label = "VAR\nDecision"), family = "Teko", color = "#68686A", hjust = 0.5, 
            vjust = "top", size = 4.5, lineheight = 0.8) +
  
  # Caption
  annotate(geom = "text", x = extended_time-1, y = -0.8975, label = "Data Source: FotMob\nData Viz: Sezer Unar (@unarsezer)", 
           family = "Teko", fontface =2, color = "#68686A", size = 5, hjust = "right", vjust = 0.5) +
  
  # Theme
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        plot.margin = margin(1,1,1,1, "cm"),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#4F5053", size = 0.25),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

ggsave(p, file = "xG_lollipop.png", width = 16, height = 12.7, dpi = 400)
```
![xG_lollipop](https://user-images.githubusercontent.com/65786664/190355855-2c415b98-dc1a-497b-9c11-d1e824c60449.png)
# Don't forget to tag me if you use the code!
