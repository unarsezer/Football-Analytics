# How to make PassSolar with GGPLOT2

## Packages & Functions
```r
library(tidyverse)
library(ggplot2)
library(circular)
library(StatsBombR)
library(stringi)
library(NISTunits)
library(ggtext)
library(ggstar)
library(geomtextpath)
library(ggshakeR)
library(ggimage)
library(ggforce)
library(ggnewscale)

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}
```

I am going to use StatsBomb Data
## Data Preparation
```r
# I chose 14/15 LaLiga season because it has all the league matches from the season
Comp <- FreeCompetitions() %>%
  filter(competition_id == 11 & season_id == 26)

Matches <- FreeMatches(Comp)

df <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

data <- df %>% 
  filter(type.name == "Pass" & team.name == "Barcelona") 

data$x <-  unlist(lapply(data$location, function(x){ ifelse(is.null(x[[1]]) == T, NA, x[[1]]) }))
data$y <-  unlist(lapply(data$location, function(x){ ifelse(is.null(x[[2]]) == T, NA, x[[2]]) }))
data$finalX <- unlist(lapply(data$pass.end_location, function(x){ ifelse(is.null(x[[1]]) == T, NA, x[[1]]) }))
data$finalY <- unlist(lapply(data$pass.end_location, function(x){ ifelse(is.null(x[[2]]) == T, NA, x[[2]]) }))

data <- data %>%
  mutate(y = 80 - y,
         finalY = 80 - finalY) %>%
  select(team.name, player.name, type.name, under_pressure, pass.type.name, x, y, finalX, finalY, 
         pass.length, pass.angle, pass.recipient.name, pass.outcome.name) %>%
  mutate(pass.type.name = replace_na(pass.type.name, "Regular"),
         pass.outcome.name = replace_na(pass.outcome.name, "Complete"),
         under_pressure = replace_na(under_pressure, F)) %>%
  # Just Complete Passes
  filter(pass.outcome.name == "Complete") %>%
  # Just Open Play Passes
  filter(pass.type.name %in% c("Regular", "Recovery", "Interception")) %>%
  # To encode some characters, I change them into latin alphabet
  mutate(player.name = stri_trans_general(str = player.name, id = "Latin-ASCII"),
         pass.recipient.name = stri_trans_general(str = pass.recipient.name, id = "Latin-ASCII")) %>%
  # Xavi's Passes
  filter(player.name == "Xavier Hernandez Creus") %>%
  # Let's change the player names
  mutate(pass.recipient.name = case_when(pass.recipient.name == "Neymar da Silva Santos Junior" ~ "Neymar",
                                         pass.recipient.name == "Javier Alejandro Mascherano" ~ "Mascherano",
                                         pass.recipient.name == "Jordi Alba Ramos" ~ "Alba",
                                         pass.recipient.name == "Jeremy Mathieu" ~ "J. Mathieu",
                                         pass.recipient.name == "Gerard Pique Bernabeu" ~ "Piqué",
                                         pass.recipient.name == "Andres Iniesta Lujan" ~ "Iniesta",
                                         pass.recipient.name == "Luis Alberto Suarez Diaz" ~ "L. Suárez",
                                         pass.recipient.name == "Ivan Rakitic" ~ "Rakitic",
                                         pass.recipient.name == "Lionel Andres Messi Cuccittini" ~ "Messi",
                                         pass.recipient.name == "Claudio Andres Bravo Munoz" ~ "C. Bravo",
                                         pass.recipient.name == "Daniel Alves da Silva" ~ "D. Alves",
                                         pass.recipient.name == "Sergio Busquets i Burgos" ~ "Busquets",
                                         pass.recipient.name == "Xavier Hernandez Creus" ~ "Xavi",
                                         pass.recipient.name == "Rafael Alcantara do Nascimento" ~ "Rafinha",
                                         pass.recipient.name == "Pedro Eliezer Rodriguez Ledesma" ~ "Pedro",
                                         pass.recipient.name == "Adriano Correia Claro" ~ "Adriano",
                                         pass.recipient.name == "Marc Bartra Aregall" ~ "Bartra",
                                         pass.recipient.name == "Douglas Pereira dos Santos" ~ "Douglas",
                                         pass.recipient.name == "Sandro Ramirez Castillo" ~ "S. Ramírez",
                                         pass.recipient.name == "Sergi Roberto Carnicer" ~ "S. Roberto",
                                         pass.recipient.name == "Martin Montoya Torralbo" ~ "M. Montoya",
                                         pass.recipient.name == "Munir El Haddadi Mohamed" ~ "Munir",
                                         pass.recipient.name == "Thomas Vermaelen" ~ "T. Vermaelen",
                                         pass.recipient.name == "Jordi Masip Lopez" ~ "J. Masip",
                                         T ~ "Nobody")) %>%
  # Radian to Degree (I think it's more understandable)
  mutate(angle = NISTradianTOdeg(pass.angle), .after = "pass.angle") %>%
  mutate(angle = ifelse(angle < 0, 360 + angle, angle)) %>%
  select(team.name, player.name, under_pressure, x, y, finalX, finalY, pass.length, angle, pass.recipient.name)
  
data <- calculate_threat(data = data, type = "statsbomb") %>%
  mutate(xt = xTEnd - xTStart) %>%
  select(!c(xTEnd, xTStart))

# Finding circular mean
receivers <- data.frame()
for (i in unique(data$pass.recipient.name)) {
  m <- data %>% filter(pass.recipient.name == i)
  m1 <- data %>% filter(pass.recipient.name == i) %>% 
    group_by(pass.recipient.name) %>%
    summarise(pass = n(), under_press = length(player.name[under_pressure == T])) %>%
    mutate(press_rate = under_press / pass)
  if (nrow(m) > 2) {
    data.circular = circular(m$angle, units = "degrees", modulo = "2pi")
    if (nrow(receivers) == 0) {
      receivers = data.frame(receiver = i, mean_angle = mean(data.circular)[[1]], dist = mean(m$pass.length), number= nrow(m), 
                             mean_xt = mean(m$xt), presssure_rate = m1$press_rate)
    } else {
      receivers = rbind(receivers, 
                        data.frame(receiver = i, mean_angle = mean(data.circular)[[1]], dist = mean(m$pass.length), number= nrow(m),
                                   mean_xt = mean(m$xt), presssure_rate = m1$press_rate))
    }
  }
}

# If you noticed, we also found the percentage of passes under pressure. 
# I thought to show it in the graph but I didn't add it. Maybe you add it.

# Minimum 60 Passes, otherwise the graph looks very complicated.
receivers <- receivers %>% 
  filter(number >= 60) %>%
  mutate(mean_angle = round(mean_angle, digits = 0),
         dist = round(dist, digits = 0))

# Create the last dataset before the chart with CircleFun function
graph_data <- data.frame()
graph_data_circle <- data.frame()
for (i in (1:nrow(receivers))) {
  t = circleFun(center = c(0,0), diameter = receivers$dist[i]*2, npoints = 361) %>%
    mutate(mean_angle = c(seq(90, 0), seq(359, 90))) %>%
    mutate(receiver = receivers$receiver[i])
  t1 = circleFun(center = c(0,0), diameter = receivers$dist[i]*2, npoints = 361) %>%
    mutate(mean_angle = c(seq(90, 0), seq(359, 90))) %>%
    filter(mean_angle == receivers$mean_angle[i]) %>%
    mutate(receiver = receivers$receiver[i])
  if (nrow(graph_data) == 0) {
    graph_data = t1
    graph_data_circle = t
  } else {
    graph_data = rbind(graph_data, t1)
    graph_data_circle = rbind(graph_data_circle, t)
  }
}
rm(t, t1, m, m1)

# Yes, I am addicted to For loop :P 

receivers <- merge(graph_data, receivers, by = c("receiver", "mean_angle"))
```
## Graph Code
```r
p <- ggplot(receivers, aes(x, y, label = receiver)) +
  # Drawing orbits
  geom_path(data = graph_data_circle, aes(x, y, group = receiver), inherit.aes = F, color = "#6A6A6B", linewidth = 0.5, linetype = 2) +
  
  # Connections with Sun
  geom_textsegment(aes(x = 0, y = 0, xend = x, yend = y, label = paste0("~",dist, " m")), color = "#ececec", size = 5,
                    family = "Changa", text_only = T, fontface = 2) +
  geom_link(aes(x = 0, y = 0, xend = x, yend = y, alpha = stat(index), size = after_stat(index), color = mean_xt), n = 500, show.legend = T) +
  scale_color_gradientn(colours = c("#c1440e","#cf7c58","#deb4a2","#ececec","#abc6db","#69a0c9","#287ab8"), guide = guide_none()) +
  scale_alpha_continuous(guide = guide_none()) +
  scale_size_continuous(guide = guide_none()) +
  
  # Planets in orbit = Receivers
  new_scale("size") +
  geom_point(color = "#34363b", shape = 21, aes(size = number, fill = mean_xt)) +
  scale_size_continuous(range = c(15, 50), breaks = c(min(receivers$number), mean(receivers$number), max(receivers$number)),
                        labels = c("Less", "", "More"), name = "Number Of Passes",
                        guide = guide_legend(override.aes = list(size = c(15, 17, 20), shape = 21, color = "transparent", fill = "#ececec"), 
                                             label.position = "bottom", title.position = "top", title.hjust = 0.5)) +
  scale_fill_gradientn(colours = c("#c1440e","#cf7c58","#deb4a2","#ececec","#abc6db","#69a0c9","#287ab8"),
                       breaks = c(min(receivers$mean_xt), quantile(receivers$mean_xt, 0.25), 
                                  quantile(receivers$mean_xt, 0.75), max(receivers$mean_xt)),
                       labels = c("Less", "", "", "More"),
                       guide = guide_colorbar(title = "Pass Value (xT)",title.position = "top", title.hjust = 0.5, 
                                              title.theme = element_text(margin = margin(t = 0, b = 0.3, l=0,r=0, "cm"),
                                                                         family = "Teko", face = 2, color = "gray", size = 20),
                                              label.position = "bottom", label.hjust = 0.5, label.vjust = -1,
                                              barwidth = unit(7, "cm"), barheight = unit(1.5, "cm"), frame.colour = "#ececec", ticks = F, 
                                              frame.linewidth = 0.75)) +
  
  new_scale("size") +
  geom_text(color = "#34363b", family = "Teko", fontface = 2, aes(size = number), show.legend = T) +
  scale_size_continuous(range = c(2.5, 6), guide = guide_none()) +
  
  # Xavi = Sun
  geom_star(aes(x = 0, y = 0), starshape = 4, color = "#ffbf34", fill = "#ffbf34", size=40) +
  geom_image(aes(x = 0, y = 0), image = "xavi.png", size = 0.1) +
  
  # Direction
  geom_text(data = graph_data_circle %>% filter(y == max(.$y)) %>% head(1),
            aes(x = x, y = y, label = "Forward"), family = "Teko", fontface = 2, color = "#7E7E7E", size = 10) +
  geom_text(data = graph_data_circle %>% filter(y == min(.$y)) %>% head(1),
            aes(x = x, y = y, label = "Back"), family = "Teko", fontface = 2, color = "#7E7E7E", size = 10) +
  geom_text(data = graph_data_circle %>% filter(x == min(.$x)) %>% head(1),
            aes(x = x, y = y, label = "Left"), family = "Teko", fontface = 2, color = "#7E7E7E", size = 10, angle = 90) +
  geom_text(data = graph_data_circle %>% filter(x == max(.$x)) %>% head(1),
            aes(x = x, y = y, label = "Right"), family = "Teko", fontface = 2, color = "#7E7E7E", size = 10, angle = 270) +
  
  # Caption
  labs(caption = "Data Source: **StatsBomb** | Data Viz: **Sezer Unar (@unarsezer)**") +
  
  coord_equal() +
  labs(title = "XAVI'S PASSING NETWORK",
       subtitle = "2014/15 | La Liga | Barcelona | Min 60 Open Play Passes") +
  theme(plot.background = element_rect(fill = "#34363b", color = "#34363b"),
        plot.margin = margin(2,0,1,0, unit = "cm"),
        plot.title = element_text(family = "Teko", face = 2, color = "#ececec", size = 40, hjust = 0.5),
        plot.subtitle = element_text(family = "Teko", color = "gray", hjust = 0.5, size = 22),
        plot.caption = element_markdown(family = "Teko", color = "#ececec", size = 15, hjust = 0.5),
        panel.background = element_rect(fill = "#34363b", color = "#34363b"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(family = "Teko", face = 2, color = "gray", size = 20),
        legend.text = element_text(family = "Teko", color = "gray", size = 18),
        legend.margin = margin(0,4,1.1,4, unit="cm"))

ggsave(p, file = "passSolar.png", dpi = 300, width = 12, height = 16)
```
![passSolar2](https://user-images.githubusercontent.com/65786664/206486774-bc26c1e0-1a21-4242-855a-16ad51b503a6.png)
# Don't forget to tag me if you use the code!
