# 12章
devtools::install_github("BillPetti/baseballr")

library(tidyverse)
library(baseballr)
correa <- scrape_statcast_savant(start_date = "2017-05-01",
                                 end_date = "2017-05-31",
                                 playerid = 621043,
                                 player_type = "batter")
correa %>%
  select(events, hc_x, hc_y) %>%
  head()
correa_bip <- correa %>%
  filter(type == "X")
correa_bip %>%
  select(events, hc_x, hc_y) %>%
  head()

# ggspraychart(data = correa, fill_value = "events")
spray_chart <- function(...){
  ggplot(...) +
    geom_curve(x = 33, xend = 223, y = -100, yend = -100,
               curvature = -.65) +
    geom_segment(x = 128, xend = 33, y = -208, yend = -100) +
    geom_segment(x = 128, xend = 223, y = -208, yend = -100) +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156,
               curvature = -.65, linetype = "dotted") +
    coord_fixed() +
    scale_x_continuous(NULL, limits = c(25, 225)) +
    scale_y_continuous(NULL, limits = c(-225, -25))
}
spray_chart(correa_bip, aes(x = hc_x, y = -hc_y, color = events)) +
  geom_point() +
  scale_color_grey()

library(statcastr)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "abdwr",
                  user = "root", password = "password")
dbSendQuery(conn, "CREATE DATABASE statcast;")
db <- src_mysql(dbname = "statcast", 
                user = "root", password = "password")
sc <- etl("statcastr", db, dir = "statcastr")
sc %>%
  etl_init() %>% 
  etl_extract(year = 2017, month = 3:10) %>% 
  etl_transform(year = 2017, month = 3:10)
files <- dir("statcastr/load/", pattern = ".csv", full.names = T)
statcast <- files %>% map(read_csv) %>% do.call("rbind", .)
copy_to(db, statcast)

sc_2017 <- db %>%
  tbl("statcast") %>%
  collect()
write_csv(sc_2017, path = "data/statcast2017.csv")

sc_2017 <- read_csv("data/statcast2017.csv")
sc_bips <- sc_2017 %>%
  filter(type == "X", game_type == "R")
left_handed_batters <- sc_bips %>%
  group_by(batter, player_name) %>%
  summarize(N = n(), Stand = mean(stand == "L")) %>%
  filter(N > 300, Stand == 1) %>%
  inner_join(sc_bips)
left_handed_batters %>%
  filter(launch_angle < 10) %>%
  summarize(N = n(),
            P_opp = 100 * mean(hc_x > 125.42, na.rm = TRUE),
            Shift = 100 *
              mean(if_fielding_alignment == "Infield shift",
                   na.rm = TRUE)) -> shift_data
ggplot(shift_data, aes(P_opp, Shift)) +
  geom_point() +
  xlab("Percentage of Grounders Pulled") +
  ylab("Percentage of Infield Shifts")
guidelines <- tibble(
  launch_angle = c(10, 25, 50),
  launch_speed = 40,
  label = c("Ground balls", "Line drives", "Flyballs")
)
crcblue <- "#2905A1"
ev_plot <- sc_bips %>%
  # 実行時間短縮のためデータを絞る
  sample_n(nrow(.) / 2) %>%
  ggplot(aes(x = as.numeric(launch_speed), y = as.numeric(launch_angle),
             color = as.numeric(estimated_ba_using_speedangle))) +
  geom_hline(data = guidelines, aes(yintercept = launch_angle),
             color = "black", linetype = 2) +
  geom_text(data = guidelines,
            aes(label = label, y = launch_angle - 4),
            color = "black", hjust = "left") +
  geom_point(alpha = 0.05) +
  scale_color_gradient("BA", low = crcblue, high = "white") +
  scale_x_continuous("Exit velocity (mph)",
                     limits = c(40, 120)) +
  scale_y_continuous("Launch angle (degrees)",
                     breaks = seq(-75, 75, 25))
ev_plot

ev_plot +
  facet_grid(p_throws ~ stand)
ev_plot +
  aes(color = estimated_woba_using_speedangle) +
  guides(color = guide_colorbar(title = "wOBA"))

sc_bips_16 <- read_csv("data/statcast2016.csv") %>%
  filter(type == "X", game_type == "R") %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle))
library(mgcv)
fit <- sc_bips_16 %>%
  sample_n(nrow(.) / 2) %>%
  gam(HR ~ s(launch_speed, launch_angle),
      family = binomial, data = .)
ls_la_grid <- expand.grid(
  launch_speed = seq(90, 115, length.out = 50),
  launch_angle = seq(15, 45, length.out = 50))

library(broom)
hats <- fit %>%
  augment(type.predict = "response", newdata = ls_la_grid)

odd_values <- seq(from = 0.1, to = 0.9, by = 0.2)
hat_labels <- hats %>%
  filter(round(launch_angle) == 30) %>%
  group_by(hr_prob = round(.fitted, 1)) %>%
  summarize(N = n(),
            launch_speed = mean(launch_speed) + 1,
            launch_angle = mean(launch_angle)) %>%
  filter(as.character(hr_prob) %in% odd_values)
ggplot(hats, aes(x = launch_speed, y = launch_angle)) +
  geom_tile(aes(fill = .fitted)) +
  geom_contour(aes(z = .fitted), breaks = odd_values) +
  geom_text(data = hat_labels, aes(label = hr_prob)) +
  xlab("Exit velocity (mph)") +
  ylab("Launch angle (degrees)") +
  scale_fill_gradient("HR prob", low = crcblue, high = "white")

sc_bips <- sc_bips %>%
  mutate(HR = ifelse(events == "home_run", 1, 0),
         launch_speed = as.numeric(launch_speed),
         launch_angle = as.numeric(launch_angle))
sc_bips %>%
  summarize(total_hr = sum(HR, na.rm = TRUE)) -> actual_hr
actual_hr
hats2 <- fit %>%
  augment(type.predict = "response", newdata = sc_bips)
sim_hr <- function(){
  N <- nrow(hats2)
  sum(runif(N) < hats2$.fitted, na.rm = TRUE)
}
HR_predict <- data.frame(HR = replicate(1000, sim_hr()))

ggplot(HR_predict, aes(HR)) +
  geom_histogram(color = "black", fill = "white", bins = 20) +
  geom_vline(xintercept = pull(actual_hr), color = crcblue) +
  annotate(geom = "text", x = actual_hr$total_hr-70, y = 200,
           label = "2017 HR Count", color = crcblue) +
  xlab("Predicted Number of Home Runs")
regulars <- sc_bips %>%
  group_by(batter, player_name) %>%
  summarize(N = n(),
            avg_la = mean(launch_angle, na.rm = TRUE),
            var_la = var(launch_angle, na.rm = TRUE)) %>%
  filter(N > 300)
regulars %>%
  arrange(desc(avg_la))
regulars %>%
  arrange(avg_la)
regulars %>%
  arrange(var_la)
sc_regulars <- sc_bips %>%
  filter(!is.na(launch_angle)) %>%
  inner_join(regulars, by = "batter")

la_plot <- ggplot(sc_regulars,
                  aes(x = launch_angle, group = batter)) +
  geom_density(size = 0.1, color = "darkgray") +
  scale_x_continuous("Launch Angle (degrees)")
la_plot
highlight <- regulars %>%
  filter(avg_la < 1 | var_la < 482) %>%
  mutate(x_coord = ifelse(avg_la < 1, -45, 50),
         y_coord = ifelse(avg_la < 1, 0.01, 0.02))
highlight_regulars <- sc_regulars %>%
  filter(!is.na(launch_angle)) %>%
  inner_join(highlight, by = "batter")
library(ggrepel)
la_plot +
  geom_text_repel(data = highlight,
                  aes(x = x_coord, label = player_name,
                      y = y_coord), color = crcblue) +
  geom_density(data = highlight_regulars, color = crcblue)
sc_split <- sc_regulars %>%
  mutate(split = game_date < "2017-07-01") %>%
  group_by(split, batter) %>%
  summarize(N = n(),
            avg_la = mean(launch_angle, na.rm = TRUE))
sc_split %>%
  arrange(batter)
sc_split_wide <- sc_split %>%
  select(-N) %>%
  spread(key = split, value = avg_la, sep = "_")
sc_split_wide
la_cor <- sc_split_wide %>%
  summarize(correlation = cor(split_TRUE, split_FALSE))
la_cor
ggplot(sc_split_wide, aes(x = split_TRUE, y = split_FALSE)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2, color = "darkgray") +
  geom_point() +
  geom_smooth() +
  scale_x_continuous("Launch angle (first half)",
                     limits = c(-2, 24)) +
  scale_y_continuous("Launch angle (second half)",
                     limits = c(-2, 24)) +
  coord_fixed()
