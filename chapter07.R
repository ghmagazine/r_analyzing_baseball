# 7章
library(tidyverse)
db <- src_sqlite("data/pitchrx.sqlite", create = TRUE)
library(pitchRx)
files <- c("inning/inning_all.xml", "inning/inning_hit.xml",
           "miniscoreboard.xml", "players.xml")
scrape(start = "2016-05-01", end = "2016-05-31",
       connect = db$con, suffix = files)

#### エラーになる場合は代わりに別のパッケージを使ってください
devtools::install_github("pontsuyu/pitchRx2")
library(pitchRx2)
gids <- game_ids %>% str_subset("gid_2016_05_.*")
scrape_inning_all(gids = gids, db_name = "data/pitchrx")
db <- src_sqlite("data/pitchrx.sqlite3")
#########

db_list_tables(db$con)
my_pitches <- db %>%
  tbl("pitch") %>%
  collect()

plate_width <- 17 + 2 * (9/pi)
crcblue <- "#2905a1"
k_zone_plot <- ggplot(NULL, aes(x = px, y = pz)) +
  geom_rect(xmin = -(plate_width/2)/12,
            xmax = (plate_width/2)/12,
            ymin = 1.5,
            ymax = 3.6, color = crcblue, alpha = 0) +
  coord_equal() +
  scale_x_continuous("水平位置（フィート）（Horizontal location (ft.)）",
                     limits = c(-2, 2)) +
  scale_y_continuous("垂直位置（フィート）（Vertical location (ft.)）",
                     limits = c(0, 5))

k_zone_plot %+% sample_n(my_pitches, 10000) +
  aes(color = type) +
  geom_point(alpha = 0.1)
  # scale_color_manual(values = c(crcblue, "white", "black"))
taken <- my_pitches %>%
  filter(type != "X")
zones <- taken %>%
  group_by(zone) %>% # 2020年11月時点ではzoneが取得できないようになっている...
  summarize(
    N = n(),
    right_edge = min(1.5, max(px)),
    left_edge = max(-1.5, min(px)),
    top_edge = min(5, quantile(pz, 0.95, na.rm = TRUE)),
    bottom_edge = max(0, quantile(pz, 0.05, na.rm = TRUE)),
    strike_pct = sum(type == "S") / n(),
    px = mean(px),
    pz = mean(pz))

library(ggrepel)
k_zone_plot %+% zones +
  geom_rect(aes(xmax = right_edge, xmin = left_edge,
                ymax = top_edge, ymin = bottom_edge,
                fill = strike_pct, alpha = strike_pct),
            color = "lightgray") +
  geom_text_repel(size = 3, aes(label = round(strike_pct, 2),
                                color = strike_pct < 0.5)) +
  scale_fill_gradient(low = "gray70", high = crcblue) +
  scale_color_manual(values = c("white", "black")) +
  guides(color = FALSE, alpha = FALSE)

library(mgcv)
strike_mod <- gam(type == "S" ~ s(px, pz),
                  family = binomial, data = taken)
library(broom)
hats <- strike_mod %>%
  augment(type.predict = "response")
k_zone_plot %+% sample_n(hats, 50000) +
  geom_point(aes(color = .fitted), alpha = 0.1) +
  scale_color_gradient(low = "gray70", high = crcblue)

library(modelr)
grid <- taken %>%
  data_grid(px = seq_range(px, n = 100),
            pz = seq_range(pz, n = 100))

grid_hats <- strike_mod %>%
  augment(type.predict = "response", newdata = grid)

tile_plot <- k_zone_plot %+% grid_hats +
  geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray92", high = crcblue)
tile_plot

more_taken <- db %>%
  tbl("pitch") %>%
  filter(type != "X") %>%
  inner_join(tbl(db, "atbat"),
             by = c("num", "gameday_link")) %>%
  collect()

hand_mod <- gam(type == "S" ~ p_throws + stand + s(px, pz),
                family = binomial, data = more_taken)
hand_grid <- more_taken %>%
  data_grid(px = seq_range(px, n = 100),
            pz = seq_range(pz, n = 100),
            p_throws, stand)
hand_grid_hats <- hand_mod %>%
  augment(type.predict = "response", newdata = hand_grid)

tile_plot %+% hand_grid_hats +
  facet_grid(p_throws ~ stand)
diffs <- hand_grid_hats %>%
  group_by(px, pz) %>%
  summarize(N = n(), .fitted = sd(.fitted))
tile_plot %+% diffs

sc_2017 <- read_csv("data/statcast2017.csv")
set.seed(111653)
sc_taken <- sc_2017 %>%
  filter(type != "X") %>%
  rename(px = plate_x, pz = plate_z) %>%
  sample_n(10000) %>%
  mutate(strike_prob = predict(strike_mod, newdata = .,
                               type = "response"))

library(lme4)
mod_a <- glmer(type == "S" ~ strike_prob + (1|pos2_person_id),
               data = sc_taken, family = binomial)
tidy(mod_a, effects = "fixed")
tidy(mod_a, effects = "ran_pars")


c_effects <- mod_a %>%
  ranef() %>%
  as_tibble() %>%
  transmute(id = as.numeric(levels(grp)),
            effect = condval)

master_id <- read_csv("data/masterid.csv")
c_effects <- c_effects %>%
  left_join(select(master_id, mlb_id, mlb_name),
            by = c("id" = "mlb_id")) %>%
  arrange(desc(effect))
c_effects %>% head()
c_effects %>% tail()

mod_b <- glmer(type == "S" ~ strike_prob + (1|pos2_person_id) +
                 (1|batter) + (1|pitcher),
               data = sc_taken, family = binomial)
tidy(mod_b, effects = "ran_pars")
c_effects <- mod_b %>%
  ranef() %>%
  as_tibble() %>%
  filter(grpvar == "pos2_person_id") %>%
  transmute(id = as.numeric(as.character(grp)),
            effect = condval)
c_effects <- c_effects %>%
  left_join(select(master_id, mlb_id, mlb_name),
            by = c("id" = "mlb_id")) %>%
  arrange(desc(effect))
c_effects %>% head()
c_effects %>% tail()





