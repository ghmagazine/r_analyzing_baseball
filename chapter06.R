mussina <- expand.grid(balls = 0:3, strikes = 0:2) %>%
  mutate(value = c(100, 118, 157, 207, 72, 82,
                   114, 171, 30, 38, 64, 122))
mussina
count_plot <- mussina %>%
  ggplot(aes(x = strikes, y = balls, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2("tOPS+", low = "grey10", high = crcblue,
                       mid = "white", midpoint = 100)
count_plot
nchar("BBSBFFFX")
sequences <- c("BBX", "C11BBC1S", "1X")
grep("1", sequences)
grepl("1", sequences)
grepl("11", sequences)
gsub("1", "", sequences)
headers <- read_csv("data/fields.csv")
pbp2016 <- read_csv("data/all2016.csv",
                    col_names = pull(headers, Header),
                    na = character())
pbp2016 <- pbp2016 %>%
  mutate(pseq = gsub("[.>123N+*]", "", PITCH_SEQ_TX))
pbp2016 <- pbp2016 %>%
  mutate(c10 = grepl("^[BIPV]", pseq))
pbp2016 <- pbp2016 %>%
  mutate(c01 = grepl("^[CFKLMOQRST]", pseq))
pbp2016 %>%
  select(PITCH_SEQ_TX, c10, c01) %>%
  head(10)


pbp16rc %>%
  select(GAME_ID, EVENT_ID, RUNS.VALUE, c00, c10, c20,
         c11, c01, c30, c21, c31, c02, c12, c22, c32) %>%
  head()















load("data/balls_strikes_count.RData")
ls()
cabrera_sample <- cabrera %>%
  sample_n(500)
k_zone_plot <- ggplot(cabrera_sample, aes(x = px, y = pz)) +
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5,
            ymax = 3.6, fill = "lightgray", alpha = 0.01) +
  coord_equal() +
  scale_x_continuous("Horizontal location (ft.)",
                     limits = c(-2, 2)) +
  scale_y_continuous("Vertical location (ft.)",
                     limits = c(0, 5))
crcblue <- "#2905a1" #add
k_zone_plot +
  geom_point(aes(color = factor(swung))) +
  scale_color_manual("Swung", values = c("gray70", crcblue),
                     labels = c("No", "Yes"))
miggy_loess <- loess(swung ~ px + pz, data = cabrera,
                     control = loess.control(surface = "direct"))
pred_area <- expand.grid(px = seq(-2, 2, by = 0.1),
                         pz = seq(0, 6, by = 0.1))
pred_area_fit <- pred_area %>%
  mutate(fit = as.numeric(predict(miggy_loess,
                                  newdata = .)))
pred_area_fit %>%
  filter(px == 0 & pz == 2.5) # down Broadway
pred_area_fit %>%
  filter(px == 0 & pz == 0)
pred_area_fit %>%
  filter(px == 2 & pz == 2.5) # way outside
cabrera_plot <- k_zone_plot %+%
  filter(pred_area_fit, fit >= 0, fit <= 1) +
  stat_contour(aes(z = fit, color = ..level..),
               binwidth = 0.2) +
  scale_color_gradient(low = "white", high = crcblue)
cabrera_plot <- cabrera_plot %>%
  directlabels::direct.label(method = "bottom.pieces")
cabrera_plot
counts <- c("0-0", "0-2", "2-0")
count_dfs <- cabrera %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  filter(count %in% counts) %>%
  split(pull(., count))
count_fits <- count_dfs %>%
  map(~loess(swung ~ px + pz, data = .,
             control = loess.control(surface = "direct"))) %>%
  map(predict, newdata = pred_area) %>%
  map(~data.frame(fit = as.numeric(.))) %>%
  map_df(bind_cols, pred_area, .id = "count") %>%
  mutate(balls = str_sub(count, 1, 1),
         strikes = str_sub(count, 3, 3))
cabrera_plot %+%
  filter(count_fits, fit > 0.1, fit < 0.7) +
  facet_wrap( ~ count)
verlander %>%
  group_by(pitch_type) %>%
  summarize(N = n()) %>%
  mutate(pct = N / nrow(verlander)) %>%
  arrange(desc(pct))
verlander %>%
  group_by(batter_hand, pitch_type) %>%
  summarize(N = n()) %>% spread(key = batter_hand, value = N) %>%
  mutate(L_pct = L / sum(L), R_pct = R / sum(R))
verlander %>%
  filter(batter_hand == "R") %>%
  group_by(balls, strikes, pitch_type) %>%
  summarize(N = n()) %>%
  spread(key = pitch_type, value = N, fill = 0) %>%
  mutate(num_pitches = CH + CU + FF + FT + SL) %>%
  mutate_if(is.numeric, funs(. / num_pitches)) %>%
  select(-num_pitches)

sample_n(umpires, 20)
umpires_rhb <- umpires %>%
  filter(batter_hand == "R",
         balls == 0 & strikes == 0 |
           balls == 3 & strikes == 0 |
           balls == 0 & strikes == 2)
ump_count_fits <- umpires_rhb %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  split(pull(., count)) %>%
  map(sample_n, 3000) %>%
  map(~loess(called_strike ~ px + pz, data = .,
             control = loess.control(surface = "direct"))) %>%
  map(predict, newdata = pred_area) %>%
  map(~data.frame(fit = as.numeric(.))) %>%
  map_df(bind_cols, pred_area, .id = "count") %>%
  mutate(balls = str_sub(count, 1, 1),
         strikes = str_sub(count, 3, 3))
k_zone_plot %+% filter(ump_count_fits, fit < 0.6 & fit > 0.4) +
  geom_contour(aes(z = fit, color = count, linetype = count),
               binwidth = 0.1) +
  scale_color_manual(values = crc_3)
