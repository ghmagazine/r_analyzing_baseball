# 付録B
library(xml2)
game_url <- paste0("http://gd2.mlb.com/components/game/mlb/",
                   "year_2012/month_06/day_13/",
                   "gid_2012_06_13_houmlb_sfnmlb_1/",
                   "inning/inning_all.xml")
xml_game <- read_xml(game_url)

xml_innings <- xml_game %>%
  xml_find_all("//inning")
xml_attrs(xml_innings[[1]])

xml_innings %>%
  xml_attrs() %>%
  map_df(bind_rows)
library(XML2R)
inning_list <- XML2R(game_url, xpath = "//inning//*//atbat")
pitches_df <- inning_list[["pitch"]]
dim(pitches_df)

library(pitchRx)
dat <- scrape(start = "2012-05-31", end = "2012-06-01")

strikeFX(pitches, geom = "tile") +
  facet_grid(pitcher_name ~ stand) +
  scale_fill_gradient(high = crcblue, low = "gray92")
animateFX(pitches) +
  facet_grid(pitcher_name ~ stand)
pitchloc <- function(t, x0, ax, vx0,
                     y0, ay, vy0, z0, az, vz0) {
  x <- x0 + vx0 * t + 0.5 * ax * I(t ^ 2)
  y <- y0 + vy0 * t + 0.5 * ay * I(t ^ 2)
  z <- z0 + vz0 * t + 0.5 * az * I(t ^ 2)
  if(length(t) == 1) {
    loc <- c(x, y, z)
  } else {
    loc <- cbind(x, y, z)
  }
  return(loc)
}

pitch_trajectory <- function(x0, ax, vx0,
                             y0, ay, vy0, z0, az, vz0,
                             interval = 0.01) {
  cross_p <- (-1 * vy0 - sqrt(I(vy0 ^ 2) -
                                2 * y0 * ay)) / ay
  tracking <- t(sapply(seq(0, cross_p, interval), pitchloc,
                       x0 = x0, ax = ax, vx0 = vx0,
                       y0 = y0, ay = ay, vy0 = vy0,
                       z0 = z0, az = az, vz0 = vz0))
  colnames(tracking) <- c("x", "y", "z")
  tracking <- data.frame(tracking)
  return(tracking)
}

players <- paste0("https://www.baseballprospectus.com/",
                  "sortable/playerids/playerid_list.csv") %>%
  read_csv()



