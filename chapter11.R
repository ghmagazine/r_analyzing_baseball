# 11章
library(tidyverse)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "abdwr",
                  user = "root", password = "password")
class(conn)

load_gamelog <- function(season) {
  glheaders <- read.csv("data/game_log_header.csv")
  remote <- paste0("http://www.retrosheet.org/gamelogs/gl",
                   season, ".zip")
  local <- paste0("gl", season, ".zip")
  download.file(url = remote, destfile = local)
  unzip(local)
  local_txt <- gsub(".zip", ".txt", local) %>%
    toupper()
  gamelog <- read_csv(local_txt,
                      col_names = names(glheaders),
                      na = character())
  file.remove(local)
  file.remove(local_txt)
  return(gamelog)
}

gl2012 <- load_gamelog(2012)
dbSendQuery(conn, "SET GLOBAL local_infile = true;")
dbWriteTable(conn, "gamelogs", value = gl2012,
             append = TRUE, row.names = FALSE)

append_game_logs <- function(conn, season) {
  message(paste("Working on", season, "season..."))
  one_season <- load_gamelog(season)
  db_write_table
  dbWriteTable(conn, name = "gamelogs", value = one_season,
               append = TRUE, row.names = FALSE)
}
db_drop_table(conn, "gamelogs")
map(1995:2017, append_game_logs, conn = conn)

query <- "
SELECT date, hometeam, dayofweek, attendance
FROM gamelogs
WHERE date > 20060101
  AND hometeam IN ('CHN', 'CHA');
"
chi_attendance <- dbGetQuery(conn, query)
head(chi_attendance)

library(lubridate)
chi_attendance <- chi_attendance %>%
  mutate(the_date = ymd(date),
         attendance = ifelse(attendance == 0, NA, attendance))

crcblue <- "#2905A1"
ggplot(chi_attendance,
       aes(x = wday(the_date), y = attendance,
           color = hometeam)) +
  geom_jitter(height = 0, width = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_y_continuous("Attendance") +
  scale_x_continuous("Day of the Week", breaks = 1:7,
                     labels = wday(1:7, label = TRUE)) +
  scale_color_manual(values = c(crcblue, "gray70"))

query <- "
SELECT date, parkid, visitingteam, hometeam,
  visitorrunsscored AS awR, homerunsscore AS hmR
FROM gamelogs
WHERE (hometeam = 'COL' OR visitingteam = 'COL')
  AND Date > 19950000;
"
rockies_games <- dbGetQuery(conn, query)
rockies_games <- rockies_games %>%
  mutate(runs = awR + hmR,
         coors = parkid == "DEN02")
ggplot(rockies_games,
       aes(x = year(ymd(date)), y = runs, linetype = coors)) +
  stat_summary(fun.data = "mean_cl_boot") +
  xlab("Season") +
  ylab("Runs per game (both teams combined)") +
  scale_linetype_discrete(name = "Location",
                          labels = c("Other", "Coors Field"))

library(retro)
dbSendQuery(conn, "CREATE DATABASE retrosheet;")
db <- src_mysql(dbname = "retrosheet", 
                user = "root", password = "password")
retro <- etl("retro", db = db, dir = "data/retro")
retro %>% 
  etl_init() %>% 
  etl_extract(season = 1990:1999) %>% 
  etl_transform(season = 1990:1999)
for(i in 0:9){
  shell(paste0("cd data\\retro\\load && cwevent -n -f 0-96 -x 0-62 -y 199",i," 199",i,"*.EV* > events_199",i,".csv"))
  shell(paste0("cd data\\retro\\load && cwgame -n -f 0-83 -y 199",i," 199",i,"*.EV* > games_199",i,".csv"))
  shell(paste0("cd data\\retro\\load && cwsub -n -y 199",i," 199",i,"*.EV* > subs_199",i,".csv"))
}
files <- dir("data/retro/load/", pattern = ".csv", full.names = T)
events <- str_subset(files, pattern = "events_.*") %>% map(read_csv) %>% do.call("rbind", .)
colnames(events) <- colnames(events) %>% tolower()
events$year_id <- str_sub(events$game_id, 4, 7)
games <- str_subset(files, pattern = "games_.*") %>% map(read_csv) %>% do.call("rbind", .)
colnames(games) <- colnames(games) %>% tolower()
subs <- str_subset(files, pattern = "subs_.*") %>% map(read_csv) %>% do.call("rbind", .)
colnames(subs) <- colnames(subs) %>% tolower()
copy_to(db, events)
copy_to(db, games)
copy_to(db, subs)

query <- "
SELECT away_team_id, home_team_id, event_cd
FROM events
WHERE year_id = 1996
AND event_cd IN (2, 18, 19, 20, 21, 22, 23);
"
hr_PF <- dbGetQuery(retro$con, query)
hr_PF <- hr_PF %>%
  mutate(was_hr = ifelse(event_cd == 23, 1, 0))
ev_away <- hr_PF %>%
  group_by(team_id = away_team_id) %>%
  summarize(hr_event = mean(was_hr)) %>%
  mutate(type = "away")
ev_home <- hr_PF %>%
  group_by(team_id = home_team_id) %>%
  summarize(hr_event = mean(was_hr)) %>%
  mutate(type = "home")
ev_compare <- ev_away %>%
  bind_rows(ev_home) %>%
  spread(key = type, value = hr_event)
ev_compare

ev_compare <- ev_compare %>%
  mutate(pf = 100 * home / away)
ev_compare %>%
  arrange(desc(pf)) %>%
  head()
ev_compare %>%
  arrange(pf) %>%
  head()
query <- "
SELECT away_team_id, home_team_id, event_cd
FROM events
WHERE year_id = 1996
AND event_cd IN (2, 18, 19, 20, 21, 22, 23)
AND bat_id = 'galaa001';
"
andres <- dbGetQuery(retro$con, query) %>%
  mutate(was_hr = ifelse(event_cd == 23, 1, 0))
andres_pf <- andres %>%
  inner_join(ev_compare, by = c("home_team_id" = "team_id")) %>%
  summarize(mean_pf = mean(pf))
andres_pf
47 / (andres_pf / 100)
