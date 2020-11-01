library(tidyverse)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "abdwr",
                  user = "root", password = "tsuyuzaki47")
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
dbSendQuery(conn, "SET GLOBAL local_infile = true;") # add
dbWriteTable(conn, "gamelogs", value = gl2012,
             append = TRUE, row.names = FALSE)

append_game_logs <- function(conn, season) {
  message(paste("Working on", season, "season..."))
  one_season <- load_gamelog(season)
  db_write_table
  dbWriteTable(conn, name = "gamelogs", value = one_season,
               append = TRUE, row.names = FALSE)
}
db_drop_table(conn, "gamelogs") # add
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

crcblue <- "#2905A1" #add
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
db <- src_mysql(dbname = "retrosheet", 
                user = "root", password = "tsuyuzaki47") # add
retro <- etl("retro", db = db, dir = "data/retro")
dbSendQuery(conn, "CREATE DATABASE retrosheet;") # add
retro %>%
  etl_init() %>%
  etl_extract() %>%
  etl_transform()
%>% 
  etl_load(season = 2019)

query <- "
SELECT *
FROM games;
"
hr_PF <- dbGetQuery(retro$con, query)
 