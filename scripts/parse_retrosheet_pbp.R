## ------------------------------------------------------------------------
download_retrosheet <- function(season) {
  # get zip file from retrosheet website
  download.file(
    url = paste0(
      "http://www.retrosheet.org/events/", season, "eve.zip"), 
    destfile = file.path("retrosheet", "zipped", 
                         paste0(season, "eve.zip"))
  )
}

## ------------------------------------------------------------------------
unzip_retrosheet <- function(season) {
  # unzip retrosheet files
  unzip(file.path("retrosheet", "zipped", 
                  paste0(season, "eve.zip")), 
        exdir = file.path("retrosheet", "unzipped"))
}

## ------------------------------------------------------------------------
create_csv_file <- function(season) {
  # http://chadwick.sourceforge.net/doc/cwevent.html
  # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
  wd <- getwd()
  setwd("retrosheet/unzipped")
  cmd <- paste0("cwevent -y ", season, " -f 0-96 ",
               season, "*.EV*", " > all", season, ".csv")
  message(cmd)
  if (.Platform$OS.type == "unix") {
    system(cmd)
  } else {
    shell(cmd)
  }
  setwd(wd)
}

## ------------------------------------------------------------------------
create_csv_roster <- function(season) {
  # creates a CSV file of the rosters
  rosters <- list.files(
    path = file.path("retrosheet", "unzipped"), 
    pattern = paste0(season, ".ROS"), 
    full.names = TRUE)
  
  rosters %>%
    map_df(read_csv, 
           col_names = c("PlayerID", "LastName", "FirstName", 
                         "Bats", "Pitches", "Team")) %>%
    write_csv(path = file.path("retrosheet", 
                               "unzipped", 
                               paste0("roster", season, ".csv")))
}

## ------------------------------------------------------------------------
cleanup <- function() {
  # removes retrosheet files not needed
  files <- list.files(
    path = file.path("retrosheet", "unzipped"),
    pattern = "(*.EV|*.ROS|TEAM*)",
    full.names = TRUE
  )
  unlink(files)
  
  zips <- list.files(
    path = file.path("retrosheet", "zipped"),
    pattern = "*.zip",
    full.names = TRUE
  )
  unlink(zips)
}

## ----eval=TRUE-----------------------------------------------------------
parse_retrosheet_pbp <- function(season) {
  download_retrosheet(season)
  unzip_retrosheet(season)
  create_csv_file(season)
  create_csv_roster(season)
  cleanup()
}

