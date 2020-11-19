# 10章のstatcast2017.csvの作成方法
library(baseballr)
start_date <- "2017-04-01"
end_date <- "2017-10-01"
days <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
start_days <- as.character(days[(1:length(days)) %% 7 == 1])
end_days <- as.character(days[(1:length(days)) %% 7 == 0])
if(length(start_days)!=length(end_days))
  end_days <- c(end_days, as.character(days[length(days)]))

dat <- list()
for(i in 1:length(end_days)){
  dat <- c(dat, list(scrape_statcast_savant(start_days[i], end_days[i])))
}
statcast2017 <- bind_rows(dat)
write.csv(statcast2017, "data/statcast2017.csv", row.names = FALSE)
