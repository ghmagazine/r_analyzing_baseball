# 2章
install.packages("tidyverse")
library(tidyverse)

spahn <- read_csv("data/spahn.csv")

spahn %>% slice(1:3) %>% select(1:10)
spahn %>% slice(1:10) %>% select(Age, W, L, ERA)

spahn %>%
  summarize(LO = min(ERA),
            QL = quantile(ERA, .25), QU = quantile(ERA, .75),
            M = median(ERA), HI = max(ERA))

spahn %>% filter(ERA == min(ERA)) %>% select(Age)

spahn %>%
  mutate(FIP = (13 * HR + 3 * BB - 2 * SO) / IP) -> spahn

spahn %>%
  arrange(FIP) %>%
  select(Year, Age, W, L, ERA, FIP) %>%
  head()

spahn %>% filter(Tm == "BSN" | Tm == "MLN") -> spahn1

spahn1 <- spahn1 %>%
  mutate(Tm = factor(Tm, levels = c("BSN", "MLN")))

spahn1 %>%
  group_by(Tm) %>%
  summarize(mean_W.L = mean(W.L, na.rm = TRUE),
            mean_ERA = mean(ERA),
            mean_WHIP = mean(WHIP),
            mean_FIP = mean(FIP))

NLbatting <- read_csv("data/NLbatting.csv")
ALbatting <- read_csv("data/ALbatting.csv")
batting <- bind_rows(NLbatting, ALbatting)
NLpitching <- read_csv("data/NLpitching.csv")
NL <- inner_join(NLbatting, NLpitching, by = "Tm")
NLbatting %>% filter(HR > 150) -> NL_150


W <- c(8, 21, 15, 21, 21, 22, 14)
L <- c(5, 10, 12, 14, 17, 14, 19)
l # error

Win.Pct <- 100 * W / (W + L)
Win.Pct

Year <- seq(from = 1946, to = 1952)
Year
Year <- 1946 : 1952
Age <- Year - 1921
plot(Age, Win.Pct)

mean(Win.Pct)
100 * sum(W) / (sum(W) + sum(L))

sort(W)
cumsum(W)
summary(Win.Pct)

W[c(1, 2, 5)]
W[1 : 4]
W[-c(1, 6)]
Win.Pct > 60
(W > 20) & (Win.Pct > 60)
Win.Pct == max(Win.Pct)
Year[Win.Pct == max(Win.Pct)]
Year[W + L > 30]

Year <- 2008 : 2017
NL <- c("PHI", "PHI", "SFN", "SLN", "SFN",
        "SLN", "SFN", "NYN", "CHN", "LAN")
AL <- c("TBA", "NYA", "TEX", "TEX", "DET",
        "BOS", "KCA", "KCA", "CLE", "HOU")
Winner <- c("NL", "AL", "NL", "NL", "NL",
            "AL", "NL", "AL", "NL", "AL")
N_Games <- c(5, 6, 5, 7, 4, 7, 7, 5, 7, 7)


WS_results <- tibble(
  Year = Year, NL_Team = NL, AL_Team = AL,
  N_Games = N_Games, Winner = Winner)
WS_results
grep("NY", c(AL, NL), value = TRUE)

WS_results %>%
  group_by(Winner) %>%
  summarize(N = n()) -> WS
WS

ggplot(WS, aes(x = Winner, y = N)) +
  geom_col()

WS_results %>%
  group_by(NL_Team) %>%
  summarize(N = n())

WS_results <- WS_results %>%
  mutate(NL_Team = factor(NL_Team,
                          levels = c("NYN", "PHI", "CHN",
                                     "SLN", "LAN", "SFN")))
str(WS_results$NL_Team)

WS_results %>%
  group_by(NL_Team) %>%
  summarize(N = n())

world_series <- list(Winner = Winner, Number.Games = N_Games,
                     Seasons = "2008 to 2017")
world_series$Number.Games
world_series[[2]]
pluck(world_series, "Number.Games")
world_series["Number.Games"]

WS_results$NL_Team
pull(WS_results, NL_Team)

library(Lahman)
ws <- filter(SeriesPost, yearID >= 1903,
             round == "WS", wins + losses < 8)
crcblue <- "#2905a1"
ggplot(ws, aes(x = wins + losses)) +
  geom_bar(fill = crcblue) +
  labs(x = "Number of games", y = "Frequency")

source("scripts/WorldSeriesLength.R", echo = TRUE)

hr_rates <- function(age, hr, ab){
  rates <- round(100 * hr / ab, 1)
  list(x = age, y = rates)
}
source("scripts/hr_rates.R")

HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
hr_rates(Age, HR, AB)
plot(hr_rates(Age, HR, AB))

getwd()
spahn <- read_csv("data/spahn.csv")
HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29
hr_rates <- hr_rates(Age, HR, AB)
Mantle <- data.frame(Age, HR, AB, Rates = hr_rates$y)
write_csv(Mantle, "data/mantle.csv")
list.files("data", pattern = "mantle")

install.packages("Lahman")
library(Lahman)
?Batting

Batting %>%
  filter(yearID >= 1960, yearID <= 1969) -> Batting_60
Batting_60 %>%
  group_by(playerID) %>%
  summarize(HR = sum(HR)) -> hr_60
hr_60 %>%
  arrange(desc(HR)) -> hr_60
head(hr_60)

hr_leader <- function(data){
  data %>%
    group_by(playerID) %>%
    summarize(HR = sum(HR)) %>%
    arrange(desc(HR)) %>%
    head(1)
}

Batting %>%
  mutate(decade = 10 * floor(yearID / 10)) %>%
  split(pull(., decade)) %>%
  map_df(hr_leader, .id = "decade")

Batting %>%
  group_by(playerID) %>%
  summarize(tAB = sum(AB, na.rm = TRUE),
            tHR = sum(HR, na.rm = TRUE),
            tSO = sum(SO, na.rm = TRUE)) -> long_careers
Batting_5000 <- filter(long_careers, tAB >= 5000)
head(Batting_5000)

ggplot(Batting_5000, aes(x = tHR / tAB, y = tSO / tAB)) +
  geom_point() + geom_smooth()
?geom_point
??geom_point


