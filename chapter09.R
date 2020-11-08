# 9章
library(tidyverse)
fields <- read_csv("data/fields.csv")
data2016 <- read_csv("data/all2016.csv",
                     col_names = pull(fields, Header),
                     na = character())
data2016 %>%
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED =
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
  data2016
data2016 %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) ->
  half_innings

data2016 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(BASES =
           paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                 ifelse(BASE2_RUN_ID > '', 1, 0),
                 ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT),
         NRUNNER1 =
           as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 =
           as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 |
                        BAT_DEST_ID == 2),
         NRUNNER3 =
           as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                        RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2,
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> data2016

data2016 %>%
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) -> data2016
data2016 %>%
  filter(Outs.Inning == 3, BAT_EVENT_FL == TRUE) -> data2016C
data2016C <- data2016C %>%
  mutate(NEW.STATE = gsub("[0-1]{3} 3", "3", NEW.STATE))

T_matrix <- data2016C %>%
  select(STATE, NEW.STATE) %>%
  table()
P_matrix <- prop.table(T_matrix, 1)
P_matrix <- rbind(P_matrix, c(rep(0, 24), 1))
P_matrix %>%
  as_tibble(rownames = "STATE") %>%
  filter(STATE == "000 0") %>%
  gather(key = "NEW.STATE", value = "Prob", -STATE) %>%
  filter(Prob > 0)

P_matrix %>%
  as_tibble(rownames = "STATE") %>%
  filter(STATE == "010 2") %>%
  gather(key = "NEW.STATE", value = "Prob", -STATE) %>%
  filter(Prob > 0)

count_runners_out <- function(s){
  s %>%
    str_split("") %>%
    pluck(1) %>%
    as.numeric() %>%
    sum(na.rm = TRUE)
}

runners_out <- sapply(row.names(T_matrix),
                      count_runners_out)[-25]
R <- outer(runners_out + 1, runners_out, FUN = "-")
names(R) <- names(T_matrix)[-25]
R <- cbind(R, rep(0, 24))

simulate_half_inning <- function(P, R, start = 1){
  s <- start
  path <- NULL
  runs <- 0
  while (s < 25){
    s.new <- sample(1:25, size = 1, prob = P[s, ])
    path <- c(path, s.new)
    runs <- runs + R[s, s.new]
    s <- s.new
  }
  runs
}

set.seed(111653)
RUNS <- replicate(10000, simulate_half_inning(T_matrix, R))
table(RUNS)
sum(RUNS >= 5) / 10000
mean(RUNS)

RUNS.j <- function(j){
  mean(replicate(10000, simulate_half_inning(T_matrix, R, j)))
}
RE_bat <- sapply(1:24, RUNS.j) %>%
  matrix(nrow = 8, ncol = 3, byrow = TRUE,
         dimnames = list(c("000", "001", "010", "011",
                           "100", "101", "110", "111"),
                         c("0 outs", "1 out", "2 outs")))
round(RE_bat, 2)

RE <- matrix(
  c(0.47, 0.25, 0.10, 1.45, 0.94, 0.32,
    1.06, 0.65, 0.31, 1.93, 1.34, 0.54,
    0.84, 0.50, 0.22, 1.75, 1.15, 0.49,
    1.41, 0.87, 0.42, 2.17, 1.47, 0.76),
  8, 3, byrow = TRUE)
round(RE - RE_bat, 2)

P_matrix_3 <- P_matrix %*% P_matrix %*% P_matrix
P_sorted <- P_matrix_3 %>%
  as_tibble(rownames = "STATE") %>%
  filter(STATE == "000 0") %>%
  gather(key = "NEW.STATE", value = "Prob", -STATE) %>%
  arrange(desc(Prob))
P_sorted %>% head()
Q <- P_matrix[-25, -25]
N <- solve(diag(rep(1, 24)) - Q)

N.0000 <- round(N["000 0", ], 2)
head(data.frame(N = N.0000))

sum(N.0000)
Length <- round(t(N %*% rep(1, 24)), 2)
data.frame(Length = Length[1, 1:8])
data2016C %>%
  mutate(HOME_TEAM_ID = str_sub(GAME_ID, 1, 3),
         BATTING.TEAM = ifelse(BAT_HOME_ID == 0,
                               AWAY_TEAM_ID, HOME_TEAM_ID)) ->
  data2016C
data2016C %>%
  group_by(BATTING.TEAM, STATE, NEW.STATE) %>%
  count() -> Team.T
Team.T %>%
  filter(BATTING.TEAM == "ANA") %>%
  head()

data2016C %>%
  filter(STATE == "100 2") %>%
  group_by(BATTING.TEAM, STATE, NEW.STATE) %>%
  tally() -> Team.T.S
Team.T.S %>%
  ungroup() %>%
  sample_n(size = 6)

Team.T.S %>%
  filter(BATTING.TEAM == "WAS") %>%
  mutate(p = n / sum(n)) -> WAS.Trans
data2016C %>%
  filter(STATE == "100 2") %>%
  group_by(NEW.STATE) %>%
  tally() %>%
  mutate(p = n / sum(n)) -> ALL.Trans
WAS.Trans %>%
  inner_join(ALL.Trans, by = "NEW.STATE") %>%
  mutate(p.EST = n.x / (1274 + n.x) * p.x +
           1274 / (1274 + n.x) * p.y) %>%
  select(BATTING.TEAM, NEW.STATE, p.x, p.y, p.EST)

make_schedule <- function(teams, k){
  n.teams <- length(teams)
  Home <- rep(rep(teams, each = n.teams), k)
  Visitor <- rep(rep(teams, n.teams), k)
  schedule <- tibble(Home = Home, Visitor = Visitor) %>%
    filter(Home != Visitor)
}
NL <- c("ATL", "CHN", "CIN", "HOU", "LAN",
        "NYN", "PHI", "PIT", "SFN", "SLN")
AL <- c("BAL", "BOS", "CAL", "CHA", "CLE",
        "DET", "MIN", "NYA", "OAK", "WS2")
teams <- c(NL, AL)
league <- c(rep(1, 10), rep(2, 10))
schedule <- bind_rows(make_schedule(NL, 9),
                      make_schedule(AL, 9))

s.talent <- 0.20
talents <- rnorm(20, 0, s.talent)
TAL <- tibble(Team = teams, League = league,
              Talent = talents)
SCH <- schedule %>%
  inner_join(TAL, by = c("Home" = "Team")) %>%
  rename(Talent.Home = Talent) %>%
  inner_join(TAL, by = c("Visitor" = "Team", "League")) %>%
  rename(Talent.Visitor = Talent)

SCH %>%
  mutate(prob.Home = exp(Talent.Home) /
           (exp(Talent.Home) + exp(Talent.Visitor))) -> SCH

head(SCH)
SCH %>%
  mutate(outcome = rbinom(nrow(.), 1, prob.Home),
         winner = ifelse(outcome, Home, Visitor)) -> SCH
SCH %>%
  select(Visitor, Home, prob.Home, outcome, winner) %>%
  head()

SCH %>%
  group_by(winner) %>%
  summarize(Wins = n()) %>%
  inner_join(TAL, by = c("winner" = "Team")) -> RESULTS

win_league <- function(RR) {
  out <- RR %>%
    mutate(Winner.Lg = 0,
           prob = exp(Talent),
           outcome = sample(nrow(.), prob = prob)) %>%
    arrange(League, desc(Wins), outcome) %>%
    select(-outcome)
  out[1 + c(0, nrow(RR) / 2), "Winner.Lg"] <- 1
  out
}

RESULTS <- win_league(RESULTS)
ws_winner <- RESULTS %>%
  filter(Winner.Lg == 1) %>%
  mutate(outcome = rmultinom(1, 7, prob),
         Winner.WS = ifelse(outcome > 3, 1, 0)) %>%
  filter(outcome > 3) %>%
  select(winner, Winner.WS)
RESULTS %>%
  left_join(ws_winner, by = c("winner")) %>%
  replace_na(list(Winner.WS = 0))

source("scripts/one.simulation.68.R")
set.seed(111653)
RESULTS <- one.simulation.68(0.20)
RESULTS
display_standings <- function(data, league) {
  data %>%
    filter(League == league) %>%
    select(Team, Wins) %>%
    mutate(Losses = 162 - Wins) %>%
    arrange(desc(Wins))
}
map(1:2, display_standings, data = RESULTS) %>%
  bind_cols()
RESULTS %>%
  filter(Winner.Lg == 1) %>%
  select(Team, Winner.WS)
set.seed(111653)
Many.Results <- map_df(rep(0.20, 1000),
                       one.simulation.68)
ggplot(Many.Results, aes(Talent, Wins)) +
  geom_point(alpha = 0.05)
crcblue <- "#2905a1"
ggplot(filter(Many.Results, Talent > -0.05, Talent < 0.05),
       aes(Wins)) +
  geom_histogram(color = crcblue, fill = "white")
fit1 <- glm(Winner.Lg ~ Talent,
            data = Many.Results, family = binomial)
fit2 <- glm(Winner.WS ~ Talent,
            data = Many.Results, family = binomial)
talent_values <- seq(-0.4, 0.4, length.out = 100)
tdf <- tibble(Talent = talent_values)
df1 <- tibble(
  Talent = talent_values,
  Probability = predict(fit1, tdf, type = "response"),
  Outcome = "Pennant")
df2 <- tibble(
  Talent = talent_values,
  Probability = predict(fit2, tdf, type = "response"),
  Outcome = "World Series")
ggplot(bind_rows(df1, df2),
       aes(Talent, Probability, linetype = Outcome)) +
  geom_line() + ylim(0, 1)
