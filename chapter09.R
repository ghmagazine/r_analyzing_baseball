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
  while (s < 25)
    s.new <- sample(1:25, size = 1, prob = P[s, ])
  path <- c(path, s.new)
  runs <- runs + R[s, s.new]
  s <- s.new
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


