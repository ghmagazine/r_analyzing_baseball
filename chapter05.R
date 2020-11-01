library(tidyverse)
library(Lahman)
fields <- read_csv("data/fields.csv")
data2016 <- read_csv("data/all2016.csv",
                     col_names = pull(fields, Header),
                     na = character())

data2016 %>%
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED =
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) -> data2016
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
  mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
  data2016

data2016 %>%
  mutate(BASES =
           paste(ifelse(BASE1_RUN_ID > '', 1, 0),
                 ifelse(BASE2_RUN_ID > '', 1, 0),
                 ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) ->
  data2016

data2016 %>%
  mutate(NRUNNER1 =
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
         NEW.STATE = paste(NEW.BASES, NOUTS)) ->
  data2016

data2016 %>%
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) ->
  data2016
data2016 %>%
  filter(Outs.Inning == 3) -> data2016C
data2016C %>%
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS
RUNS_out <- matrix(round(RUNS$Mean, 2), 8, 3)
dimnames(RUNS_out)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(RUNS_out)[[1]] <- c("000", "001", "010", "011",
                             "100", "101", "110", "111")
RUNS.2002 <- matrix(c(.51, 1.40, 1.14, 1.96, .90, 1.84, 1.51,
                      2.33, .27, .94, .68, 1.36, .54, 1.18,
                      .94, 1.51, .10, .36, .32, .63, .23,
                      .52, .45, .78), 8, 3)
dimnames(RUNS.2002) <- dimnames(RUNS_out)
cbind(RUNS_out, RUNS.2002)

data2016 %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs),
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> data2016
Master %>%
  filter(nameFirst == "Jose", nameLast == "Altuve") %>%
  pull(retroID) -> altuve.id
data2016 %>%
  filter(BAT_ID == altuve.id,
         BAT_EVENT_FL == TRUE) -> altuve
altuve %>%
  select(STATE, NEW.STATE, run_value) %>%
  slice(1:3)
altuve %>%
  group_by(BASES) %>%
  summarize(N = n())
ggplot(altuve, aes(BASES, run_value)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  geom_hline(yintercept = 0, color = crcblue) +
  xlab("RUNNERS")
altuve %>%
  group_by(BASES) %>%
  summarize(RUNS = sum(run_value),
            PA = n()) -> Runs_Altuve
Runs_Altuve
Runs_Altuve %>% summarize(RE24 = sum(RUNS))

data2016 %>% filter(BAT_EVENT_FL == TRUE) -> data2016b
data2016b %>%
  group_by(BAT_ID) %>%
  summarize(RE24 = sum(run_value),
            PA = length(run_value),
            Runs.Start = sum(Runs.State)) -> runs
runs %>%
  filter(PA >= 400) -> runs400
head(runs400)
ggplot(runs400, aes(Runs.Start, RE24)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, color = crcblue) -> plot1
plot1
runs400 %>%
  inner_join(Master, by = c("BAT_ID" = "retroID")) -> runs400
library(ggrepel)
plot1 +
  geom_text_repel(data = filter(runs400, RE24 >= 40),
                  aes(label = nameLast))

data2016 %>%
  inner_join(runs400, by = "BAT_ID") -> regulars
regulars %>%
  group_by(BAT_ID, BAT_LINEUP_ID) %>%
  summarize(N = n()) %>%
  arrange(desc(N)) %>%
  mutate(Position = first(BAT_LINEUP_ID)) -> positions
runs400 %>%
  inner_join(positions, by = "BAT_ID") -> runs400
ggplot(runs400, aes(Runs.Start, RE24, label = Position)) +
  geom_text() +
  geom_hline(yintercept = 0, color = crcblue) +
  geom_point(data = filter(runs400, BAT_ID == altuve.id),
             size = 4, shape = 16, color = crcblue)

data2016 %>% filter(EVENT_CD == 23) -> home_runs
home_runs %>%
  select(STATE) %>%
  table()
home_runs %>%
  select(STATE) %>%
  table() %>%
  prop.table() %>%
  round(3)
mean_hr <- home_runs %>%
  summarize(mean_run_value = mean(run_value))
mean_hr

ggplot(home_runs, aes(run_value)) +
  geom_histogram() +
  geom_vline(data = mean_hr, aes(xintercept = mean_run_value),
             color = crcblue, size = 1.5) +
  annotate("text", 1.7, 2000,
           label = "Mean Run \ nValue", color = crcblue)
home_runs %>%
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  head(1)

data2016 %>%
  filter(EVENT_CD == 20) -> singles

mean_singles <- singles %>%
  summarize(mean_run_value = mean(run_value))
ggplot(singles, aes(run_value)) +
  geom_histogram(bins = 40) +
  geom_vline(data = mean_singles, color = crcblue,
             aes(xintercept = mean_run_value), size = 1.5) +
  annotate("text", 0.8, 4000,
           label = "Mean Run \ nValue", color = crcblue)
singles %>%
  select(STATE) %>%
  table()
singles %>%
  arrange(desc(run_value)) %>%
  select(STATE, NEW.STATE, run_value) %>%
  slice(1)
singles %>%
  arrange(run_value) %>%
  select(STATE, NEW.STATE, run_value) %>%
  slice(1)
data2016 %>%
  filter(EVENT_CD %in% c(4, 6)) -> stealing
stealing %>%
  group_by(EVENT_CD) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))
stealing %>% group_by(STATE) %>% summarize(N = n())
ggplot(stealing, aes(run_value, fill = factor(EVENT_CD))) +
  geom_histogram() +
  scale_fill_manual(name = "EVENT_CD",
                    values = c(crcblue, "gray70"),
                    labels = c("Stolen Base (SB)",
                               "Caught Stealing (CS)"))
stealing %>% filter(STATE == "100 1") -> stealing.1001
stealing.1001 %>%
  group_by(EVENT_CD) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))

stealing.1001 %>%
  group_by(NEW.STATE) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))
stealing.1001 %>% summarize(Mean = mean(run_value))
