library(Lahman)
library(tidyverse)
ws <- filter(SeriesPost, yearID >= 1903,
             round == "WS", wins + losses < 8)
ggplot(ws, aes(x = wins + losses)) +
  geom_bar(fill = "blue") +
  labs(x = "Number of games", y = "Frequency")
