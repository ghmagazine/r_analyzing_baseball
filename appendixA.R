# 付録A
source("scripts/parse_retrosheet_pbp.R")
parse_retrosheet_pbp(1950)
fields <- read_csv("data/fields.csv")
data <- read_csv("data/all1950.csv",
                 col_names = pull(fields, Header))
roster <- read_csv("data/roster1950.csv")

headers <- read_csv("data/fields.csv")
pbp2016 <- read_csv("data/all2016.csv",
                    col_names = pull(headers, Header))
pbp2016 <- pbp2016 %>%
  mutate(sequence = gsub("[.>123+*N]", "", PITCH_SEQ_TX))
pbp2016 <- pbp2016 %>%
  mutate(c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence))
pbp2016 <- pbp2016 %>%
  mutate(c20 = grepl("^[BIPV]2", sequence),
         c30 = grepl("^[BIPV]3", sequence),
         c02 = grepl("^[CFKLMOQRST]2", sequence))

b <- "[BIPV]"
s <- "[CFKLMOQRST]"
pbp2016 <- pbp2016 %>%
  mutate(c11 = grepl(paste0("^", s, b,
                            "|", b, s), sequence),
         c21 = grepl(paste0("^", s, b, b,
                            "|", b, s, b,
                            "|", b, b, s), sequence),
         c31 = grepl(paste0("^", s, b, b, b,
                            "|", b, s, b, b,
                            "|", b, b, s, b,
                            "|", b, b, b, s), sequence))
pbp2016 <- pbp2016 %>%
  mutate(c12 = grepl(paste0("^", b, s, s,
                            "|", s, b, s,
                            "|", s, s, "[FR]*", b), sequence),
         c22 = grepl(paste0("^", b, b, s, s,
                            "|", b, s, b, s,
                            "|", b, s, s, "[FR]*", b,
                            "|", s, b, b, s,
                            "|", s, b, s, "[FR]*", b,
                            "|", s, s, "[FR]*", b, "[FR]*", b),
                     sequence),
         c32 = grepl(paste0("^", s, "*", b, s,
                            "*", b, s, "*", b), sequence)
         & grepl(paste0("^", b, "*", s, b, "*", s),
                 sequence))
