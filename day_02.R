library(data.table)
library(dplyr)
library(stringr)

# part 1 ------------------------------------------------------------------
# A for Rock, B for Paper, and C for Scissors.
# X for Rock, Y for Paper, and Z for Scissors
# points:
# 1 for Rock, 2 for Paper, and 3 for Scissors
#0 if you lost, 3 if the round was a draw, and 6 if you won

df <- fread("day_02_input_p1", header = FALSE)

df <- df %>%
  mutate(outcome_points = ifelse((V1 == "A" & V2 == "X") | (V1 == "B" & V2 == "Y") | (V1 == "C" & V2 == "Z"), 3,
                                 ifelse((V1 == "A" & V2 == "Z") | (V1 == "B" & V2 == "X") | (V1 == "C" & V2 == "Y"), 0, 6))) %>%
  mutate(shape_points = ifelse(V2 == "X", 1,
                               ifelse(V2 == "Y", 2, 3))) %>%
  mutate(round_points = outcome_points + shape_points)

sum(df$round_points)


# part 2 ------------------------------------------------------------------

df <- fread("day_02_input_p1", header = FALSE)

df <- df %>%
  mutate(outcome_points = ifelse(V2 == "X", 0,
                               ifelse(V2 == "Y", 3, 6))) %>%
  mutate(shape_points = ifelse((V1 == "A" & V2 == "Y") | (V1 == "B" & V2 == "X") | (V1 == "C" & V2 == "Z"), 1,
                                 ifelse((V1 == "A" & V2 == "Z") | (V1 == "B" & V2 == "Y") | (V1 == "C" & V2 == "X"), 2, 3))) %>%
  mutate(round_points = outcome_points + shape_points)

sum(df$round_points)
