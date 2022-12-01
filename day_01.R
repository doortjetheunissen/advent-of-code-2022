library(data.table)
library(dplyr)


# part 1 ------------------------------------------------------------------
df <- fread("day_01_input_p1") %>%
  mutate(new_elf = ifelse(is.na(V1), 1, 0), elf_nr = 1)

for (i in 2:nrow(df)){
    df$elf_nr[i] <- df$elf_nr[i-1] + df$new_elf[i]
}

df_complete <- df[complete.cases(df),] %>%
  group_by(elf_nr) %>%
  summarise(total_calories = sum(V1)) %>%
  slice(which.max(total_calories))


# part 2 ------------------------------------------------------------------'
rm(list = ls())

df <- fread("day_01_input_p1") %>%
  mutate(new_elf = ifelse(is.na(V1), 1, 0), elf_nr = 1)

for (i in 2:nrow(df)){
  df$elf_nr[i] <- df$elf_nr[i-1] + df$new_elf[i]
}

df_complete <- df[complete.cases(df),] %>%
  group_by(elf_nr) %>%
  summarise(total_calories = sum(V1)) %>%
  arrange(desc(total_calories))

top3 <- df_complete[c(1:3),]
sum(top3$total_calories)
