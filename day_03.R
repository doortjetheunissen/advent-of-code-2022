library(data.table)
library(tidyverse)


# PART 1 ------------------------------------------------------------------
# import data
#df <- fread("day_03_input_test", header = FALSE)
df <- fread("day_03_input", header = FALSE)

# find shared item
df <- df %>%
  rowwise() %>%
  mutate(item_count = nchar(V1)) %>%
  mutate(first_compartment = str_sub(V1, 1, item_count/2),
         second_compartment = str_sub(V1, (item_count/2)+1, item_count)) %>%
  mutate(first_unique = unique(str_split(first_compartment, "")))

df$overlap <- ""

for (i in 1:nrow(df)){
    df$overlap[i] <- unique(df$first_unique[i][[1]][str_detect(df$second_compartment[i], df$first_unique[i][[1]])])
}  

# match with priority
df_prio <- as.data.frame(cbind(c(letters,LETTERS),c(1:52)))

df <- df %>%
  left_join(df_prio, by = c("overlap" = "V1"))

sum(as.numeric(df$V2))


# PART 2 ------------------------------------------------------------------
badge_vector <- c("")
for (i in 1:100){
  elf1 <- unique(str_split(df$V1[(i*3)-2], ""))[[1]]
  elf1_2 <- unique(elf1[str_detect(df$V1[(i*3)-1], elf1)])
  elf1_2_3 <- unique(elf1_2[str_detect(df$V1[i*3], elf1_2)])
  badge_vector[i] <- elf1_2_3
}

df_badge <- as.data.frame(badge_vector) %>%
  left_join(df_prio, by = c("badge_vector" = "V1"))

sum(as.numeric(df_badge$V2))
