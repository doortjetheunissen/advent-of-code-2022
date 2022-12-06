library(data.table)
library(dplyr)


# import data -------------------------------------------------------------
#df <- fread("day_04_input_test", header = FALSE)
df <- fread("day_04_input_p1", header = FALSE)


# part 1 ------------------------------------------------------------------
df <- df %>%
  rowwise() %>%
  mutate(split1_1 = as.numeric(str_split(V1, "-")[[1]][1]),
         split1_2 = as.numeric(str_split(V1, "-")[[1]][2]),
         split2_1 = as.numeric(str_split(V2, "-")[[1]][1]),
         split2_2 = as.numeric(str_split(V2, "-")[[1]][2]),
         length1 = length(split1_1:split1_2),
         length2 = length(split2_1:split2_2))

for (i in 1:nrow(df)){
  if (df$length1[i] > df$length2[i]){
    df$contain[i] <- (df$split2_1[i] >= df$split1_1[i]) & (df$split2_2[i] <= df$split1_2[i])
  } else {
    df$contain[i] <- (df$split1_1[i] >= df$split2_1[i]) & (df$split1_2[i] <= df$split2_2[i])
  }
}

sum(df$contain)


# part 2 ------------------------------------------------------------------
# using output df of part 1

for (i in 1:nrow(df)){
  df$overlap[i] <- ((df$split1_1[i] >= df$split2_1[i]) & (df$split1_1[i] <= df$split2_2[i])) |
    ((df$split1_2[i] >= df$split2_1[i]) & (df$split1_2[i] <= df$split2_2[i])) |
    df$contain[i]
}

sum(df$overlap)  
