library(readr)
library(dplyr)
library(stringr)

## read in stack data
#stack <- readLines("day_05_input_test")[1:3] %>%
stack <- readLines("day_05_input_p1")[1:8] %>%
  paste(collapse = "\n") %>%
  read_fwf()

stack.list <- as.list(stack)

stack.list <- lapply(stack.list, rev)
stack.list <- lapply(stack.list, na.omit)

rm(stack)

## read in movement data
#movement <- as.data.frame(readLines("day_05_input_test")[6:9])
movement <- as.data.frame(readLines("day_05_input_p1")[11:511])
movement.list <- regmatches(movement[,1], gregexpr("[[:digit:]]+", movement[,1]))

rm(movement)


# part 1 ------------------------------------------------------------------
## move crates

for (i in 1:length(movement.list)){
  box_count <- as.numeric(movement.list[[i]][1])
  stack_source <- as.numeric(movement.list[[i]][2])
  stack_destination <- as.numeric(movement.list[[i]][3])
  
  for (j in 1:box_count){
    length_source <- length(stack.list[[stack_source]])
    box_to_move <- stack.list[[stack_source]][length_source]
    
    if(length_source > 1){
      stack.list[[stack_source]] <- stack.list[[stack_source]][1:(length_source - 1)]
    } else {
      stack.list[[stack_source]] <- logical(0)
    }
    
    stack.list[[stack_destination]] <- c(stack.list[[stack_destination]], box_to_move)
  }
}


stack.list


# part 2 ------------------------------------------------------------------
## move crates

for (i in 1:length(movement.list)){
  box_count <- as.numeric(movement.list[[i]][1])
  stack_source <- as.numeric(movement.list[[i]][2])
  stack_destination <- as.numeric(movement.list[[i]][3])
  
  length_source <- length(stack.list[[stack_source]])
  boxes_to_move <- stack.list[[stack_source]][(length_source - box_count + 1):length_source]
  
  if(length_source > box_count){
    stack.list[[stack_source]] <- stack.list[[stack_source]][1:(length_source - box_count)]
  } else {
    stack.list[[stack_source]] <- logical(0)
  }
  
  stack.list[[stack_destination]] <- c(stack.list[[stack_destination]], boxes_to_move)
}


stack.list

