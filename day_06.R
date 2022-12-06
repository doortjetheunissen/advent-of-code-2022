
## import data
signal <- readLines("day_06_input_p1")

# part 1 ------------------------------------------------------------------
first_marker <- 0
i <- 4
while(first_marker == 0){
  block <- str_sub(signal, i-3, i)
  if(length(unique(str_split(block, "")[[1]])) == 4){
    first_marker <- i
  } else {
    i <- i + 1
  }
}


# part 2 ------------------------------------------------------------------

first_marker <- 0
i <- 14
while(first_marker == 0){
  block <- str_sub(signal, i-13, i)
  if(length(unique(str_split(block, "")[[1]])) == 14){
    first_marker <- i
  } else {
    i <- i + 1
  }
}
