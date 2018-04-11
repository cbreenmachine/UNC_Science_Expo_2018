rowEqualizer <- function(x1, x2, x3){
  # rowEqualizer() takes two column vectors
  # It compares the number of rows in both and outputs a 
  # pruned version so that the rows have the same size
  
  # Remove NAs
  # This assumes that there are no gaps in the data and that both vectors have the same start point
  # That could be a dangerous assumption. This particular function works for Science Expo Shiny App
  x1 <- x1[!is.na(x1)]
  x2 <- x2[!is.na(x2)]
  x3 <- x3[!is.na(x3)]
  
  # Minimum number of rows in each
  number_rows_1 = NROW(x1)
  number_rows_2 = NROW(x2)
  
  left_bound = 1
  right_bound = min(NROW(x1), NROW(x2), NROW(x3))
  
  # PRune out the superfluous entries
  x1 <- x1[left_bound:right_bound]
  x2 <- x2[left_bound:right_bound]
  x3 <- x3[left_bound:right_bound]
  
  df <- data.frame(x1,x2,x3)
  
  return(df)
  
}