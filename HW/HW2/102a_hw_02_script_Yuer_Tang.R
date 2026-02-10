## Part 1: create the object board:

## Part 2: Robustly show board each time, then the logic should be:
## 1. show the grid 2. show the number on the grid, 3. add on ladders 4. add on chutes
show_board<- function(board){
  # To make it robust
  n_row <- board$n_row
  n_col <- board$n_col
  # Set up blank canvas
  plot.new()
  plot.window(xlim=c(0,n_col), ylim=c(0,n_row), asp=1)
  # Draw the vertical and horizontal line to segment the grid
  for (i in 0:n_col) {
    segments(x0=i, y0=0, x1=i, y1=n_row)
  }
  for (j in 0:n_row) {
    segments(x0=0, y0=j, x1=n_col, y1=j)
  }
  # Putting on the number requires to think about the snake pattern on it, observe the even and odd row
  # For odd row, -> even row <-
  # Beside knowing the order, we need to put number on the grid by text(x,y,label) so we should figure out x,y
  # We first build the number grid then so we know the value, then afterward, we have the value, we local the value to the coordinagte we created by plot function
  grid <- matrix(NA, nrow = n_row, ncol = n_col)
  for (row in 1:n_row) {
    # If it's increasing row, the start value is the last row* n_col +1
    # If it's decreasing row, the start value is the current row*n_col -1
    if (row%%2 == 1) {
      start <- (row - 1)*n_col + 1
      grid[row, ] <- start:(start + n_col - 1)
    } else {
      start <- row * n_col
      grid[row, ] <- start:(start -n_col + 1)
    }
  }
  for (row in 1:n_row) {
    for (col in 1:n_col) {
      # we will put it in the middle, so -0.5
      # col-0.5 is the coordinate
      text(col-0.5, row-0.5, labels = grid[row, col])
    }
  }
  # Now, to show the ladder, we fit in arrow(x0,y0,x1,y1,color, lwd) where the start(x0,y0) and end(x0,y0) are stored as dataframe in baord$ladders
  for (i in seq_along(board$ladder$start)){
    start_number <- board$ladder$start[i]
    end_number <- board$ladder$end[i]
    
    # we want to track the coords of the start and end letters in the board x,y, so it's mapping the number grid with the plot coordinates
    start_position <- which(grid ==start_number, arr.ind = TRUE)# arr.ind turns the row and col to us which is y axis, and x axis
    # However, the position on grid needs correction to move it onto the coords because number is plotted at the center of the board
    x0 <- start_position[1,"col"] - 0.5
    y0 <- start_position[1, "row"] -0.5
    # Same with end pos
    end_position <- which(grid ==end_number, arr.ind = TRUE)
    x1 <- end_position[1,"col"] - 0.5
    y1 <- end_position[1, "row"] -0.5
    
    arrows(x0,y0,x1,y1,col="green",lwd=2)
    # Exactly the same logic with chutes
  for (i in seq_along(board$chutes$start)){
    start_number <- board$chutes$start[i]
    end_number <- board$chutes$end[i]
    start_position <- which(grid==start_number, arr.ind=TRUE)
    x0 <- start_position[1,"col"] - 0.5
    y0 <- start_position[1, "row"] -0.5
    end_position <- which(grid ==end_number, arr.ind = TRUE)
    x1 <- end_position[1,"col"] - 0.5
    y1 <- end_position[1, "row"] -0.5
    arrows(x0,y0,x1,y1,col="red",lwd=2)
  }
    
  }
}

# Part 4, play with yourself!

play_solo <- function(board, verbose = FALSE) {
  
  n_row <- board$n_row
  n_col <- board$n_col
  winning_square <- n_row * n_col
  
  pawn <- 0
  turn <- 0
  move_log <- c()
  chute_tally <- rep(0, nrow(board$chutes))
  ladder_tally <- rep(0, nrow(board$ladders))
  
  while (pawn < winning_square) {
    turn <- turn + 1
    if (verbose) {
      cat("Turn", turn, "\n")
      cat("Start at", pawn, "\n")
    }
    spin_value <- sample(6, 1)
    if (verbose) {
      cat("Spinner:", spin_value, "\n")
    }
    new_position <- pawn + spin_value
    
    if (new_position > winning_square){
      if (verbose) {
        cat("Turn ends at:", new_position )
      }
      move_log <- c(move_log, pawn)
      next
    }
    
    pawn <- new_position
    ladder_idx <- which(board$ladders$start == pawn)
    if (length(ladder_idx) > 0) {
      if (verbose) {
        cat("Ladder!")
      }
      ladder_tally[ladder_idx] <- ladder_tally[ladder_idx] + 1
      pawn <- board$ladders$end[ladder_idx]
    }
    chute_idx <- which(board$chutes$start == pawn)
    if (length(chute_idx) > 0) {
      if (verbose) {
        cat("Chutes!")
      }
      chute_tally[chute_idx] <- chute_tally[chute_idx] + 1
      pawn <- board$chutes$end[chute_idx]
    }
    
    if (verbose) {
      cat("Turn ends at:" , pawn, "\n")
    }
    move_log <- c(move_log, pawn)
  }
  
  return(list(
    turns = turn,
    chute_tally = chute_tally,
    ladder_tally = ladder_tally,
    move_log = move_log
  ))
}