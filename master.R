##Generates a "new" board
new.board <- function(nrow = 25, ncol = 25, seeds = NULL) {
  if (seeds == NULL) {
    seeds <- (matrix(runif(nrow*ncol) > 0.5, nrow = nrow, ncol = ncol))
  }
  board <- matrix(FALSE, nrow = nrow, ncol = ncol)
  for(i in seq_along(seeds)) {
    board[seeds[[i]][1],seeds[[i]][2]] <- TRUE
  }
  board
}

##Returns living neighbors of a cell
num.neighbors <- function() {
  
}

##Implementation of the rules
rules <- function() {
  
}

##Generates the next iteration of the board
next.board <- function() {
  
}

##Plays the game

