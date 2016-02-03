##Generates a "new" board
new.board <- function(row.col = 10, seeds = NULL) {
  if (is.null(seeds)) {
    board <- matrix(runif(row.col^2) > 0.85, nrow = row.col, ncol = row.col)
  }
  else {
    board <- matrix(FALSE, nrow = row.col, ncol = row.col)
    for (i in 1:(length(seeds)/2)) {
      x <- seeds[2*i-1]
      y <- seeds[2*i]
      board[x, y] <- TRUE
    }
  }
  board
}

##Returns living neighbors of a cell
num.neighbors <- function(board, a, b) {
  row.col = nrow(board)
  neighbors <- 0
  if (a > 1) {
    neighbors <- neighbors + board[a-1, b]
    if (b > 1) {neighbors <- neighbors + board[a-1, b-1]}
    if (b < row.col) {neighbors <- neighbors + board[a-1, b+1]}
  }
  if (a < row.col) {
    neighbors <- neighbors + board[a+1, b]
    if (b > 1) {neighbors <- neighbors + board[a+1, b-1]}
    if (b < row.col) {neighbors <- neighbors + board[a+1, b+1]}
  }
  if (b > 1) {neighbors <- neighbors + board[a, b-1]}
  if (b < row.col) {neighbors <- neighbors + board[a, b+1]}
  
  neighbors
}

##Implementation of the rules
rules <- function(board) {
  for (i in 1:nrow(board)){
    for (j in 1:ncol(board)){
      n <- num.neighbors(board, i, j)
      if (board[i,j] == TRUE & n < 2) {
        board[i,j] <- FALSE
      }
      else if (board[i,j] == TRUE & n > 3) {
        board[i,j] <- FALSE
      }
      else if (board[i,j] == FALSE & n == 3) {
        board[i,j] <- TRUE
      }
    }
  }
}

##Generates the next iteration of the board
next.board <- function() {
  
}

##Plays the game

