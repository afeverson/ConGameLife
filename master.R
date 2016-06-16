##Creating a GIF of Conways Game of Life

##Opening appropriate packages
library(ggplot2)
library(reshape2)
library(animation) #this one requires ImageMagick to work

##Generates a "new" board
new.board <- function(row.col = 10, seeds = NULL) {
  if (is.null(seeds)) {
    board <- matrix(runif(row.col^2) > 0.75, nrow = row.col, ncol = row.col)
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
  n <- nrow(board)
  b <- board
  for (i in 1:n) {
    for (j in 1:n) {
      if ((b[i,j] == TRUE) && (num.neighbors(b,i,j) < 2)) {
        board[i,j] <- FALSE
      }
      if ((b[i,j] == TRUE) && (num.neighbors(b,i,j) > 3)) {
        board[i,j] <- FALSE
      }
      if ((b[i,j] == FALSE) && (num.neighbors(b,i,j) == 3)) {
        board[i,j] <- TRUE
      }
    }
  }
  board
}

##Making the board pretty with ggplot
looks <- function(board) {
  board <- melt(board)
  board$value <- factor(ifelse(board$value, "A", "D"))
  p <- ggplot(board, aes(x = Var1, y = Var2, z = value, color = value)) +
    geom_tile(aes(fill = value)) +
    scale_fill_manual(values = c("D" = "white", "A" = "green")) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  p
}

##Plays the game
play <- function(rc = 10, seeds = NULL, iter = 50) {
  b <- new.board(row.col = rc, seeds = seeds)
  n = 0
  while (n < iter) {
    b <- rules(b)
    n <- n + 1
    print(looks(b))
  }
}

ani.options()

ani.options(convert = 'C:/Program Files/ImageMagick-7.0.2-Q16/magick.exe')

saveGIF(play(rc=20,iter=20), interval = 0.5)
