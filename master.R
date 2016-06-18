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


ani.options(convert = 'C:/Program Files/ImageMagick-7.0.2-Q16/magick.exe')

##Good random sized board
saveGIF(play(rc=20,iter=50), interval=0.1)

##Glider that goes from bottom left to top right
saveGIF(play(rc=10,seeds=c(1,1,1,3,2,2,2,3,3,2),iter=31), interval=0.2)

##Pulsar
saveGIF(play(rc=15,seeds=c(2,4,2,5,2,6,2,10,2,11,2,12,
                           7,4,7,5,7,6,7,10,7,11,7,12,
                           9,4,9,5,9,6,9,10,9,11,9,12,
                           14,4,14,5,14,6,14,10,14,11,14,12,
                           4,2,4,7,4,9,4,14,
                           5,2,5,7,5,9,5,14,
                           6,2,6,7,6,9,6,14,
                           10,2,10,7,10,9,10,14,
                           11,2,11,7,11,9,11,14,
                           12,2,12,7,12,9,12,14
                           ),iter=12), interval=0.25)

##Big board (random seeds)
saveGIF(play(rc=100,iter=50), interval=0.1)
