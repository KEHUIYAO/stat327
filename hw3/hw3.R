# Name: KEHUI YAO
# Email:kyao24@wisc.edu

rm(list = ls())

# Implement Connect Four in the same manner that we
# implemented Tic-tac-toe in class. Start by implementing
# the helper functions, below, and testing them by running
#   source("hw3test.R")
# Then write code for the game itself.
#
# We'll test your code by running
#   source("hw3.R")
# We might also play Connect Four, read your code, and do other tests.

# Returns TRUE if vector v (of character strings) contains
# (at least) four in a row of player (character string). e.g.
#   four.in.a.row("X", c("O","X","X","X","X","O"))
# is TRUE, while
#   four.in.a.row("O", c("O","X","X","X","X","O"))
# is FALSE.
four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  total.set.length=length(v)-3
  if (total.set.length<1) return (FALSE)
  total.set=list()
  for (i in 1:total.set.length){
    total.set[[i]]=v[i:(i+3)]
  }
  test.set=rep(player,4)
  test.result=unlist(lapply(total.set,function(x){
    if(sum(test.set==x)==4){return (TRUE)}else{return (FALSE)}
  }))
  if (sum(test.result)!=0) return (TRUE)
  return(FALSE) 
}

# Returns TRUE if (matrix) board (of character strings)
# contains at least four in a row of (string) player, who
# just played in position (r, c). (Here "r" means "row" and
# "c" means "column").
#
# Hint: this function should call four.in.a.row() four times, once
# each for the current row, column, diagonal, and reverse diagonal.
won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  
  v1=board[r,]
  v2=board[,c]
  v3=board[row(board)-col(board)==r-c]
  v4=board[row(board)+col(board)==r+c]
  
  judge.list=list(v1,v2,v3,v4)
  win_or_not=sum(unlist(lapply(judge.list,function(x) four.in.a.row(player,x))))
  if (win_or_not!=0) return (TRUE)
  
  return(FALSE) 
}

# Returns largest index of an empty position in column col
# of (matrix) board. If there is no such empty position in
# board, return value is NULL.
largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  col.set=board[,col]
  
  get.index=function(x){
    len=length(x)
    for (i in 1:length(x)){
      if (x[i]=="E"|x[i]==""){next}else{
        if((i-1)!=0){return(i-1)}else{
          return (NULL)
        }
      }
    }
    return (len)
  }
  
  return(get.index(col.set)) 
}

source("hw3test.R") # Run tests on the functions above.

# ... your code to implement Connect Four using the
# functions above ...


# Hint: this program is modeled on the tic-tac-toe program we did in
# class, so studying the latter program is worthwhile.

# Note that a user click in a column indicates that a checker should
# go to that column's lowest empty row (unless the column is full).

# Note that you should implement a computer player. At the least, it
# should choose randomly from among the non-full columns. (Feel free
# to do much more! If your computer player beats me on its first try,
# you will earn a package of M&Ms. This is a hard task. Feel free to
# ask for tips.)


plot_board=function(){
  x=rep(1:7,each=6)
  y=rep(1:6,times=7)
  plot(x,y,type="n",xlim=c(0,8),ylim=c(7,0))
  x0=c(rep(0.5,5),seq(1.5,6.5,1))
  y0=c(seq(1.5,5.5,1),rep(0.5,6))
  x1=c(rep(7.5,5),seq(1.5,6.5,1))
  y1=c(seq(1.5,5.5,1),rep(6.5,6))
  segments(x0=x0,y0=y0,x1=x1,y1=y1)
}

start.game=function(){
  plot_board()
  for (i in 1:(n_col*n_row)) {
  if (player == "X") { # human player
    repeat {
      index = identify(x, y, n=1, plot=FALSE)
      col = x[index]
      row = y[index]
      if (board[row, col] == "") {
        row=largest.empty.row(board,col)
        break
      } else {
        text(x=2, y=n_row+0.7, labels="Please click on empty square.")
      }
    }
  } else { # computer player
    repeat{
      index = sample(x=which(c(board) == ""), size=1)
      col = x[index]
      row = largest.empty.row(board,col)
      if (!is.null(row)) break
    }
  }
  board[row, col] = player
  text(x=col, y=row, labels=player)
  cat(sep="", "i=", i, ", player=", player, ", index=", index,
      ", row=", row, ", col=", col, ", board:", "\n")
  print(board)
  if (won(player,board,row,col,debug=TRUE)) {
    text(x=2, y=1/3, labels=paste(player, " won!"), col="red")
    board = matrix(data=rep("", times=n_col*n_row), nrow=n_row, ncol=n_col)
    
    break
  }
  player = ifelse(test=(player == "X"), yes="O", no="X")
  }
}

### initial setting
n_col=7
n_row=6
x = rep(1:n_col,times=n_row)
y = rep(1:n_row, each = n_col)
board = matrix(data=rep("", times=n_col*n_row), nrow=n_row, ncol=n_col)
player = "X"

start.game()


