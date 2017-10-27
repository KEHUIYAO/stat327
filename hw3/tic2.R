# 1. Draw tic-tac-toe board.
# 2. Add board and playing loop.

rm(list=ls()) # clear all defined objects

x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

plot(x, y, type="n", xlim=c(0, 4), ylim=c(4, 0))
segments(x0=c(0.5, 0.5, 1.5, 2.5), y0=c(1.5, 2.5, 0.5, 0.5),
         x1=c(3.5, 3.5, 1.5, 2.5), y1=c(1.5, 2.5, 3.5, 3.5))

board = matrix(data=rep("E", times=9), nrow=3, ncol=3)
player = "X"
for (i in 1:9) {
  cat(sep="", "i=", i, ", player=", player, "\n")
  # ... player takes a turn ...
  player = ifelse(test=(player == "X"), yes="O", no="X")
}
