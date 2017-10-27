# 1. Draw tic-tac-toe board.

rm(list=ls()) # clear all defined objects

x = rep(1:3, each = 3)
y = rep(1:3, times = 3)

plot(x, y, type="n", xlim=c(0, 4), ylim=c(4, 0))
segments(x0=c(0.5, 0.5, 1.5, 2.5), y0=c(1.5, 2.5, 0.5, 0.5),
         x1=c(3.5, 3.5, 1.5, 2.5), y1=c(1.5, 2.5, 3.5, 3.5))
