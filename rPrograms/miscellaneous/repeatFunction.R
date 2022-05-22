rep(3:-2, times = 2)
rep(3:6, each = 2)
rep("wee", times = 4)
rep(1:5, 1:5)
#The above is the same as...
rep(1:5, seq(from = 2, to = 6, by = 1))
x <- matrix(nrow = 2, ncol = 2, data = 1:4, byrow = T)
x
rep(x, 2)
