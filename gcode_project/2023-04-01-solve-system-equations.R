
# solve system of linear equations

# install.packages("matlib")
library(matlib)
#

# eq 1
# 2x1 + x2 = 11

# eq 2
# x1 + 2x2 = 10
#

# matrix form
# [2 1][x1] = [11]
# [1 2][x2] = [10]

# 
# x1[2,1] + x2[1,2] = [11,10]
#

#
#
m1 = matrix(c(2, 1, 1, 2), byrow= T, nrow = 2)
x = c(4,3)
#

# predict y #
m1 %*% x
#
y = m1 %*% x
# inverse solve for x #
inv(m1) %*% y
# same with solve
solve(m1, y)
#

# show equation
showEqn(m1)
# plot equaiton 
plotEqn(m1,y)
# matlib solve #
Solve(m1,y)
#
