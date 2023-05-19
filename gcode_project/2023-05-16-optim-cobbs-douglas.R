
library(tidyverse)
library(ggthemes)
library(microecon)
#


# m = px * x + py + y
# y = (m - (px * x))/py
#
# objective function
# par = x
#
f = function(par = 1, m = 100, a = 0.5, b = 0.5, px = 1, py = 1){
  y = (m - (px * par))/py
  U = (par^a) * (y^b)
  return(U)
}
#

# x = 1, y = 99
# U is 39 #
f(par = 1, m = 100, px = 1, py = 1, a = 0.2, b = 0.8)
# optimal with x = 20 #
f(par = 20, m = 100, px = 1, py = 1, a = 0.2, b = 0.8)
#

# library(microecon)
cobbs_douglas_utility(I = 100, a = 0.2, b = 0.8, px = 1, py = 1)
#

# we are searching for the x value that maximize utility
# y is calculated automatically given the budget constraints we have
# y = (m - (px * x))/py

# perfect #
optim(par = c(1), # starting value #
      fn = f,
      a = 0.2,
      b = 0.8,
      lower = 1, # lower bound for search
      upper = 100, # upper bound for search #
      method = "L-BFGS-B",
      control = list(fnscale = -1)) # maximize instead of minimize
#

# optim$par
# tell us that 20 is the optimal value for x

#
out = optim(par = c(1), # starting value #
      fn = f,
      a = 0.2,
      b = 0.8,
      lower = 1, # lower bound for search
      upper = 100, # upper bound for search #
      method = "L-BFGS-B",
      control = list(fnscale = -1)) # maximize instead of minimize
#

# we can put it back into our function and retrieve the ideal value #
f(par = out$par, m = 100, px = 1, py = 1, a = 0.2, b = 0.8)
#
