
library(tidyverse)
library(Deriv)

#
x = seq(0.1, 10, by = 0.1)
px = 1
py = 1
I = 10
y = (I - x*px) / py
#

plot(x,y, type = 'l')
# $$U(x,y) = a \ln x + b \ln y$$

a = 0.3
b = 0.7

#
f = function(x,y,a,b) a * log(x) + b * log(y)
dxu = Deriv(f, "x") # MUx
dyu = Deriv(f, "y") # MUy
#

# MRS, MUx/MUy
#
- dxu(x = x, a = a) / dyu(y = y, b = b)
# MRS from derivative of cobb-douglas or MUx/MUy#
MRS = function(x,y,a,b) - (a/x) / (b/y)
MRS(x = x, y = y, a = a, b = b)
# (a/x) / (b/y)
# which is
# (a*y) / (c/y)

# the slope of the Budget Line is #
slope_budget = -py/px 
slope_budget

#
cbind(x,y, slope_budget, MRS(x = x, y = y, a = a, b = b))
#

library(microecon)
cobbs_douglas_utility(I = 10, a = a, b = b, px = 1, py = 1)
#
