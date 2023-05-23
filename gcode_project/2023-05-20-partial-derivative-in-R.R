
############################
# partial derivatives in R #
############################

# Partial Derivative of Cobb-Douglas Utility Function
# monotonic transformation does not affect the Marginal Rate of Sub

# form 1
a*log(x) + b * log(y)
# form 2
(x^a) * (y^b)
#

# Hal Varian p. 72 Intermediate Micro, 8th edition #

############################
############################

library(Deriv)
library(microecon)
library(tidyverse)
library(ggthemes)

############################
############################

#
cb = cobbs_douglas_utility(I = 10, a = 0.5, b = 0.5, px = 1, py = 1)
cb

############################
############################

#
f_cobbs_ln <- function(a, x, b, y) a*log(x) + b * log(y)
#
# partial derivative form 1 #
f_ <- Deriv(f_cobbs_ln, "x")
f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
# compare
microecon::f_derive(x = 1:10, f = f_cobbs_ln, a = 0.5, y = 1, b = 0.5, h = 0.001)
#

#
dx = f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
dy = f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
#

# MRS
- dx/dy

############################
############################

# partial derivative form 2 #
f_ <- Deriv(f_cobbs, "x")
f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
# compare
microecon::f_derive(x = 1:10, f = f_cobbs, a = 0.5, y = 1, b = 0.5, h = 0.001)
#

#
dx = f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
dy = f_(x = 1:10, a = 0.5, b = 0.5, y = 1)
#

# MRS
- dx/dy

############################
############################
