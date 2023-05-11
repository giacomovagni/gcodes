
############################################################
############################################################

library(tidyverse)
library(Deriv)

# cobbs-douglas utility function
f_cobbs = function(x,y,a=0.5,b=0.5) (x^a) * (y^b)
# cobbs-douglas solve for y
f_cobbs_y = function(x,U=25,a=0.5,b=0.5) (U / x^a)^(1/b)
# maximum number of good x given a price
f_max = function(I,p) I/p
# budget function
f_budget = function(x,px,y,py) x*px + py*y
# budget solve for y
f_budget_y = function(x,px,py, I) (I - x*px)/py
#

# price for x
px = 3
# price for y
py = 2
#

# find maximum number of x I can get given their price
# what if I spent all my money on x
x_max = f_max(I = 100, p = px)
# find maximum number of x I can get given their price
# what if I spent all my money on y
y_max = f_max(I = 100, p = py)
#

# must sum to $100
f_budget(x = x_max, px = px, y = 0, py = py)
#

# slope of budget line is px/py
slope = -(px/py)
#

#
x = 0:x_max
# calculate the values for y that respect the budget #
y = y_max + (slope * x)
# this is our budget line, given the prices of x and y #
plot(x,y, type = "l", xlim = c(0,60), ylim = c(0,60))
abline(h=0,v=0, col = 'gray')
#

##########################################################################################
##########################################################################################

# let's compute all combinations of utility that respect the Budget line

# cobbs-douglas alpha and beta parameters (returns on investment)
a = 0.7
b = 0.3
#

#
Utility_distribution = f_cobbs(x = x, y = y, a = a, b = b)
# plot the Utility function
plot(density(Utility_distribution))
#

# let's find the maximum possible utility for all combinations of x and y, given our budget
max_U = max(Utility_distribution)
max_U
#

# Now let's retrieve the indifference curve with the maximum utility
# to do so, we predict the values for y, keeping max U constant
# solve for y, y = (U/x^a)^(1/b)

# note that you can get more precise values by setting x to seq(0,60, by = 0.01), etc #
# here I just the x from our budget line
y_Umax = f_cobbs_y(x = x, U = max_U, a = a, b = b)
#

# this is our indifference curve with maximum Utility
plot(x, y_Umax, type = 'l', ylim = c(0,100))
#

##########################################################################################
##########################################################################################

# Let's superimpose the indifference curve on the budget line #

# this is our budget line, given the prices of x and y #
plot(x,y, type = "l", xlim = c(0,40), ylim = c(0,60))
abline(h=0,v=0, col = 'gray')
lines(x, y_Umax, type = 'l', col = 'red')
#

##########################################################################################
##########################################################################################

# Now let's find the optimal point

##########################################################################################
##########################################################################################

# ------------------------------------------------------ #
# method 1. The combination that touches the budget line #
# ------------------------------------------------------ #

# we predict all the values for y (same as before) 
# and we calculate the budget given those values
# only 1 value for y will respect the budget (given the indifference curve of Maximum Utility)
max_U
#
df = data.frame(x, y = f_cobbs_y(x, U = max_U, a = a, b = b)) %>% mutate(Budget = x*px+y*py)
# we can see that we are over our budget
head(df) #
# we retrieve the row which respect our budget
# because I am using rounded values, we are not exactly at 100, but very close
w = which.min(df$Budget - 100)
df[w, ]
# the optimal combination maximising utility
# is 23 = x, and 15.5 = y
# which respect our budget of $100
#

# this is our budget line, given the prices of x and y #
plot(x,y, type = "l", xlim = c(0,40), ylim = c(0,60))
abline(h=0,v=0, col = 'gray')
lines(x, y_Umax, type = 'l', col = 'red')
abline(h = df[w, ]$y, v = df[w, ]$x, col = 'blue')
#

df[w, ]
#

##########################################################################################
##########################################################################################

# ------------------------------------------------------ #
# method 2. slope of budget line == MRS
# ------------------------------------------------------ #

# The optimal point can also be found by using the Marginal Rate of Substitution 
# which is the slope of and indifference curve. 
# Here we use the indifference curve with maximum utility
# we derive its slope at each point

# plot #
plot(x,y, type = 'l')
#

# find maximum number of x I can get given their price
# what if I spent all my money on x
x_max = f_max(I = 100, p = px)
# find maximum number of x I can get given their price
# what if I spent all my money on y
y_max = f_max(I = 100, p = py)
#

#
x = seq(0, x_max, by = 0.001)
# calculate the values for y that respect the budget #
y = y_max + (slope * x)


## plots ##
plot(x,y, type = "l", xlim = c(0,60), ylim = c(0,60))
abline(h=0,v=0, col = 'gray')
#

max_U = max(f_cobbs(x = x, y = y, a = a, b = b))
max_U

#
df2 = data.frame(x, y = f_cobbs_y(x, U = max_U, a = a, b = b)) %>% mutate(Budget = x*px+y*py)
df2$slope = slope
#

# indifference curve #
points(df2$x, df2$y, type = "l")
#

#
f_derive = function(h = 0.01, f = f_cobbs, x, y, a, b) (f(x = x+h, y = y, a = a, b = b) - f(x = x,y = y,a = a, b = b)) / h
MUxy = - f_derive(h = 0.01, x = x, y = y, a = a, b = b) / f_derive(h = 0.01, x = y, y = x, a = b, b = a)
#
df2$MRS = MUxy
#
w2 = which.min(abs(df2$slope - df2$MRS))
#

#
abline(h = df2[w2, ]$y, v = df2[w2, ]$x, col = 'red')
#

#
df[w, ]
df2[w2, ]
#