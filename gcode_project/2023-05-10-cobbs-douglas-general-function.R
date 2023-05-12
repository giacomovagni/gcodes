
# Giacomo Vagni
# May 2023
# Essex

#
library(tidyverse)
library(Deriv)

############################################################
############################################################

#
f_cobbs = function(x=x,y=y,a=a,b=b) (x^a) * (y^b)
# cobbs-douglas solve for y
f_cobbs_y = function(x,U=25,a=a,b=b) (U / x^a)^(1/b)
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

# I = Budget

# general function to solve a Cobbs-Douglas Utility function #
cobbs_douglas_utility = function(I, a=0.5, b=0.5, px, py){
  
  # find maximum number of x I can get given their price
  # what if I spent all my money on x
  x_max = f_max(I = 100, p = px)
  # find maximum number of x I can get given their price
  # what if I spent all my money on y
  y_max = f_max(I = 100, p = py)
  #
  
  # slope of budget line is px/py
  slope = -(px/py)
  #
  
  x = 0:x_max
  # calculate the values for y that respect the budget #
  y = y_max + (slope * x)
  #
  
  #
  U = f_cobbs(x,y,a,b)
  max_U = max(U)
  #
  
  #
  y_Umax = f_cobbs_y(x = x, U = max_U, a = a, b = b)
  #
  
  #
  df = data.frame(x, y = f_cobbs_y(x, U = max_U, a = a, b = b)) %>% mutate(Budget = x*px+y*py)
  # we can see that we are over our budget
  head(df) #
  # we retrieve the row which respect our budget
  # because I am using rounded values, we are not exactly at 100, but very close
  w = which.min(df$Budget - I)
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
  
  #
  optimal_bundle = df[w, ]
  #
  
  return(list(x_max = x_max, y_max = y_max, slope = slope, x = x, y = y, y_Umax = y_Umax, U=U, optimal_bundle = optimal_bundle))
}

#
cobbs_douglas_utility(I = 1000, a = 0.4, b = 0.6, px = 4, py = 3.5)
#

##########################################################################################
##########################################################################################