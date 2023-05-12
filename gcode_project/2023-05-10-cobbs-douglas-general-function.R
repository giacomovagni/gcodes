
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
  x_max = f_max(I = I, p = px)
  # find maximum number of x I can get given their price
  # what if I spent all my money on y
  y_max = f_max(I = I, p = py)
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
  df = data.frame(x, y = f_cobbs_y(x, U = max_U, a = a, b = b), max_U) %>% mutate(Budget = x*px+y*py)
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
  plot(x,y, type = "l", xlim = c(0,x_max), ylim = c(0,y_max))
  abline(h=0,v=0, col = 'gray')
  lines(x, y_Umax, type = 'l', col = 'red')
  abline(h = df[w, ]$y, v = df[w, ]$x, col = 'blue')
  #
  
  #
  optimal_bundle = df[w, ]
  #
  
  #
  df = data.frame(x, y, y_Umax)
  #
  
  #
  return(list(df = df, x_max = x_max, y_max = y_max, slope = slope, x = x, y = y, y_Umax = y_Umax, U=U, optimal_bundle = optimal_bundle))
}

#
model1 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 1, py = 1)$df
model2 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 1, py = 2)$df
model3 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 2, py = 1)$df
#

#
models = bind_rows(model1, model2, model3, .id = 'model')
#

#
models %>% 
  ggplot(aes(x,y, colour = model)) + 
  geom_line() + 
  geom_line(aes(x,y_Umax, colour = model), linetype = 5) + 
  ylim(c(0, 120)) + 
  xlim(c(0, 120)) + 
  theme_minimal() + 
  geom_abline(slope = 1, linetype = 2, alpha = 0.4)
#

##########################################################################################
##########################################################################################

#
model4 = cobbs_douglas_utility(I = 400, a = 0.25, b = 0.75, px = 1, py = 4)$df
model5 = cobbs_douglas_utility(I = 400, a = 0.25, b = 0.75, px = 4, py = 1)$df
#

#
models = bind_rows(model1, model2, model3, model4, model5, .id = 'model')
#

#
models %>% 
  ggplot(aes(x,y, colour = model)) + 
  geom_line() + 
  geom_line(aes(x,y_Umax, colour = model), linetype = 5) + 
  ylim(c(0, 420)) + 
  xlim(c(0, 420)) + 
  theme_minimal() + 
  geom_abline(slope = 1, linetype = 2, alpha = 0.4)
#

##########################################################################################
##########################################################################################

