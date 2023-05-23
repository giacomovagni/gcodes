library(tidyverse)
library(ggthemes)
library(microecon)
library(scales)
library(DescTools)
#

#
n=1000
#

#
r = rlnorm(n, meanlog = 1.5, sdlog = 0.1)
Gini(r)
r = rlnorm(n, meanlog = 1.5, sdlog = 1)
Gini(r)
r = rlnorm(n, meanlog = 1.5, sdlog = 2)
Gini(r)
#

range = function(x) (x-min(x))/(max(x)-min(x)) 

#
micro_sim = function(n = 1000 , px = 1, py = 1, wealth = 10000, meanlog = 7, sdlog = 0.1){
  #
  # some other unobserved 
  x = rnorm(n, sd = 1)
  # I = wealth * plogis(q = x, location = location, scale = scale)
  # plot(r, I, main = paste("location", location, ", scale", scale), type = 'p')
  
  I = ( rlnorm(n, meanlog = meanlog, sdlog = sdlog) ) 
  #
  
  hist(I, breaks = 100)
  # I = r
  # rn = range(r)
  #I = wealth * rn
  #
  #hist(I)
  #summary(I)
  #Gini(I)
  #
  #
  #
  I = ifelse(I < 200, runif(n, 190, 210), I)
  #
  
  # rn_alpha = rn + plogis(x)
  
  #
  beta = round(rescale(I, to = c(0.4, 0.7)), 2)
  #hist(alpha)
  #plot(r,alpha)
  # cor(I,alpha)
  #
  
  #
  df = data.frame(id = 1:n, I, alpha = 1-beta, beta)
  df$px = px
  df$py = py
  #
  df = arrange(df, I)
  #
  
  #
  df$x = df$alpha * ( df$I / df$px) 
  df$y = df$beta * ( df$I / df$py) 
  #
  df$x = round(df$x)
  df$y = round(df$y)
  df$I = round(df$I)
  #
  df$social_class = ntile(df$I, n = 10)
  #
  df$budget_check = f_budget(x = df$x, px = df$px, y = df$y, py = df$py)
  #
  df$U = f_cobbs(x = df$x, y = df$y, a = df$alpha, b = df$beta)
  #
  return(df)
}

#
m1 = micro_sim(px = 1, py = 1, wealth = 10000, meanlog = 7, sdlog = 0.1)
Gini(m1$I, unbiased=FALSE)
summary(m1$I)
#
m2 = micro_sim(px = 1, py = 1, wealth = 10000, meanlog = 7, sdlog = 1)
summary(m2$I)
Gini(m2$I, unbiased=FALSE)
#

# price rise #
m3 = micro_sim(px = 3, py = 1, wealth = 10000, meanlog = 7, sdlog = 1)
Gini(m3$I, unbiased=FALSE)
#

#
m1 %>% group_by(social_class) %>% summarise(n(), median(I), mean(U), mean(alpha), mean(x))
m2 %>% group_by(social_class) %>% summarise(n(), median(I), mean(U), mean(alpha), mean(x))
m3 %>% group_by(social_class) %>% summarise(n(), median(I), mean(U), mean(alpha), mean(x))
#

1023/192
1459/193
#

#
#

#
k = 1
poorest = cobbs_douglas_utility(I = m1$I[k], a = m1$alpha[k], b = m1$beta[k], px = m1$px[k], py = m1$py[k])
poorest
k = 500
richest = cobbs_douglas_utility(I = m1$I[k], a = m1$alpha[k], b = m1$beta[k], px = m1$px[k], py = m1$py[k])
richest
#
cobbs_douglas_models(poorest, richest, ymax = 250, xmax = 250)
#
