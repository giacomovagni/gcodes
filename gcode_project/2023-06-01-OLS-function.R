
library(tidyverse)
library(ggplot2)
#

#
f = function(n = c(300, 500, 500), intercept = c(100, 120, 300), beta = c(20, 5, 2), sds = c(10, 2, 2)){
  d1 = rbinom(n[1], 1, plogis(rnorm(n[1])))
  d2 = rbinom(n[2], 1, plogis(rnorm(n[1])))
  d3 = rbinom(n[3], 1, plogis(rnorm(n[1])))
  y1 = intercept[1] + d1*beta[1] + rnorm(n[1], sd = sds[1])
  y2 = intercept[2] + d2*beta[2] + rnorm(n[2], sd = sds[2])
  y3 = intercept[3] + d3*beta[3] + rnorm(n[3], sd = sds[3])
  
  a = data.frame(id = 1:n[1], d = d1, period = 0, y = y1)
  b = data.frame(id = 1:n[2], d = d2, period = 1, y = y2)
  c = data.frame(id = 1:n[3], d = d3, period = 2, y = y3)
  df = bind_rows(a,b,c)
  return(df)
}
#

df = f(intercept = c(100, 120, 140), beta = c(20, 15, 5), sds = c(10, 10, 10))
df

#
f_graph = function(df){
  
  #
  #pp_spread = df %>% 
  #  group_by(period, d) %>% 
  #  dplyr::summarise(y = mean(y)) %>% 
  #  spread(d, y) %>% 
  #  dplyr::mutate(diff = `1`-`0`)
  #
  
  t0 = tidy(t.test(y~d, data = df, subset = period == 0))
  t1 = tidy(t.test(y~d, data = df, subset = period == 1))
  t2 = tidy(t.test(y~d, data = df, subset = period == 2))
  #
  
  t_test = bind_rows(t0, t1, t2) %>% select(1:5) %>% mutate(periods = c(0,1,2))
  t_test = select(t_test, periods, everything())
  #
  
  #
  m0 = lm(y ~ as.factor(period), df)
  m1 = lm(y ~ as.factor(period) + d, df)
  m2 = lm(y ~ as.factor(period) * d, df)
  #
  
  anova_out = anova(m0, m1, m2)
  #

  #
  mydf_m1 <- ggpredict(m1, terms = c("period", 'd'))
  mydf_m2 <- ggpredict(m2, terms = c("period", 'd'))
  mydf1 = data.frame(mydf_m1)
  mydf2 = data.frame(mydf_m2)
  
  #
  mydf1$model = 'Additive'
  mydf2$model = 'Multiplicative'
  
  #
  bd = bind_rows(mydf1, mydf2)
  #
  
  #
  fig = ggplot(mydf2, aes(x, predicted, colour = group)) +
    geom_line() +
    # geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = .1) + 
    #facet_wrap(~model) + 
    scale_x_continuous(breaks = c(0, 1,2), labels = c(1950, 1975, 2020)) + 
    theme_minimal(base_size = 16)
  #
  
  #
  return(list(list(m0, m1, m2), models_comparison = anova_out, diff_mult = t_test, bd = bd, fig = fig, tidym1 = tidy(m1), tidy = tidy(m2)))
}

#
f_graph2 = function(df){
  
  #
  m_lm = lm(y ~ d*period, df)
  #
  mydf_m <- ggpredict(m_lm, terms = c("period", 'd'))
  mydf_m = data.frame(mydf_m)
  mydf_m$model = 'Multiplicative'
  
  #
  bd = mydf_m
  #
  
  #
  ggplot(bd, aes(x, predicted, colour = group)) +
    geom_line() +
    # geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = .1) + 
    facet_grid(~model) + 
    scale_x_continuous(breaks = c(0, 1,2), labels = c(1950, 1975, 2020)) + 
    theme_minimal()
  #
}

f_graph(df = df)
#
