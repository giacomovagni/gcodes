
#
source('~/Dropbox/academia/projects-in-progress/Trends in Child Care/rscripts-analyses/oaxaca-mediation-supplmentatry/oaxaca-functions-2022-03.R')
source('~/Documents/GitHub/gcodes/gcode_project/2023-06-01-OLS-function.R')
#

#
df1 = f(n = c(150, 150, 150), intercept = c(100, 120, 125), beta = c(20, 7, 7), sds = c(20, 20, 20))
df2 = f(n = c(150, 150, 150), intercept = c(100, 100, 100), beta = c(20, 7, 7), sds = c(20, 20, 20))
#

#
out1 = f_graph(df1)
out1$fig
out1$diff_mult
out1$models_comparison
out1$tidy
out1$tidym1
#
out2 = f_graph(df2)
out2$fig
out2$diff_mult
out2$tidy
#

#
out1$diff_mult
out2$diff_mult
#

df1$y2 = round(df1$y)
df2$y2 = round(df2$y)
#

#
library(vcd)
#
tab = out1$diff_mult %>% select(F = estimate2, M = estimate1) %>% round() %>% t()
tab2 = out2$diff_mult %>% select(F = estimate2, M = estimate1) %>% round() %>% t()
tab = as.matrix(tab)
tab2 = as.matrix(tab2)
tab = as.table(tab)
tab2 = as.table(tab2)
#
tab
lodds(tab, ref = 1, log = F)
lodds(tab2, ref = 1, log = F)
#
x_lor = loddsratio(tab, ref = 1)
coef(x_lor)
x_lor
x_lor = lodds(tab, ref = 1, log = F)
loddsratio(tab, ref = 1, log = F)
coef(x_lor)
confint(x_lor)
#

#
x_lor <- loddsratio(x)
coef(x_lor)
x_lor
confint(x_lor)
summary(x_lor)
#
g1 = glm(y2 ~ d*period, data=df1)
g2 = glm(y2 ~ d*period, data=df2)
#

g1$coefficients

ggpredict(g1, terms = c("period", "d")) %>% data.frame() 

#
ggpredict(g1, terms = c("period","d")) %>% data.frame()  %>% 
  ggplot(aes(x, predicted, colour = group)) +
  geom_line() +
  # geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = .1) + 
  #facet_wrap(~model) + 
  scale_x_continuous(breaks = c(0, 1,2), labels = c(1950, 1975, 2020)) + 
  theme_minimal(base_size = 16)
#

