
#
xtable_md = function(table){
  pr = print.xtable(xtable(table),comment =F)
  pr = gsub(pattern = "table", replacement = "aligned", x = pr)
  pr = gsub(pattern = "tabular", replacement = "array", x = pr)
  pr = gsub(pattern = "\\\\centering", replacement = "", x = pr)
  cat("$$",sub("\n", "", pr),"$$")
}
#
library(oaxaca)
library(oaxacad)
library(tidyverse)
library("ggrepel")
library("stringr")
#
# oaxaca_data()

#
swiss$catholic_city = ifelse(swiss$Catholic > 80, 1, 0)
swiss$name = rownames(swiss)#
#

library(xtable)
#
m0 = lm(Fertility ~ Education, data = swiss)
xt1 = cbind(expand_grid(Education = c(0,10,20,50)), predicted = predict(m0, newdata = expand_grid(Education =  c(0,10,20,50))))
#
xtable_md(xt1)
lag(xt1$predicted) - xt1$predicted#
#

#
m1 = lm(Fertility ~ Education, data = swiss, subset = catholic_city == 0 )
m2 = lm(Fertility ~ Education, data = swiss, subset = catholic_city == 1 )
#

#

#
swiss %>%
  ggplot(aes(Education, Fertility, colour = as.factor(catholic_city), label =  name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F, fullrange = T) +
  geom_text_repel() +
  scale_color_manual("Catholic City", values = c('red3', 'blue4')) +
  theme_minimal(base_size = 16) + ggtitle("Education and Fertility in French-speaking provinces of Switzerland in 1888")
#

#
swiss$Agriculture_rev = rev(swiss$Agriculture)
out_X = swiss %>% group_by(catholic_city) %>% summarise(mean(Education), mean(Agriculture), mean(Fertility))
ox = oaxaca(Fertility ~ Education | catholic_city, data=swiss) #Agriculture_rev
#
ox
#
Decomp(ox)
Decomp_simple(ox)
Decomp_Regression(ox)

(74.078-81.229) -0.103*(13.226-6.625) + 6.625*(-0.601) -3.968
-0.103*(13.226-6.625)
-0.704*(6.625-13.226)

(81.229-74.078) -0.704*(6.625-13.226) + 13.226*(-0.103- -0.704) -3.968
