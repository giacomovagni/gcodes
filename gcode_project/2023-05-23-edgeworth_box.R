
library(microecon)
library(tidyverse)
library(ggthemes)
library(Deriv)

I=100

#
k0 = 5
km = 15
#

#
m1 = lapply(k0:km, function(i) cobbs_douglas_utility(I = i, a = 0.5, b = 0.5, px = 1, py = 1, by = 0.1)$df )
m1_opt = lapply(k0:km, function(i) cobbs_douglas_utility(I = i, a = 0.5, b = 0.5, px = 1, py = 1, by = 0.1)$optimal_bundle )
#
m1U = bind_rows(m1, .id = "U")
#

#
m2 = lapply(k0:km, function(i) cobbs_douglas_utility(I = i, a = 0.5, b = 0.5, px = 1, py = 1, by = 0.1)$df )
m2_opt = lapply(k0:km, function(i) cobbs_douglas_utility(I = i, a = 0.5, b = 0.5, px = 1, py = 1, by = 0.1)$optimal_bundle )
m2U = bind_rows(m2, .id = "U")
#

#
plot(m1U$x, m1U$yU, type = 'n', ylim = c(0, 10), xlim = c(0, 10), axes = F, xlab = 'x', ylab = 'y')
axis(side = 1, at = 1:10)
axis(side = 2, at = 1:10)
#
for(i in (k0:km) - k0+1) lines(m1U$x[m1U$U == i], m1U$yU[m1U$U == i], col = 'blue4')
for(i in (k0:km) - k0+1) text( m1_opt[[i]]$x, y = m1_opt[[i]]$yU, col = 'blue4', labels = LETTERS[i])
#
#
par(new = TRUE)
plot(m2U$x, m2U$yU, type = 'n', ylim = c(10, 0), xlim = c(10, 0), axes = F, xlab = 'x', ylab = 'y')
axis(side = 3, at = 10:1)
axis(side = 4, at = 10:1)
#
for(i in (k0:km) - k0+1) lines(m2U$x[m2U$U == i], m2U$yU[m2U$U == i], col = 'coral')
for(i in (k0:km) - k0+1) text( m2_opt[[i]]$x, y = m2_opt[[i]]$yU, col = 'coral', labels = LETTERS[i])
#
#


#
m1U %>%
  ggplot() +
  geom_line(aes(x, yU, group = U)) +
  ylim(0, 15) +
  xlim(0, 15)
#
