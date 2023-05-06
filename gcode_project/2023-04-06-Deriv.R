
library(Deriv)

#
f = function(x,y) x^0.3 + y^0.7
f = function(x,y) x^2 + y
#

#
h = 0.01
x = 1:10
f_deriv = function(h, x, x2, f) ( f(x+h,x2) - f(x,x2)) / h
f_deriv(h = 0.001, x = x, x2 = 1, f = f)
Deriv(f, "x")
2 * x
#
plot(x, f(x, y = 0))
#