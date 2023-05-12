
# a is the intercept
# r is the growth rate
f_growth = function(a, r, t) a * (1 + r)^t
f_growth_e = function(a, r, t) a * exp(t*r)
#

# Compound Interest, of 2% growth over 3 years, with a start of 1000
f_growth_e(a = 1000, r = 0.02, t = 3)
#
# yearly-growth
f_growth(a = 1000, r = 0.02, t = 3)
#

# 10 years #
time = seq(0,10,by=1)
#

# compute growth rate
growth_rate_10 = f_growth(a = 1000, r = 0.02, t = time)
growth_rate_10_e = f_growth_e(a = 1000, r = 0.02, t = time)
#
max(growth_rate_10)
max(growth_rate_10_e)
#
plot(time, growth_rate_10, type = 'l')
lines(time, growth_rate_10_e, type = 'l', col = 'red')
#

#
time = 0:10
#
f_growth(a = 1, r = 0.1, t = time)
(1.1^time)
#
f_growth(a = 1, r = 0.5, t = time)
(1.5^time)
#
f_growth(a = 1, r = 0.7, t = time)
(1.7^time)
#
f_growth(a = 1, r = 1, t = time)
(2^time)
#
f_growth(a = 1, r = 3, t = time)
(4^time)
#

#
time = 0:40
# decay #
# - 10% decay
f_growth(a = 40, r = -0.1, t = time)
# when does 40 runs out?
# at what time does 40 reach 1?
# solve for time, 
40*(0.9)^time >= 1
(0.9)^time >= 1/40
log(x = 1/40, 0.9) # 35
time >= log(x = 1/40, 0.9)
#

#
plot(time, f_growth(a = 100, r = -.2, t = time))
plot(time, log(f_growth(a = 100, r = -.2, t = time)))
#

# when log < 0, we are under 1 #
cbind(time, f_growth(a = 100, r = -.2, t = time), log(f_growth(a = 100, r = -.2, t = time)))
#

#
t=seq(0,10,by=1)
#

#
y = t^2
y
# t^2 = y
# t = sqrt(y)
#

#
# exp(t) = y
# t = log(y)
#

# logarithm and exponential #
x1 = 1:10
x2 = 10:1
a = 0.3
b = 0.7
#
y = (x1^a) * (x2^b)
y_log = a*log(x1) + b*log(x2)
exp(y_log)
#

