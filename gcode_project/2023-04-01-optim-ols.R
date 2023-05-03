
# simple OLS optimisation using R function optim

#
betas = c(2, 3, 1, 2)

n=1000
X = matrix(rnorm(n), ncol = 4)
y = X%*%betas

#
X_obs = X[,1:2]
summary(lm(y ~ X_obs))
#

# objective functions
f_min_SS = function(y, X_obs, par){
  SSQ = sum( (y - (X_obs %*% par))^2)
  return(SSQ)
}

# sum of squared errors #
f_min_SS(y, X_obs, par = c(2,2))
f_min_SS(y, X_obs, par = c(2,3))
#

#
optim(par = c(1,1), y = y, X_obs = X_obs, fn = f_min_SS, lower = c(-5, -5), upper = c(5,5), method = "L-BFGS-B") # 
#

