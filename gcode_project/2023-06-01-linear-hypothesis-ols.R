
#
set.seed(1)
#

#
class = rmultinom(n, c(1,1,1), c(0.2, 0.5, 0.3))
#
class = data.frame(t(class))
colnames(class) = c('high', 'mid', 'low')
rowSums(class)
table(class$high)
table(class$mid)
table(class$low)
#

#
n=100
U = rnorm(n, 0, 1)
I = U + rnorm(n, 0, 0.1)
K = U + rnorm(n, 0, 0.1)
y = I + K + class$low*-3 + rnorm(n)
#

df = data.frame(U, class, I, K, y)
#

lm(y ~ U + mid + low, data = df)

#
m0 = lm(y ~ 1, data = df)
m1 = lm(y ~ I + K + mid + low, data = df)
m2 <- lm(y ~ I + K + mid + low, data = df, offset = 0 + I*0 + K*0 + mid*0 + low*0)
m3 <- lm(y ~ I + K + mid + low, data = df, offset = 0 + I*0 + K*0 + mid*0 + low*-3)
#
summary(m1) # different than 0
summary(m2)
summary(m3)
#

#
library("car")
m1

#
anova(m0, m1)
linearHypothesis(m1, c("(Intercept) = 0", "I = 0", "K = 0", "mid = 0", "low = 0"))
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,0,0,0,0))
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,0,0,0,-3))
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,1,1,0,-3))
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,2,1,0,-3))
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,2,2,0,-3))
#
#
linearHypothesis(m1, "(Intercept) = 0")
#
linearHypothesis(m1, c("(Intercept)", "I", "K", "mid", "low"), c(0,0,0,0,0))
linearHypothesis(m1, c("(Intercept) = 0", "I = K", "mid = low"))
linearHypothesis(m1, c("I = K"))
linearHypothesis(m1, c("(Intercept) = 0", "I - K = 0", "mid + low = 0"))
#

########################################################################
########################################################################

#
x = fam %>% select(idno, hldid, sex, university, survey, childcare)
#

setDF(x)

library(fastDummies)
?dummy_cols
x

x$survey = as.factor(x$survey) 
x = fastDummies::dummy_cols(x, select_columns = c("survey", "sex"))
x
#

#
m1 = lm(childcare ~ (survey_1974 + survey_1983 + survey_2000 + survey_2015)*university*sex, data=x)
summary(m1)
#

#
coefs <- names(coef(m1))
## test against the null model (i.e., only the intercept is not set to 0)
linearHypothesis(m1, coefs[-1])
#
linearHypothesis(m1, coefs[grep(":", coefs)], verbose=TRUE)
#

#
m1 = lm(childcare ~ (survey_1974 + survey_1983 + survey_2000 + survey_2015)*sex, data=x)
summary(m1)
linearHypothesis(m1, coefs[-1])

#
linearHypothesis(m1, c("survey_1974:sexmale = 0", "survey_1983:sexmale = 0", "survey_2000:sexmale = 0", "survey_2015:sexmale = 0"))
linearHypothesis(m1, c("survey_1974:sexmale = 0", "survey_1983:sexmale = 0", "survey_2000:sexmale = 0", "survey_2015:sexmale = -20"))
linearHypothesis(m1, c("survey_1974:sexmale = 0", "survey_1983:sexmale = -30", "survey_2000:sexmale = -20", "survey_2015:sexmale = -20"))

# gap is constant # that is cool #
linearHypothesis(m1, c("survey_1974:sexmale = 0", "survey_1983:sexmale = -20", "survey_2000:sexmale = -20", "survey_2015:sexmale = -20"))
linearHypothesis(m1, c("survey_1974:sexmale = -20", "survey_1983:sexmale = -20", "survey_2000:sexmale = -20", "survey_2015:sexmale = -20"))
#

#
coefs <- names(coef(m1))
## test against the null model (i.e., only the intercept is not set to 0)
linearHypothesis(m1, coefs[-1])
#
linearHypothesis(m1, coefs[grep(":", coefs)], verbose=TRUE)
#

#
#