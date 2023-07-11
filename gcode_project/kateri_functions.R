
#
int_coef_normalisation = function(x) sqrt((1+1/((2*x)^2))) - (1/(2*x))
#

#
tt = as.table(rbind(c(100, 20, 30),
                    c(10, 100, 40)))
#
I=ncol(tt)
J=ncol(tt)
#
iac(tt, normalize = F, weighting = "none")
# sqrt( (3.912^2) / ((I*J)^2) )
model_gnm = gnm(Freq ~ Var1*Var2, data=tt, family = poisson)
#
coef_gnm = coef(model_gnm)
coef_gnm_OR = coef_gnm[grepl(":", names(coef_gnm))]
sum_coef_gnm_OR = sum(coef_gnm_OR^2)
#
intr_coeff = sqrt( (sum_coef_gnm_OR / (I*J) ) )
intr_coeff
sqrt( (1+1 / (2*intr_coeff)^2 ) ) - (1/2*intr_coeff)
#

#
nroot = function(root = 2, number = 2) number^(1/root) 
#

# geometric standard deviation #
SOR = exp(sqrt( sum(or_local$LOR)^2 / (I*J)^2 ))
SOR

####################################################################################################
####################################################################################################

#
tab3_df = structure(list(uni = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 
                                 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
                                 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
                                 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
                                 2L), levels = c("YES", "NO"), class = "factor"), clusters = structure(c(1L, 
                                                                                                         1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 2L, 
                                                                                                         2L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 2L, 2L, 3L, 
                                                                                                         3L, 4L, 4L, 5L, 5L, 6L, 6L, 7L, 7L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 
                                                                                                         4L, 5L, 5L, 6L, 6L, 7L, 7L), levels = c("PaidL", "Paid", "PShort", 
                                                                                                                                                 "DomShort", "Free", "Dom", "NonStPaid"), class = "factor"), country = structure(c(1L, 
                                                                                                                                                                                                                                   1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 
                                                                                                                                                                                                                                   2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                                                                                                                                                                                                                                   3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                                                                                                                                                                                                                   4L, 4L, 4L, 4L, 4L, 4L, 4L), levels = c("USA", "UK", "Sweden", 
                                                                                                                                                                                                                                                                           "Germany"), class = "factor"), Freq = c(86, 84, 87, 89, 42, 50, 
                                                                                                                                                                                                                                                                                                                   70, 104, 35, 76, 53, 101, 8, 29, 37, 46, 52, 90, 22, 74, 65, 
                                                                                                                                                                                                                                                                                                                   159, 26, 70, 74, 171, 7, 21, 68, 149, 61, 115, 25, 52, 50, 128, 
                                                                                                                                                                                                                                                                                                                   20, 63, 34, 98, 10, 41, 19, 69, 16, 82, 32, 178, 25, 152, 10, 
                                                                                                                                                                                                                                                                                                                   77, 25, 206, 3, 20)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                             -56L))
#

## mu is education = row #
mu=rescale_3way(score = c(1:2), dtable = tab3_df, X = tab3_df$uni, rows = 0, iflag = T)
nu=rescale_3way(score = c(1:7), dtable = tab3_df, X = tab3_df$clusters, rows = 1, iflag = T)

####################################################################################################
####################################################################################################

# this is wrong ufnortumnaltey #
cn = rescale(score = c(1:4), dtable = tab3_df, X = tab3_df$country, rows = 1, iflag = T)
#

#
sum(nu$score)
sum(nu$score^2)
#

#
mu = mu$score[match(tab3_df$uni, mu$X)] #U
nu = nu$score[match(tab3_df$clusters, nu$X)] #V
cn = cn$score[match(tab3_df$country, cn$X)] #V
#


#
rescale_3way <- function(score, dtable, X, iflag, rows) {
  if (rows==1) w <- with(dtable, tapply(Freq,X,sum)/sum(Freq)) 
  else w <- with(dtable, tapply(Freq,X,sum)/sum(Freq))
  if (iflag==0) w <- w/w
  # sum of score squared - minus the sum of scores
  # score = mu0
  alpha <- sqrt( (sum(w*score^2)-(sum(w*score))^2/sum(w))^(-1))
  beta <- -alpha*sum(w*score)/sum(w); score <- alpha*score+beta ; 
  return(list(X =names(table(X)), score=score, weight=w, alpha = alpha, beta = beta)) 
}

#
rescale <- function(score, I, J, dtable, iflag, rows) {
  if (rows==1) w <- with (dtable, tapply(freq,X,sum)/sum(freq)) 
  else w <- with (dtable, tapply(freq,Y,sum)/sum(freq))
  if (iflag==0) w <- w/w
  # sum of score squared - minus the sum of scores
  # score = mu0
  alpha <- sqrt( (sum(w*score^2)-(sum(w*score))^2/sum(w))^(-1))
  beta <- -alpha*sum(w*score)/sum(w); 
  # score here #
  score <- alpha*score+beta ; 
  
  if (rows==1) score_n = rep(score, J)
  else score_n = rep(score, I)
  #
  return(list(score=score, score_n = score_n, weight=w, alpha = alpha, beta = beta)) 
}
#

rescale(score = c(1:2), I = 2, J = 7, dtable = data.frame(tab_lay2wayUni), iflag = 1, rows = 1)
#

#
fit.R <- function(freq,NI,NJ,iflag){
  # fits the R-model to a NIxNJ table
  # calls function rescale() for the scores
  # iflag: flag for selecting marginal(=1) or uniform(=0) weights
  # for the scores’ constraints
  X <- gl(NI,NJ,length=NI*NJ);
  Y <- gl(NJ,1,length=NI*NJ);
  dtable <- data.frame(freq,X,Y);
  nu0 <- c(1:NJ);
  nu <- rep(rescale(nu0, dtable, iflag, 0)$score, NI);
  R.model <- glm(freq ~ X+Y+X:nu,poisson);
  s1 <- NI+NJ;
  s2 <- 2*NI+NJ-2;
  #
  mu0 <- c(coef(R.model)[s1:s2],0);
  #
  mu <- rescale(mu0, dtable, iflag, 1)$score;
  phi <- rescale(mu0, dtable, iflag, 1)$alpha^(-1)
  #
  
  G2 <- R.model$deviance;
  df <- R.model$df.residual;
  p.value <- 1-pchisq(G2,df);
  fit.freq <- R.model$fitted.values;
  ovw <- summary(R.model); 
  
  l = list(model=R.model,overview=ovw,G2=G2,df=df,p.value=p.value, fit.freq=fit.freq,phi=phi,mu_parametric=mu,nu=nu[1:NJ]) 
  return(l)
}


#
fit.C <- function(freq,NI,NJ,iflag){
  # fits the R-model to a NIxNJ table
  # calls function rescale() for the scores
  # iflag: flag for selecting marginal(=1) or uniform(=0) weights
  # for the scores’ constraints
  X <- gl(NI,NJ,length=NI*NJ);
  Y <- gl(NJ,1,length=NI*NJ);
  dtable <- data.frame(freq,X,Y);
  mu0 <- c(1:NI);
  mu <- rep(rescale(mu0, dtable, iflag, 1)$score, NJ);
  C.model <- glm(freq ~ X+Y+Y:mu,poisson);
  s1 <- NI+NJ;
  s2 <- 2*NI+NJ-2;
  #
  nu0 <- c(coef(R.model)[s1:s2],0);
  #
  nu <- rescale(nu0, dtable, iflag, 1)$score;
  phi <- rescale(nu0, dtable, iflag, 1)$alpha^(-1)
  #
  
  G2 <- R.model$deviance;
  df <- R.model$df.residual;
  p.value <- 1-pchisq(G2,df);
  fit.freq <- R.model$fitted.values;
  ovw <- summary(R.model); 
  
  l = list(model=R.model,overview=ovw,G2=G2,df=df,p.value=p.value, fit.freq=fit.freq,phi=phi,nu_parametric=nu,mu=mu[1:NI]) 
  return(l)
}



#
rescale <- function(score, dtable, iflag, rows) {
  if (rows==1) w <- with (dtable, tapply(freq,X,sum)/sum(freq)) 
  else w <- with (dtable, tapply(freq,Y,sum)/sum(freq))
  if (iflag==0) w <- w/w
  # sum of score squared - minus the sum of scores
  # score = mu0
  alpha <- sqrt( (sum(w*score^2)-(sum(w*score))^2/sum(w))^(-1));
  beta <- -alpha*sum(w*score)/sum(w); score <- alpha*score+beta ; 
  return(list(score=score, weight=w, alpha = alpha, beta = beta)) 
}

# mu0
# 1,2,3
#
sum(rescale(score = c(1,2,3),dtable = dtable, iflag = 1, rows = 1)$score)
sum(rescale(score = c(1,2,3),dtable = dtable, iflag = 1, rows = 1)$score^2)


fit.U(freq = dtable$freq, NI = 3, NJ = 3, iflag = 0)
fit.R(freq = dtable$freq, NI = 3, NJ = 3, iflag = 1)
#

#
fit.U <- function(freq,NI,NJ,iflag) {
  # fits the U-model to a NIxNJ table
  # calls function rescale() for the scores
  # iflag: flag for selecting marginal(=1) or uniform(=0) weights
  # for the scores’ constraints
  X <- gl(NI,NJ,length=NI*NJ);
  Y <- gl(NJ,1,length=NI*NJ);
  dtable <- data.frame(freq,X,Y);
  mu0 <- rep(1:NI);
  nu0 <- rep(1:NJ);
  mu <- rep(rescale(mu0, dtable, iflag, 1)$score,each=NJ);
  nu <- rep(rescale(nu0, dtable, iflag, 0)$score, NI);
  U.model <- glm(freq ~ X+Y+mu:nu,poisson)
  G2 <- U.model$deviance
  df <- U.model$df.residual
  p.value <- 1-pchisq(G2,df)
  fit.freq <- U.model$fitted.values
  phi <- coef(U.model)[NI+NJ]
  ovw <- summary(U.model) 
  
  return(list(model=U.model,overview=ovw,G2=G2,df=df, p.value=p.value,fit.freq=fit.freq,phi=phi,mu=matrix(mu, nrow=NI,byrow=TRUE)[1:NI],nu=nu[1:NJ])) 
}
#

#
fit.R <- function(freq,NI,NJ,iflag){
  # fits the R-model to a NIxNJ table
  # calls function rescale() for the scores
  # iflag: flag for selecting marginal(=1) or uniform(=0) weights
  # for the scores’ constraints
  X <- gl(NI,NJ,length=NI*NJ);
  Y <- gl(NJ,1,length=NI*NJ);
  dtable <- data.frame(freq,X,Y);
  nu0 <- c(1:NJ);
  nu <- rep(rescale(nu0, dtable, iflag, 0)$score, NI);
  R.model <- glm(freq ~ X+Y+X:nu,poisson);
  s1 <- NI+NJ;
  s2 <- 2*NI+NJ-2;
  #
  mu0 <- c(coef(R.model)[s1:s2],0);
  #
  mu <- rescale(mu0, dtable, iflag, 1)$score;
  phi <- rescale(mu0, dtable, iflag, 1)$alpha^(-1)
  #
  
  G2 <- R.model$deviance;
  df <- R.model$df.residual;
  p.value <- 1-pchisq(G2,df);
  fit.freq <- R.model$fitted.values;
  ovw <- summary(R.model); 
  
  l = list(model=R.model,overview=ovw,G2=G2,df=df,p.value=p.value, fit.freq=fit.freq,phi=phi,mu_parametric=mu,nu=nu[1:NJ]) 
  return(l)
}

#
fit.RC = function(freq,NI,NJ,iflag) {
  # fits the RC-model to a NIxNJ table
  # needs the gnm package
  # calls function rescale() for the scores
  # iflag: flag for selecting marginal(=1) or uniform(=0) weights
  # for the scores’ constraints
  X <- gl(NI,NJ,length=NI*NJ);
  Y <- gl(NJ,1,length=NI*NJ);
  dtable <- data.frame(freq,X,Y);
  RC.model <- gnm(freq ~ X+Y+Mult(X, Y), family=poisson); 
  s1 <- NI+NJ;
  s2 <- 2*NI+NJ-1;
  s3 <- s2+1;
  s4 <- 2*(NI+NJ)-1;
  mu0 <- coef(RC.model)[s1:s2];
  nu0 <- coef(RC.model)[s3:s4];
  mu <- rescale(mu0, dtable, iflag, 1)$score;
  nu <- rescale(nu0, dtable, iflag, 0)$score;
  phi <- rescale(mu0, dtable, iflag, 1)$alpha*rescale(nu0, dtable, iflag, 0)$alpha^(-1) ; 
  G2 <- RC.model$deviance;
  df <- RC.model$df.residual;
  p.value <- 1-pchisq(G2,df);
  fit.freq <- predict(RC.model, type="response", se.fit=TRUE);
  ovw <- RC.model;
  
  l = list(model=RC.model,overview=ovw, G2=G2, df=df, p.value=p.value, fit.freq=fit.freq, phi=phi, mu=mu, nu=nu)
  
  return(l);
}

