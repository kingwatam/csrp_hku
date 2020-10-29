rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

# install.packages("https://github.com/kingwatam/bivpois/raw/master/bivpois_0.50-3.1.tar.gz", repo=NULL, type="source", dependencies = TRUE)

library(bivpois)
library(boot)
# library(dplyr)
library(magrittr) # pipes
library(parallel) # detectCores()
library(mcglm)

setwd(sprintf("~%s/multivariate", setpath))
source('lm.bp2.R')
source('BPO.reg.R')

df <- read.csv("archive/toydataset.csv")
dfcount <- read.csv("archive/toydataset_count.csv")

# cor(df[complete.cases(df), 2:6], method = 'pearson')

# GLM ----
# GLM for bootstrap
poisreg <- function(dat, indices) {
  dat <- dat[indices,]
  fitboot <-  glm(y1 ~ x1 + x2 + x3, data = dat, family = poisson(link = 'log'))
  return(coef(fitboot)) 
} 

glmy1 <- glm(y1 ~ x1 + x2 + x3, data = df, family = poisson)
# glmy2 <- glm(y2 ~ x1 + x2 + x3, data = df, family = poisson)
summary(glmy1)

glmy1_boot <- boot(data=df[complete.cases(df),], statistic = poisreg, parallel = 'snow', ncpus = parallel::detectCores(), R = 1000)

boot.ci(boot.out = glmy1_boot, type = c('basic', 'perc'), index = 1)

# p-value methods ----
# basic method of caculating p-values
get_basic_pval <- function(boot_object){
  for (i in (1:length(boot_object$t0))){
    beta <- 2*boot_object$t0[i]-boot_object$t[,i]
    p1 <- sum(beta >= 0)/length(beta) # proportion of x greather than 0
    p2 <- sum(beta <= 0)/length(beta) # proportion of x less than 0
    print(2 * min(p1, p2))
  }
}

# percentile method of caculating p-values
get_perc_pval <- function(boot_object){
  for (i in (1:length(boot_object$t0))){
    p1 <- sum(boot_object$t[,i] >= 0)/length(boot_object$t[,i]) # proportion of x greather than 0
    p2 <- sum(boot_object$t[,i] <= 0)/length(boot_object$t[,i]) # proportion of x less than 0
    print(2 * min(p1, p2))
  }
}

# # another method to p-values, but not sure which method of CI this corresponds to, ref: https://stats.stackexchange.com/a/83038
# get_pval <- function(boot_object)
# for (i in (1:length(boot_object$t0))){
#   print(
#     mean(abs(boot_object$t[,i] - mean(boot_object$t[,i]) )> abs(boot_object$t0[i]))
#     )
# }

get_perc_pval(glmy1_boot)

# bivariate poisson ----
# bivariate poisson for bootstrap
bipoisreg <- function(dat, indices) {
  dat <- dat[indices,]
  invisible(
    capture.output(
      fitboot <- bivpois::lm.bp(y1 ~  x1 + x2 + x3 , y2 ~ x1 + x2 + x3
                                , l3 = ~ x1 + x2 + x3
                                , zeroL3 = FALSE, data = dat, verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
    )
  )
  return(coef(fitboot)) 
} 

# compute time
system.time(
  bpy1y2_boot <-
    boot(data=df[complete.cases(df),], statistic = bipoisreg, parallel = 'snow', ncpus = parallel::detectCores(), R = 10) # takes about 0.5 min depending on your CPU
)

# bootstrapped standard errors
apply(bpy1y2_boot$t, MARGIN =  2, FUN = sd)

# rates with exposure (population) as weights produce identical results as counts with an offset
summary( glm(female.num ~ x1 + x2 + x3 + offset(log(female.st.pop/100000)), data = dfcount, family = poisson) )
summary( glm(female.num ~ x1 + x2 + x3, data = dfcount, family = poisson, offset = log(female.st.pop/100000)) )
summary( glm(female.rate ~ x1 + x2 + x3, data = dfcount, family = poisson(link = 'log'), weights = (female.st.pop/100000)) )
summary( glm(female.rate ~ x1 + x2 + x3, data = dfcount, family = poisson) )
summary( glm(male.num ~ x1 + x2 + x3, data = dfcount, family = poisson) )

invisible(capture.output(
  biv <- bivpois::lm.bp(y1 ~  x1 + x2 + x3 , y2 ~ x1 + x2 + x3
                        , l3 = ~ x1 + x2 + x3
                        , zeroL3 = FALSE, data = df[complete.cases(df),], verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))

summary( glm(y1 ~ x1 + x2 + x3, data = df, family = poisson) )


invisible(capture.output(
  biv1 <- bivpois::lm.bp(female.rate ~  1 , male.rate ~ 1
                      , l1l2 = ~ x1 + x2 + x3
                      , l3 = ~  x1 + x2 + x3
                      , zeroL3 = FALSE, data = dfcount[complete.cases(dfcount),], verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))
  
invisible(capture.output(
  biv2 <- lm.bp2(female.rate ~  x1 + x2 + x3 , male.rate ~ x1 + x2 + x3
               , l3 = ~  x1 + x2 + x3
               , zeroL3 = FALSE, data = dfcount[complete.cases(dfcount),]
               , weights1 = dfcount$female.st.pop[complete.cases(dfcount)]/100000
               , weights2 = dfcount$male.st.pop[complete.cases(dfcount)]/100000
               ,verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))

invisible(capture.output(
  biv3 <- lm.bp2(female.rate ~  x1 + x2 + x3 , male.rate ~ x1 + x2 + x3
                 , l3 = ~  x1 + x2 + x3
                 , zeroL3 = FALSE, data = dfcount[complete.cases(dfcount),]
                 , weights1 = dfcount$female.st.pop[complete.cases(dfcount)]/100000
                 , weights2 = dfcount$male.st.pop[complete.cases(dfcount)]/100000
                 , weights3 =  rowMeans(cbind(dfcount$female.st.pop[complete.cases(dfcount)], dfcount$male.st.pop[complete.cases(dfcount)]))/100000
                 , verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))

invisible(capture.output(
  biv4 <- BPO.reg(female.num ~  x1 + x2 + x3 , male.num ~ x1 + x2 + x3
                 , l3 = ~  x1 + x2 + x3
                 , zeroL3 = FALSE, data = dfcount[complete.cases(dfcount),]
                 , t = data.frame(rowMeans(cbind(dfcount$female.st.pop[complete.cases(dfcount)], dfcount$male.st.pop[complete.cases(dfcount)]))/100000,
                                  dfcount$female.st.pop[complete.cases(dfcount)]/100000, 
                                  dfcount$male.st.pop[complete.cases(dfcount)]/100000)
                 , verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))

invisible(capture.output(
  biv5 <- lm.dibp(female.rate ~  x1 + x2 + x3 , male.rate ~ x1 + x2 + x3
                 , l3 = ~  x1 + x2 + x3
                 , zeroL3 = FALSE, data = dfcount[complete.cases(dfcount),]
                 , verbose=FALSE) # zeroL3 is set to false for bivariate, true for regular GLM
))

# compare mcglm with bivpois ----
dfcount <- dfcount[complete.cases(dfcount),]
fit.female <- mcglm(c(female.num ~  x1 + x2 + x3), 
      list(mc_id(dfcount)), 
      data = dfcount, 
      variance = c("poisson_tweedie"), 
      link = c("log")
      , covariance = c("identity")
      # , offset = list(log(dfcount$female.st.pop/100000))
)

fit.male <- mcglm(c(male.rate ~  x1 + x2 + x3), 
                  list(mc_id(dfcount)), 
                  data = dfcount, 
                  variance = c("poisson_tweedie"), 
                  link = c("log")
                  , covariance = c("identity")
                  # ,  offset = list(log(dfcount$male.st.pop/100000))
                  )

# ini <- list()
# ini$regression <- list("female" = coef(fit.female, type = "beta")$Estimates,
#                           "male" = coef(fit.male, type = "beta")$Estimates)
# ini$power <- list("female" = coef(fit.female, type = "power")$Estimates,
#                   "male" = coef(fit.male, type = "power")$Estimates)
# ini$tau <- list("female" = coef(fit.female, type = "tau")$Estimates,
#                   "male" = coef(fit.male, type = "tau")$Estimates)
# ini$rho <- c(0, 0, 0)

fit.both <- mcglm(c(female.num ~  x1 + x2 + x3 , male.num  ~ x1 + x2 + x3), 
                  list(mc_id(dfcount), mc_id(dfcount)), 
                  data = dfcount, 
                  variance = c("poisson_tweedie","poisson_tweedie"), 
                  link = c("log", "log")
                  , covariance = c("identity", "identity")
                  , offset = list(log(dfcount$female.st.pop/100000), log(dfcount$male.st.pop/100000))
                  , control_algorith = list(max_iter = 20, tuning = 0.1, verbose = FALSE, tol = 1e-04)
                  )

estimates <- coef(fit.both, std.error = TRUE)
z_score <- coef(fit.both, std.error = TRUE)[,1]/coef(fit.both, std.error = TRUE)[,2]
p_values <- round(2*pnorm(-abs(z_score)),5)
data.frame(estimates, p_values = p_values)

dfcount$id <- 1
fit.both.dglm <- mcglm(c(female.num ~  x1 + x2 + x3 , male.num  ~ x1 + x2 + x3), 
                  list(mc_dglm(~  x1 + x2 + x3   , id = "id", dfcount), mc_dglm(~  x1 + x2 + x3  , id = "id", dfcount)), 
                  data = dfcount, 
                  variance = c("poisson_tweedie","poisson_tweedie"), 
                  link = c("log", "log")
                  , covariance = c("identity", "identity")
                  , offset = list(log(dfcount$female.st.pop/100000), log(dfcount$male.st.pop/100000))
                  , control_algorithm = list(max_iter = 1000, tuning = 0.01, verbose = FALSE, tol = 1e-06)
)

estimates <- coef(fit.both.dglm, std.error = TRUE)
z_score <- coef(fit.both.dglm, std.error = TRUE)[,1]/coef(fit.both.dglm, std.error = TRUE)[,2]
p_values <- round(2*pnorm(-abs(z_score)),5)
data.frame(estimates, p_values = p_values)

lambda3_rate <- simple.bp(dfcount$female.rate, dfcount$male.rate)
lambda3_num <- simple.bp(dfcount$female.num, dfcount$male.num, ini3 = 1.0, maxit = 300, pres = 1e-8)

summary( glm(female.rate ~ x1 + x2 + x3, data = dfcount, family = poisson, weights = female.st.pop/100000) )$coef
summary( glm(female.rate ~ x1 + x2 + x3, data = dfcount, family = poisson) )

summary( glm(male.rate ~ x1 + x2 + x3, data = dfcount, family = poisson, weights = male.st.pop/100000) )$coef
summary( glm(male.rate ~ x1 + x2 + x3, data = dfcount, family = poisson) )

# replicating lm.bp using multiroot (jbpm function) ----
dfcount <- dfcount[complete.cases(dfcount),]
n <- nrow(dfcount)
y1<-matrix(c(dfcount$female.rate), ncol=1,nrow=n)
y2<-matrix(c(dfcount$male.rate), ncol=1,nrow=n)
x<-matrix(c(rep(1,n),dfcount$x1,dfcount$x2, dfcount$x3), nrow=n,ncol=4)

jbpm <- function(b, parms){
  n <- nrow(y1)
  h<-matrix(NA, nrow=n, ncol=1)
  my<-pmin(y1,y2) #this gives min(y1,y2)
  lambda1<- exp(b[1]+b[2]*x[,2]+b[3]*x[,3]+b[4]*x[,4])
  lambda2<- exp(b[5]+b[6]*x[,2]+b[7]*x[,3]+b[8]*x[,4])
  lambda3<- exp(b[9]+b[10]*x[,2]+b[11]*x[,3]+b[12]*x[,4])
  for(i in 1:n){
    a1<- lambda1[i]
    a2<- lambda2[i]
    a3<- lambda3[i]
    n1<-y1[i]
    n2<-y2[i]
    w1<-0.0
    w2<-0.0
    for(z in 0:my[i]) {
      w1<-w1+(a3^z*a1^(n1-z)*a2^(n2-z)/(gamma(z+1)*gamma(n1-z+1)*gamma(n2-z+1)))
      w2<-w2+(a3^z*a1^(n1-z)*a2^(n2-z)*z/(gamma(z+1)*gamma(n1-z+1)*gamma(n2-z+1)))
    }
    w3<-w2/w1 
    h[i]<-w3
  }
  u10=sum(-lambda1+y1-h)
  u11=sum((-lambda1+y1-h)*x[,2])
  u12=sum((-lambda1+y1-h)*x[,3])
  u13=sum((-lambda1+y1-h)*x[,4])
  # u14=sum((-lambda1+y1-h)*x[,5])
  u20=sum(-lambda2+y2-h)
  u21=sum((-lambda2+y2-h)*x[,2])
  u22=sum((-lambda2+y2-h)*x[,3])
  u23=sum((-lambda2+y2-h)*x[,4])
  # u24=sum((-lambda2+y2-h)*x[,6])
  u30=sum(-lambda3+h)
  u31=sum((-lambda3+h)*x[,2])
  u32=sum((-lambda3+h)*x[,3])
  u33=sum((-lambda3+h)*x[,4])
  c(u10=u10,u11=u11,u12=u12,u13=u13
    # , u14=u14
    , u20=u20,u21=u21,u22=u22,u23=u23
    # , u24=u24
    , u30=u30,u31=u31
    , u32=u32,u33=u33
    )
}

fn<- function (b, parms)
{
  p1<-matrix(NA, nrow=n,ncol=1)
  h<-matrix(NA, nrow=n,ncol=1)
  my<-pmin(y1,y2) #this gives min(y1,y2)
  mu<-matrix(NA, nrow=n,ncol=1)
  
  lambda2 <- exp(b[1]+b[2]*x[,2]+b[3]*x[,3]+b[4]*x[,4])
  mu1 <- exp(b[5]+b[6]*x[,2]+b[7]*x[,3]+b[8]*x[,4])
  # logit_p1 <- b[9]+b[10]*x[,2]+b[11]*x[,3]+b[12]*x[,4]
  p1 <- mu1/ (1+mu1)
  
  for(i in 1:n){
    a1<-p1[i]
    a2<-lambda2[i]
    n1<-y1[i]
    n2<-y2[i]
    w1<-0.0
    w2<-0.0
    for(z in 0:my[i]) {
      db<- dbinom(z,n1,a1)
      dp<- dpois(n2-z,a2)
      w1<-w1+db*dp
      w2<-w2+z*db*dp
    }
    w3<-w2/w1 
    h[i]<-w3
    i=i+1
  }
  u10=sum(-lambda2+y2-h)
  u11=sum((-lambda2+y2-h)*x[,2])
  u12=sum((-lambda2+y2-h)*x[,3])
  u13=sum((-lambda2+y2-h)*x[,4])
  u20=sum(h- y1*p1)
  u21=sum((h - y1*p1)*x[,2])
  u22=sum((h - y1*p1)*x[,3])
  u23=sum((h - y1*p1)*x[,4])
  u30=sum(-mu+h)
  c(u10=u10,u11=u11,u12=u12,u13=u13, u20=u20,u21=u21,u22=u22,u23=u23, u30=u30)
}

initial <- c(summary(glm(female.rate ~ x1 + x2 + x3 , data = dfcount, family = poisson))$coef[, 1], 
             summary(glm(male.rate ~ x1 + x2 + x3 , data = dfcount, family = poisson))$coef[, 1], rep(0,4))

system.time(fit.jbpm <- rootSolve::multiroot(f=jbpm, start= initial
                                  , parms = c(y1, y2, x)
                              )
)

initial <- c(summary(glm(female.rate ~ x1 + x2 + x3 , data = dfcount, family = poisson))$coef[, 1], 
             summary(glm(male.rate ~ x1 + x2 + x3 , data = dfcount, family = poisson))$coef[, 1], rep(0,1))


system.time(fit.jbpm <- rootSolve::multiroot(f=fn, start= initial
                                             , parms = c(y1, y2, x)
)
)


