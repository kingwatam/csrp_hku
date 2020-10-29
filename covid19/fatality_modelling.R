rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(openxlsx)
library(limSolve) #solve
library(MASS) #ginv()

setwd(sprintf("~%s/covid19", setpath))

covid <- openxlsx::read.xlsx("covid19_fatality_rate.xlsx", sheet = "hubei")
covid$date <- as.Date(covid$date, origin = "1899-12-30")
sarshk <- openxlsx::read.xlsx("SARS_HK_BJ.xlsx", sheet = "HK")
sarsbj <- openxlsx::read.xlsx("SARS_HK_BJ.xlsx", sheet = "BJ")

# simulate daily death, recovery, fatality rates for disease duration k & n days of epidemic
get_rates <- function(dat, k, show_vector=TRUE, rate = "death"){
  df <- as.data.frame(dat$new.confirmed[1:(nrow(dat))])
  
  for (n in (1:(k))){
    if (k<=0){
      break
    }
    new_column <- as.data.frame(rep(0,n))
    new_column[(n+1):nrow(dat),1] <- dat$new.confirmed[1:(nrow(dat)-n)]
    df <- cbind(df, new_column)
  }
  df <- cbind(df, dat$new.deaths[1:(nrow(dat))])
  df <- cbind(df, dat$new.recovered[1:(nrow(dat))])
  
  C <- as.matrix(df[,0:k+1])
  D <- as.vector(df[,k+2])
  R <- as.vector(df[,k+3])
  DplusR <- D+R
  death_rates <- as.vector(solve_eq(C, D))
  recovery_rates <- as.vector(solve_eq(C, R))
  total_rates <- as.vector(solve_eq(C, DplusR))
  print(paste0("k = ", k, ", n = ", nrow(dat)))
  if (rate == "death"){
    if (show_vector){
      print(death_rates)
      # return(death_rates)
    }
    print(sum(death_rates))
    cat('\n')
    return(death_rates)
  } else if (rate == "recovery"){
    if (show_vector){
      print(recovery_rates)
      # return(recovery_rates)
    }
    print(sum(recovery_rates))
    cat('\n')
    return(recovery_rates)
  } else if (rate == "fatality") {
    fatality <- sum(death_rates)/(sum(death_rates)+sum(recovery_rates))
    # fatality <- sum(death_rates)/(sum(total_rates))
    print(fatality)
    cat('\n')
    return(fatality)
  }
}

solve_eq <- function(A, b, output='solution'){
  # limSolve::Solve(A, b)
  nnls(A,b)$X # non-negative least squares

  # G <- matrix(nrow = 2, byrow = TRUE, data = c(rep(-1, length(b)),
  #                                               rep(1, length(b))))
  # h <- c(-1, 0)
  # lsei(A=A, B=b, G=G, H=h)$X
  
  # linp(Co)
  # ginv(A) %*% b
  # qr.solve(A, b)
}

get_df <- function(dat, k, lastcolumn = "new.deaths"){
  df <- as.data.frame(dat$new.confirmed[1:(nrow(dat))])
  for (n in (1:k)){
    if (k<=0){
      break
    }
    new_column <- as.data.frame(rep(0,n))
    new_column[(n+1):nrow(dat),1] <- dat$new.confirmed[1:(nrow(dat)-n)]
    df <- cbind(df, new_column)
  }
  if (lastcolumn == "new.deaths"){
    df <- cbind(df, dat$new.deaths[1:(nrow(dat))])
  } else if (lastcolumn == "new.recovered") {
    df <- cbind(df, dat$new.recovered[1:(nrow(dat))])
  }
  names(df)[1:k+1] <- sprintf("%s%s", "new.confirmed", rep(1:k+1)) # rename column names
  names(df)[1] <- sprintf("%s%s", "new.confirmed", rep(1)) # rename column names
  names(df)[k+2] <- lastcolumn # rename column names
  return(df)
}

solve_eq_reg <- function(df, k){
  # constrained non-linear least squares (Nash Variant of the Marquardt algorithm)
  # coefficients <- summary(colf::colf_nlxb(as.formula(paste(colnames(df)[k+2], "~",
  #                                                  paste(colnames(df)[c(1:k+1)], collapse = "+"),"-1",
  #                                                  sep = "")),
  #                                 data = df, lower = rep(0, k+1))
  # )$coef[1:k+1]

  # # constrained ridge regression
  # coefficients <- coef(
  #   glmnetUtils::glmnet(as.formula(paste(colnames(df)[k+1], "~",
  #                                           paste(colnames(df)[c(1:k)], collapse = "+"),
  #                                           sep = "")),
  #                          data = df, alpha = 1, family="gaussian", intercept=FALSE, lower.limits = rep(0, k))
  #   , s = 0.0001)
  
  # # MLE still not working yet
  # ols.lf2 <- function(param) {
  #   mu <- param[1]
  #   theta <- param[-1]
  #   y <- as.vector(df[k+1]) #DV
  #   x <- df[1:k] #IV
  #   sigma <- x%*%theta   #multiply matrices
  #   sum(dnorm(y, mu, sigma, log = TRUE)) #normal distribution(vector of observations, mean, sd)
  # }
  # mle_ols <- maxLik::maxLik(logLik = ols.lf2, start = c(mu = 1, theta1 = 1, theta2 = 1, theta3 = 1), method="BFGS")
  # coefficients <- summary(mle_ols)$coef[1:k]

  # constrained non-linear least squares (port algorithm)
  coefficients <- summary(colf::colf_nls(as.formula(paste(colnames(df)[k+2], "~",
                                                           paste(colnames(df)[c(0:k+1)], collapse = "+"),"-1",
                                                           sep = "")),
                                          data = df, lower = rep(0, k+1))
  )$coef[1:k+1]

  # coefficients <- sum(summary(lm(as.formula(paste(colnames(df)[k+1], "~",
  #                                               paste(colnames(df)[c(1:k)], collapse = "+"), "-1",
  #                                               sep = "")),
  #                              data = df)
  #                           )$coef[1:k])
  return(as.vector(coefficients))
}

solve_eq_glm <- function(df, k){

    # constrained elastic net GLM
  coefficients <- coef(glmnet::glmnet(model.matrix(as.formula(paste(colnames(df)[k+2], "~",
                                                          paste(colnames(df)[c(0:k+1)], collapse = "+"),"-1",
                                                          sep = "")), df)[,-1], # this is x
                                         colnames(df)[k+2], # this is y
                                         family = "poisson", 
                                         lambda = 0, lower.limits = 0)
  )[1:k+1]

  return(as.vector(coefficients))
}

get_rates_reg <- function(dat, k, show_vector=TRUE, rate = "death", model = "reg"){
  df <- get_df(dat, k, lastcolumn = 'new.deaths')
  if (model == 'reg'){
    death_rates <- solve_eq_reg(get_df(dat, k, lastcolumn = 'new.deaths'), k)
    recovery_rates <- solve_eq_reg(get_df(dat, k, lastcolumn = 'new.recovered'), k)
  } else if (model == 'glm'){
    death_rates <- solve_eq_glm(get_df(dat, k, lastcolumn = 'new.deaths'), k)
    recovery_rates <- solve_eq_glm(get_df(dat, k, lastcolumn = 'new.recovered'), k)
  }
  print(paste0("k = ", k, ", n = ", nrow(dat)))
  if (rate == "death"){
    if (show_vector){
      print(death_rates)
    }
    print(sum(death_rates))
    cat('\n')
    return(death_rates)
  } else if (rate == "recovery"){
    if (show_vector){
      print(recovery_rates)
    }
    print(sum(recovery_rates))
    cat('\n')
    return(recovery_rates)
  } else if (rate == "fatality") {
    fatality <- sum(death_rates)/(sum(death_rates)+sum(recovery_rates))
    print(fatality)
    cat('\n')
    return(fatality)
  }
}

get_results <- function(data, n, type ="fatality", method = "inverse", mink = 0, maxk = 0){
  results <- data.frame(matrix(rep(NA, n^2), ncol=n))
  for (t in (1:(n))){
    
    if (max(0,mink) > (t-max(1,maxk))){
      next
    }
    for (k in (max(0,mink):(t-max(0,maxk)))){
      try({
        if (method=='regression'){
          invisible(capture.output(
            results[k,t] <- sum(get_rates_reg(covid[1:t,], k, show_vector=FALSE, rate = type))
          ))
        } else if (method=='inverse') {
          invisible(capture.output(
            results[k,t] <- sum(get_rates(data[1:t,], k, show_vector=FALSE, rate = type))
          ))
        } else if (method=='PAE') {
          invisible(capture.output(
            results[k,t] <- sum(get_rates_PAE(data[1:t,], k, show_vector=FALSE, rate = type))
          ))
        } else if (method=='glm') {
          invisible(capture.output(
            results[k,t] <- sum(get_rates_reg(data[1:t,], k, show_vector=FALSE, rate = type, model = 'glm'))
          ))
        }
      }, silent = TRUE)
    }
    print(paste(t))
  }
  return(results)
}

pick_k_old <- function(data, n, type ="fatality", mink = 1, maxk = 1){
  death <- data.frame(matrix(rep(NA, n^2), ncol=n))
  deviance_matrix <- data.frame(matrix(rep(NA, n^2), ncol=n))
  for (t in (1:n)){
    deviance0 <- Inf
    for (k in (mink:(t-maxk))){
      try({
      df <- get_df(data, k, lastcolumn = "new.deaths")
      C <- as.matrix(df[,0:k+1])
      D <- as.vector(df[,k+2])
      invisible(capture.output(
      death_rates <- get_rates(data[1:t,], k, show_vector=FALSE, rate = 'death')
      ))
      residuals <- rep(0, k+1)
      for (i in (1:(k+1))){
          residuals[i]<- crossprod(death_rates, C[i,])-D[i] # crossprod does sum of products with two vectors
        }
      deviance <- sum(abs(residuals))
      deviance_matrix[k,t] <- deviance
      if (deviance < deviance0){
        death[,t] <- NA
        death[k,t] <- sum(death_rates)
      }
      deviance0 <- deviance
      
      death[k,t] <- sum(death_rates)
      }, silent = TRUE)
    }
    print(paste(t))
  }
    if (type == "death"){
    return(deviance_matrix)
  }
  if (type == "recovery"){
  recovery <- data.frame(matrix(rep(NA, n^2), ncol=n))
  # for (t in (1:n)){
  #   deviance0 <- Inf
  #   for (k in (mink:(t-maxk))){
  #     try({
  #       df <- get_df(data, k, lastcolumn = "new.recovered")
  #       C <- as.matrix(df[,0:k+1])
  #       D <- as.vector(df[,k+2])
  #       invisible(capture.output(
  #         death_rates <- get_rates(data[1:t,], k, show_vector=FALSE, rate = 'recovery')
  #       ))
  #       residuals <- rep(0, k+1)
  #       for (i in (1:(k+1))){
  #         residuals[i]<- crossprod(death_rates, C[i,]) # crossprod does sum of products with two vectors
  #       }
  #       deviance <- sum(abs(residuals))
  #       if (deviance  deviance0){
  #         recovery[,t] <- NA
  #         recovery[k,t] <- sum(death_rates)
  #       }
  #       deviance0 <- deviance
  #     }, silent = TRUE)
  #   }
  #   print(paste(t))
  # }
    return(recovery)
  } 
 if (type == "fatality") {
    fatality <- sum(death_rates)/(sum(death_rates)+sum(recovery_rates))
    # fatality <- sum(death_rates)/(sum(total_rates))
    print(fatality)
    cat('\n')
    return(fatality)
  }
}

pick_k <- function(data, n, type ="fatality", mink = 1, maxk = 1){
  death <- data.frame(matrix(rep(NA, n^2), ncol=n))
  deviance_matrix <- data.frame(matrix(rep(NA, n^2), ncol=n))
  for (t in (1:n)){
    deviance0 <- Inf
    if (max(0,mink) > (t-max(1,maxk))){
      next
    }
    for (k in (max(0,mink):(t-max(0,maxk)))){
      try({
        df <- get_df(data, k, lastcolumn = "new.deaths")
        C <- as.matrix(df[,0:k+1])
        D <- as.vector(df[,k+2])
        invisible(capture.output(
          death_rates <- get_rates(data[1:t,], k, show_vector=FALSE, rate = 'death') # death probabilities from 1 to k
        ))
        death[k,t] <- sum(death_rates)
        death_rates <- NULL
      }, silent = TRUE)
    }
    print(paste(t))
  }
  if (type == "death"){
    return(death)
  }
  if (type == "recovery"){
    recovery <- data.frame(matrix(rep(NA, n^2), ncol=n))
    for (t in (1:n)){
      deviance0 <- Inf
      for (k in (mink:(t-maxk))){
        try({
          df <- get_df(data, k, lastcolumn = "new.recovered")
          C <- as.matrix(df[,0:k+1])
          D <- as.vector(df[,k+2])
          invisible(capture.output(
            death_rates <- get_rates(data[1:t,], k, show_vector=FALSE, rate = 'recovery')
          ))
          residuals <- rep(0, k+1)
          for (i in (1:(k+1))){
            residuals[i]<- crossprod(death_rates, C[i,]) # crossprod does sum of products with two vectors
          }
          deviance <- sum(abs(residuals))
          if (deviance < deviance0){
            recovery[,t] <- NA
            recovery[k,t] <- sum(death_rates)
          }
          deviance0 <- deviance
        }, silent = TRUE)
      }
      print(paste(t))
    }
    return(recovery)
  } 
  if (type == "fatality") {
    fatality <- sum(death_rates)/(sum(death_rates)+sum(recovery_rates))
    # fatality <- sum(death_rates)/(sum(total_rates))
    print(fatality)
    cat('\n')
    return(fatality)
  }
}

# covid$new.deaths <- covid$new.deaths+covid$new.recovered

n <- 50
death_covid <- get_results(covid, n, type = "death", mink = 0, maxk = 0)
death_covid1 <- pick_k_old(covid, n, type = "death", mink = 0, maxk = 0)
death_covid2 <- pick_k(covid, n, type = "death", mink =  0, maxk = 0)

death_covid3 <- death_covid2/death_covid

death_covid4 <- get_results(covid, n, type = "death", method = "regression", mink = 0, maxk = 0)
death_covid5 <- get_results(covid, n, type = "death", method = "inverse", mink = 0, maxk = 0)


rec_covid <- get_results(covid, n, type = "recovery", mink = 0, maxk = 0)


fatality_covid <- get_results(covid, n, type = "fatality", mink = 0, maxk = 0)
fatality_covid1 <- pick_k_old(covid, n, type = "fatality", mink = 0, maxk = 0)
fatality_covid2 <- pick_k(covid, n, type = "fatality", mink = 0, maxk = 0)



# #Testing colf::colf_nls
# n <- 27
# k <- 11
# df<- get_df(covid[1:n,], k, lastcolumn = 'new.deaths')
# summary(colf::colf_nls(as.formula(paste(colnames(df)[k+2], "~",
#                                         paste(colnames(df)[c(0:k+1)], collapse = "+"),"-1",
#                                         sep = "")),
#                        data = df, lower = rep(0, k+1))
# )

for (k in (1:(round(n)))){
  res <- try({#get_rates_reg(covid[1:n,], k, show_vector=TRUE, rate = "recovery")
    get_rates_reg(covid[1:n,], k, show_vector=TRUE, rate = "death")}, silent = TRUE)
  if (class(res) == "try-error") {
    break
  }
}

n <- 29
for (k in (1:(n-1))){
8}


for (k in (1:(round(n)))){
  invisible(capture.output(
    get_rates(covid[1:n,], k, show_vector=TRUE, rate = "death")
  ))
}

n <- 45
for (k in (2:(n))){
  get_rates(sarshk[1:n,], k, show_vector=TRUE, rate = "death")
}

n <- 58
for (k in (2:(round(n)))){
  # for (t in ((n-3):(n))){
    # if (k<=t){
      get_rates(covid[1:n,], k, show_vector=TRUE, rate = "fatality")
}

n <- 58
fatality_covid_ex <- get_results(covid[2:34,], n, type = "fatality", mink = 1, maxk = 1)
death_covid <- get_results(covid, n, type = "death", mink = 1, maxk = 1)

n <- 28 # nrow(sarsbj)
fatality_sarsbj <- get_results(sarsbj, n, type = "fatality")
death_sarsbj <- get_results(sarsbj, n, type = "death")

n <- 60
fatality_sarshk <- get_results(sarshk, n, type = "fatality")
death_sarshk <- get_results(sarshk, n, type = "death")

## recursive function as a function
# uniroot(function(x, c=0.113540887, ans=222.3606098, period=22){
#   func <- function(x, c, power){
#     (1+c*x^power)
#   }
#   f <- (1+c)
#   for (power in (1:period)){
#     f <- f * func(x, c, power)
#   }
#   return(f-ans)
# }
# , lower=0.0001, upper=10)$root

# PAE (proportional allotment estimator)
x0 <- D/rowSums(P)

for (n in (1:(k))){
  x0_newcolumn <- as.data.frame(rep(0,n))
  x0_newcolumn[(n+1):nrow(dat),1] <- x0[1:(nrow(dat)-n)]
  x0 <- cbind(x0, x0_newcolumn)
}


PAE <- function(dat, k, type = "death"){
  df <- as.data.frame(dat$new.confirmed[1:(nrow(dat))])
  
  for (n in (1:(k))){
    if (k<=0){
      break
    }
    new_column <- as.data.frame(rep(0,n))
    new_column[(n+1):nrow(dat),1] <- dat$new.confirmed[1:(nrow(dat)-n)]
    df <- cbind(df, new_column)
  }
  df <- cbind(df, dat$new.deaths[1:(nrow(dat))])
  df <- cbind(df, dat$new.recovered[1:(nrow(dat))])
  C <- as.matrix(df[,0:k+1])
  D <- as.vector(df[,k+2])
  R <- as.vector(df[,k+3])
  
  if (type == "recovery"){
    D <- R
  } 
  
  C_indicator <- ifelse(C > 0, 1, 0)
  x0 <- as.data.frame(D/rowSums(C))
  for (n in (1:(k))){
    x0_newcolumn <- as.data.frame(rep(0,n))
    x0_newcolumn[(n+1):nrow(x0),1] <- x0[(n+1):nrow(x0),1]
    x0 <- cbind(x0, x0_newcolumn)
  }
  x0 <- as.matrix(x0)
  x0 <- x0 * C_indicator
  u0 <- as.vector(colSums(C*x0)/colSums(C))

  u_delta <- 1
  iteration <- 0
  # print(paste(iteration, sum(u0)))
  
  while(u_delta >= 0.00001){ #u_delta >= 0.0000001 #iteration <= 30-1
    x1 <- matrix(rep(0, nrow(dat)*(k+1)), nrow = nrow(dat)) # matrix of zeros (n,k+1)
    for (i in (1:nrow(dat))){
      for (j in (1:(k+1))){
        x1[i,j]<- (D[i]*u0[j])/crossprod(u0, C[i,]) # crossprod does sum of products with two vectors
      }
    }
    x1 <- x1 * C_indicator
    u1 <- as.vector(colSums(C*x1)/colSums(C))
    u_delta <- abs(sum(u0)-sum(u1))
    u0 <- u1
    iteration <- iteration + 1
  }
  return(u0)
}

get_rates_PAE <- function(dat, k, show_vector=TRUE, rate = "death"){
  print(paste0("k = ", k, ", n = ", nrow(dat)))
  if (rate == "death"){
    death_rates <- PAE(dat, k, type = rate)
    if (show_vector){
      print(death_rates)
      # return(death_rates)
    }
    print(sum(death_rates))
    cat('\n')
    return(sum(death_rates))
  } else if (rate == "recovery"){
    recovery_rates <- PAE(dat, k, type = rate)
    if (show_vector){
      print(recovery_rates)
      # return(recovery_rates)
    }
    print(sum(recovery_rates))
    cat('\n')
    return(sum(recovery_rates))
  } else if (rate == "fatality") {
    death_rates <- PAE(dat, k, type = "death")
    recovery_rates <- PAE(dat, k, type = "recovery")
    fatality <- sum(death_rates)/(sum(death_rates)+sum(recovery_rates))
    # fatality <- sum(death_rates)/(sum(total_rates))
    print(fatality)
    cat('\n')
    return(fatality)
  }
}

get_rates_PAE(covid[1:28,], 20, , rate='recovery')

n <- 34
fatality_covid_PAE <- get_results(covid, n, type = "fatality", method = 'PAE', mink = 1, maxk = 1)
death_covid_PAE <- get_results(covid, n, type = "death", method = 'PAE', mink = 1, maxk = 1)

n <- 40
death_sarshk <- get_results(sarshk, n, type = "death", mink = 1, maxk = 1)
death_sarshk_PAE <- get_results(sarshk, n, type = "death", method = 'PAE', mink = 1, maxk = 1)
fatality_sarshk <- get_results(sarshk, n, type = "fatality", , mink = 1, maxk = 1)
fatality_sarshk_PAE <- get_results(sarshk, n, type = "fatality", method = 'PAE', mink = 1, maxk = 1)

dat <- covid
test <- get_df(dat, k, lastcolumn = 'new.deaths')

C <- as.matrix(df[,1:k])
D <- as.vector(df[,k+1])
R <- as.vector(df[,k+2])

# solving for a single x variable in an equation (recursive function with only a forloop)
uniroot(function(x, c=0.009721593, ans=187.4523175, period=30-2){ 
  f <- (1+c) # 2nd period
  for (power in (1:period)){ # third period to last period
    f <- f * (1+c/x^power)
  }
  return(f-ans)
}
, lower=0.00001, upper=2, tol = 0.0000001)$root

saveRDS(fatality_covid_PAE, file = "fatality_covid_PAE.rds")
saveRDS(death_covid_PAE, file = "death_covid_PAE.rds")

saveRDS(fatality_sarshk_PAE, file = "fatality_sarshk_PAE.rds")
saveRDS(death_sarshk_PAE, file = "death_sarshk_PAE.rds")

fatality_sarshk_PAE <- readRDS("fatality_sarshk_PAE.rds")
death_sarshk_PAE <- readRDS("death_sarshk_PAE.rds")

fatality_covid_PAE <- readRDS("fatality_covid_PAE.rds")
death_covid_PAE <- readRDS("death_covid_PAE.rds")