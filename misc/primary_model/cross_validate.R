rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

setwd(sprintf("~%s/misc/primary_model", setpath))

df <- openxlsx::read.xlsx("ElecVote_1828-2020.xlsx")

# library(doParallel)
# registerDoParallel(makePSOCKcluster(parallel::detectCores()))
library(dplyr)
library(forecast)

# converge <- data.frame()
# for (i in 2:20){
#   cv <- caret::train(ev_dem_538 ~ 1 + incumPriSupp + OppPriSupp  + pre1932, data = df[22:48,2:5],
#                method = "glm", family = poisson,
#                trControl = caret::trainControl(method = "repeatedcv",
#                                                number = i, repeats = 20, allowParallel = TRUE))
#   converge <- rbind(converge, data.frame("repeats" =  i, cv$results[, c("RMSE", "Rsquared", "MAE")]))
# }

set.seed(808176)
cv <- caret::train(ev_dem_538 ~ 1 + incumPriSupp + OppPriSupp  + pre1932, data = df[22:48,2:5],
                   method = "glm", family = gaussian,
                   trControl = caret::trainControl(method = "repeatedcv",
                                                   number = 10, repeats = 1000))
cv$results

cv_lag1 <- caret::train(ev_dem_538 ~ 1 + incumPriSupp + OppPriSupp  + pre1932 + lag(ev_dem_538, 1), data = df[22:48,2:5],
                   method = "glm", family = gaussian, na.action = na.pass, # cv.glm or caret can't handle models with lagged terms
                   trControl = caret::trainControl(method = "repeatedcv",
                                                   number = 10, repeats = 1000))
cv_lag1$results

cv_lag2 <- caret::train(ev_dem_538 ~ 1 + incumPriSupp + OppPriSupp  + pre1932 + lag(ev_dem_538, 1) + lag(ev_dem_538, 2), data = df[22:48,2:5],
                        method = "glm", family = gaussian, na.action = na.pass,
                        trControl = caret::trainControl(method = "repeatedcv",
                                                        number = 10, repeats = 1000))

cv_lag2$results

# arima
y <- ts(df$ev_dem_538[22:48], start = c(1), end = c(27), 1)
armodel <- Arima(y, order = c(0, 0, 0), xreg = as.matrix(df[22:48, 3:5]), include.constant =  TRUE)
forecast(armodel, xreg = as.matrix(df[22:49, 3:5]))

# expanding window cross-validation of arima
ar_func <- function(y, xreg, h) { # adapted from https://stats.stackexchange.com/questions/430876/tscv-auto-arima-with-xreg-results-in-nas
  ncol <- NCOL(xreg)
  x <- matrix(xreg[seq_along(y), ], ncol = ncol)
  if (NROW(xreg) < length(y) + h) {
    stop("Not enough xreg data for forecasting")
  }
  newX <- matrix(xreg[length(y) + seq(h) , ], ncol = ncol)
  fit <- Arima(y, order = c(1, 0, 0), xreg =  x, include.constant =  TRUE)
  forecast(fit, xreg = newX, h = 0)
}
ar_cv <- tsCV(y, ar_func, xreg = as.matrix(df[22:48, 3:5]), h = 1, initial = 5)
c(RMSE =  sqrt(mean(ar_cv^2, na.rm=TRUE)), MAE = mean(abs(ar_cv), na.rm=TRUE))

# Gaussian glm 
df$proportion <- df$ev_dem_538/538
glmmodel <- glm(ev_dem_538 ~ 1 + incumPriSupp + OppPriSupp  + pre1932 + lag(ev_dem_538, 1) + lag(ev_dem_538, 2),
                data = df[, c(2:5)], family = gaussian)
predict(glmmodel, newdata = df[, c(2:5)])

# logistic glm - gives the best prediction based on R2 (covXY/SDxSDy) and RMSE
binmodel <- glm(proportion ~ 1 + incumPriSupp + OppPriSupp  + pre1932 + lag(ev_dem_538, 1) + lag(ev_dem_538, 2) ,
                data = df[, c(2:5, 7)], family = binomial)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
predict(binmodel, newdata = df[, c(2:5)]) %>% logit2prob
# 150 Votes for Biden and 388 for Trump, but this primary model only uses primary results in Feb 2020 (thus before COVID-19 hit the US)

# unable to properly bootstrap the distribution of predictions for 2020, almost half of them result in NAs and the distribution looks uniform
boot_pred <- function(dat, indices){
  dat <- dat[indices,]
  fitboot <- glm(proportion ~ 1 + incumPriSupp + OppPriSupp  + pre1932 + lag(ev_dem_538, 1) + lag(ev_dem_538, 2) ,
                   data = dat[, c(2:5, 7)], family = binomial)
  return(coef(fitboot))
} 

boot_results <- boot::boot(data=df, statistic = boot_pred, parallel = 'snow', ncpus = parallel::detectCores(), R = 100)
