rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(magrittr) # pipes

setwd(sprintf("~%s/early_warning", setpath))

# youth vs total suicides from news 2019-2020
df <- openxlsx::read.xlsx("early_warning_stats_daily.xlsx",sheet = "suicide & youth suicide")
df <- df[, 1:3]
df$Date <- as.Date(df$Date, origin = "1899-12-30")
# df <- zoo(df[,2], seq(from = as.Date('2019-06-01'),  to =as.Date('2020-06-01'), by = 1))

library(surveillance)
library(zoo)

online <- openxlsx::read.xlsx("early_warning_stats_daily.xlsx",sheet = "suicide case & suicide threads")
online$Date <- as.Date(online$Date, origin = "1899-12-30")
names(online)[names(online)=='number.of.suicide.cases'] <- 'suicide_cases'
names(online)[names(online)=='number.of.suicide.threads.in.lihkg&hkgolden'] <- 'suicide_threads'

online$before_dec2019 <- ifelse(online$Date < '2019-11-23', 1, 0)

model <- lm(suicide_threads ~ suicide_cases*before_dec2019, data = online)
summary(model)


model <- glm(suicide_threads ~ 
               # lag(Date, k = c(1, 2, 3))+
               # Date+
               suicide_cases*before_dec2019, family = poisson(link = 'log'), data = online)
summary(model)
model_r2 <-  1 - summary(model)$deviance / summary(model)$null.deviance # for poisson this is the correct formulat (the logLik version below is a scalar multiple of this version), ref http://cameron.econ.ucdavis.edu/research/jbes96preprint.pdf 

# model_r2 <-  1 - logLik(model)[1] / logLik(update(model, suicide_threads ~ 1))[1]  # binomial/logistic glm specific R2

get_poi_dev <- function(model){ # get deviance for poisson GLM, model$deviance & deviance() aren't compatible with geeglm()
  sum(poisson()$dev.resids(model$y, fitted(model), wt = rep(1, length(model$y))))
}

print(model_r2)
acf(model$residuals)

online$id <- 1
model_ar <- geepack::geeglm(suicide_threads ~ 
                        suicide_cases*before_dec2019, 
                        corstr = "ar1",
                        id = id,
                        family = poisson(link = 'log'), data = online)
acf(model_ar$residuals)
model_ar_r2 <- 1-get_poi_dev(model_ar)/ get_poi_dev(update(model_ar, suicide_threads ~ 1))
print(model_ar_r2)



for (i in 8:12){
  for (j in 1:31){
  select_date <- paste0('2019-', i, '-', j)
  iferror(
    online$before_dec2019 <- ifelse(online$Date < select_date, 1, 0), next)
  model <- glm(suicide_threads ~ suicide_cases*before_dec2019, family = poisson(link = 'log'), data = online)
  summary(model)
  model_r2 <- 1 - summary(model)$deviance / summary(model)$null.deviance
  print(select_date)
  print(model_r2)
  }
}





sentinment <- openxlsx::read.xlsx("early_warning_stats_daily.xlsx",sheet = "sentiment")
sentinment$date <- as.Date(sentinment$date, origin = "1899-12-30")
