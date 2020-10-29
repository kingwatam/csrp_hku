rm(list=ls())
graphics.off()
# par(mar=c(0,0,0,0)) # set plot margins to 0
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(magrittr) # pipe 
library(dplyr)

setwd(sprintf("~%s/covid19", setpath))

df <- read.csv("https://github.com/ulklc/covid19-timeseries/raw/master/countryReport/raw/rawReport.csv")

table <- data.frame(country = unique(df$countryName))
table <- merge(table, unique(df[, c('countryName', 'region')]), by.x = c('country'), by.y = c('countryName'), all.x = TRUE)
df$day <- as.Date(df$day)

league <- df %>% filter(day == max(as.Date(unique(df$day))))
league$active <- league$confirmed - league$recovered - league$death
league$cumulative_cfr <- round(league$death/(league$recovered + league$death)*100, 1)
league$death_rate <- round(league$death/league$confirmed*100, 1)

global <- aggregate(. ~ day, data=df[,c('day', 'confirmed', 'death', 'recovered')], sum)
