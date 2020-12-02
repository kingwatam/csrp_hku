rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(openxlsx)
library(plyr) 
library(dplyr)
library(magrittr) # pipe 
library(tidyr)
library(car) # recode
library(ggplot2)

setwd(sprintf("~%s/helium", setpath))
df <- openxlsx::read.xlsx("helium_cases_2011-17.xlsx", sheet = 'cases') # cases
df <- df[1:46,1:5]

df$date <- as.Date(df$date, origin = "1899-12-30")
df$info <- ifelse(df$male==1, paste0('#', df$case, ': ','M/', df$age, ' (n=', df$articles, ')'), paste0('#', df$case, ': ','F/', df$age, ' (n=', df$articles, ')'))

saveRDS(df, file = "helium_cases_2011-17.rds")

# monthly counts
dfcount <- openxlsx::read.xlsx("helium_cases_2011-17.xlsx", sheet = 1) # sheet 1 is count data
dfcount$month <- as.Date(dfcount$month, origin = "1899-12-30")

pop <- openxlsx::read.xlsx("helium_cases_2011-17.xlsx", sheet = 'population') #  is count data
pop$month <- as.Date(pop$month, origin = "1899-12-30")
pop$pop <- zoo::na.approx(pop$pop, x=pop$month)
pop <- pop[(which(pop$month==dfcount$month[1]):which(pop$month==dfcount$month[length(dfcount$month)])),] # keep only relevant months for population numbers (same months as dfcount)
write.xlsx(pop[], "population.xlsx")

# pseudo-intensity
dfcount$intensity <- dfcount$count/pop$pop*1000000
intensity <-  ggplot(dfcount, aes(x=midmonth,y=intensity)) +
  geom_line()
print(intensity)

# count chart
dfcount$midmonth <- as.Date(dfcount$month+14)
count_line <-  ggplot(dfcount, aes(x=midmonth,y=count)) + 
  geom_line() + xlab('time')
print(count_line)

month_date_range <- seq(min(dfcount$midmonth) , max(dfcount$midmonth) , by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(dfcount$midmonth), max(dfcount$midmonth) , by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)
year_df$year_date_range <- year_df$year_date_range + months(5)

count_bar <-  ggplot(dfcount, aes(x=midmonth,y=count)) + 
  geom_bar(stat="identity") + xlab(element_blank()) + scale_x_date(date_minor_breaks = "1 month",  date_breaks = "1 year") +
  geom_text(data=month_df, aes(x=month_date_range, y = -0.03, label=month_format), size= 2.5, color='black', angle=0) + # month labels
  geom_text(data=year_df, aes(x=year_date_range,y = -0.1, label=year_format), size=3, color='black') + # year labels
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
print(count_bar)

# scan statistics
library(scanstatistics)
poisson_result <- scan_pb_poisson(counts = dfcount$count, 
                                  zones = rep(1, nrow(dfcount)), 
                                  n_mcsim = 100,
                                  max_only = TRUE)
print(poisson_result)

