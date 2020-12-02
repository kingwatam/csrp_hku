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
library(ggplot2)

setwd(sprintf("~%s/cc/cc_data", setpath))
df <- openxlsx::read.xlsx("suicide_rates_eastern.xlsx")

# wide to long data format
df <- reshape(df,
               idvar = c("sex", "age", "district"), # this line is to keep variables
               varying = sprintf("%s",rep(2002:2017)),
               v.name=c("rate"),
               timevar = "year",
               times = 2002:2017,
               direction = "long")

df$sex <- ifelse(df$sex=='M', 'Male', 'Female')

df$years08_12 <- ifelse(df$year %in% c(2002:2007, 2013:2017), 0, 1)
df$eastern <- ifelse(df$district == 'Eastern', 1, 0)

# df <- df %>% filter(age != '0-14')

df <- df %>% filter(district %in% c('Eastern', 'Kowloon City'))
# df <- df %>% filter(!(district %in% c('Hong Kong')))

test_intervals <- function(df){
  # output statistical significant betas from different year intervals (pre-interval and post-interval)
  table  <-  data.frame(matrix(ncol = 1,  nrow = 0))
  count <- 1
  for (i in (2002:2007)){
    for (j in (2008:2017)){
      for (i_2 in (0:(2007-i))){
        for (j_2 in (0:(2017-j))){
          df$postvspre <- ifelse(df$year %in% (i:(i+i_2)), 0,
                                 ifelse(df$year %in% (j:(j+j_2)), 1, NA))
          DiDmodel <- lm(rate ~ sex*age + eastern*postvspre, data = df)
          if (i >= j | summary(DiDmodel)$coef['eastern:postvspre',4] >= 0.05){
            next
          }
          table[count, 1] <- ifelse(i==(i+i_2), i, sprintf("%s-%s", i,i+i_2))
          table[count, 2] <- ifelse(j==(j+j_2), j, sprintf("%s-%s", j,j+j_2))
          table[count, 3] <- summary(DiDmodel)$coef['eastern:postvspre',1]
          table[count, 4] <- summary(DiDmodel)$coef['eastern:postvspre',2]
          table[count, 5] <- summary(DiDmodel)$coef['eastern:postvspre',4]
          table[count, 6] <- summary(DiDmodel)$coef['eastern',1]
          table[count, 7] <- summary(DiDmodel)$coef['eastern',2]
          table[count, 8] <- summary(DiDmodel)$coef['eastern',4]
          table[count, 9] <- summary(DiDmodel)$coef['postvspre',1]
          table[count, 10] <- summary(DiDmodel)$coef['postvspre',2]
          table[count, 11] <- summary(DiDmodel)$coef['postvspre',4]
          count <- count+1
        }
      }
    }
  }
  names(table)[1:11] <- c('pre', 'post', 'eastern:postvspre', 'SE', 'p-value', 'eastern', 'SE', 'p-value', 'postvspre', 'SE', 'p-value')
  return(table)
}

# table <- test_intervals(df)

df$postvspre <- ifelse(df$year %in% (2004:2006), 0,
                       ifelse(df$year %in% (2008:2015), 1, NA))
DiDmodel <- (lm(rate ~ sex*age + postvspre*eastern, data = df)) 
summary(DiDmodel)
# ggiraphExtra::ggPredict(DiDmodel, se=TRUE, point=FALSE, interactive=FALSE) # only 3 independent variables allowed

plot_DiD <- ggeffects::ggemmeans(DiDmodel, terms = c('postvspre', 'eastern', 'age', 'sex')) %>%
  ggplot(., aes(x, predicted, color=group)) + geom_line() +
  facet_grid(rows = vars(panel), cols = vars(facet)) + labs(x = 'Pre vs Post', y = 'Suicide rate', colour = 'Districts') +
  scale_color_hue(labels = c("Kownloon City", "Eastern"))   +
  scale_x_continuous(breaks=c(0,1), labels=c('0'="2004-2006",'1'="2008-2015"), expand = c(0.06,0.06)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# visualise modelled changes over time 
df$year <- as.character(df$year)
DiDmodel <- (lm(rate ~ sex*age + year*eastern, data = df)) 
summary(DiDmodel)

plot_modelled <- ggeffects::ggemmeans(DiDmodel, terms = c('year', 'eastern', 'age', 'sex')) %>%
  ggplot(., aes(x, predicted, color=group, group=group)) + geom_point() + geom_line() +
  facet_grid(rows = vars(panel), cols = vars(facet)) + labs(x = 'Pre vs Post', y = 'Suicide rate', colour = 'Districts') +
  scale_color_hue(labels = c("Kownloon City", "Eastern"))   +
  scale_x_discrete(expand = c(0.06,0.06)) +
  theme(axis.text.x=element_text(angle=90)) 

# visualise observed changes over time (2002-2007 only)
plot_observed <- df %>% filter(year %in% c(2002:2007)) %>% ggplot(., aes(year, rate, color=as.factor(eastern), group=as.factor(eastern))) + geom_point() + geom_line() +
  facet_grid(rows = vars(sex), cols = vars(age)) + labs(x = 'Pre vs Post', y = 'Suicide rate', colour = 'Districts') +
  scale_color_hue(labels = c("Kownloon City", "Eastern"))  +
  theme(axis.text.x=element_text(angle=90))


int.starts <- 2008
int.ends <- 2012

df$xth_year <- df$year-2001

df$period <- ifelse(df$year %in% c(2002:int.starts-1), 'pre', 
                    ifelse(df$year %in% c((int.starts):int.ends), df$year, 
                           ifelse(df$year %in% c((int.ends+1):2017), 'post', NA)))

df$period_ <- ifelse(df$year %in% c(2002:2007), 'pre', 
                    ifelse(df$year %in% c(2008:2009), '2008-2009', 
                           ifelse(df$year %in% c(2010:2011), '2010-2011', 
                                  ifelse(df$year %in% c(2012:2015), '2012-2015', 
                                         ifelse(df$year %in% c(2016:2017), '2016-2017', NA)))))

# df$post <- ifelse(df$year >= 2011, 1, 0)

# df <- within(df, period <- relevel(period, ref = 'pre'))

DiDmodel <- lm(rate ~ sex*prevpost + age*prevpost + eastern*years08_12, data = df)
summary(DiDmodel)

df$period <- relevel(as.factor(df$period), ref = 'pre')
df$period_ <- relevel(as.factor(df$period_), ref = 'pre')

DiDmodel <- lm(rate ~  sex*age + xth_year + eastern*period, data = df)
summary(DiDmodel)

ggeffects::ggemmeans(DiDmodel, terms = c('period', 'eastern', 'age', 'sex')) %>% 
  ggplot(., aes(x, predicted, color=group, group=interaction(x,group))) + geom_point(position=position_dodge(width=0.5)) + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.1, position=position_dodge(width=0.5)) +
  facet_grid(rows = vars(panel), cols = vars(facet)) + labs(x = 'Pre vs Post', y = 'Suicide rate', colour = 'Districts') + 
  scale_color_hue(labels = c("Kownloon City", "Eastern"))   +
  # scale_x_continuous(breaks=c(0,1), labels=c('0'="2004-2006",'1'="2008-2015"), expand = c(0.06,0.06)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))

test_intervals_during <- function(df){
  # output statistical significant betas from different year intervals (pre-interval and post-interval)
  table  <-  data.frame(matrix(ncol = 1,  nrow = 0))
  count <- 1
  for (i in (2002:2007)){
    for (j in (2008:2017)){
      for (i_2 in (0:(2007-i))){
        for (j_2 in (0:(2017-j))){
          df$postvspre <- ifelse(df$year %in% (i:(i+i_2)), 0,
                                 ifelse(df$year %in% (j:(j+j_2)), 1, NA))
          
          df$period <- ifelse(df$year %in% c(i:int.starts-1), 'pre', 
                              ifelse(df$year %in% c((int.starts):int.ends), 'during', 
                                     ifelse(df$year %in% c((int.ends+1):2017), 'post', 0)))
          
          df$period <- relevel(as.factor(df$period), ref = 'pre')
          
          DiDmodel <- lm(rate ~ sex*age + eastern*period, data = df)
          if (i >= j | (summary(DiDmodel)$coef['eastern:periodduring',4] >= 0.05 & summary(DiDmodel)$coef['eastern:periodpost',4] >= 0.05)){
            next
          }
          table[count, 1] <- ifelse(i==(i+i_2), i, sprintf("%s-%s", i,i+i_2))
          table[count, 2] <- ifelse(j==(j+j_2), j, sprintf("%s-%s", j,j+j_2))
          table[count, 3] <- summary(DiDmodel)$coef['eastern:periodduring',1]
          table[count, 4] <- summary(DiDmodel)$coef['eastern:periodduring',4]

          table[count, 5] <- summary(DiDmodel)$coef['periodduring',1]
          table[count, 6] <- summary(DiDmodel)$coef['periodduring',4]
          
          table[count, 7] <- summary(DiDmodel)$coef['eastern:periodpost',1]
          table[count, 8] <- summary(DiDmodel)$coef['eastern:periodpost',4]
          
          table[count, 9] <- summary(DiDmodel)$coef['periodpost',1]
          table[count, 10] <- summary(DiDmodel)$coef['periodpost',4]
          
          table[count, 11] <- summary(DiDmodel)$coef['eastern',1]
          table[count, 12] <- summary(DiDmodel)$coef['eastern',4]
          count <- count+1
        }
      }
    }
  }
  names(table)[1:12] <- c('pre', 'post', 'eastern:periodduring', 'p-value', 'periodduring', 'p-value', 'eastern:periodpost', 'p-value', 'periodpost', 'p-value', 'eastern', 'p-value')
  return(table)
}