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

setwd(sprintf("~%s/cc", setpath))
df <- readRDS("cc_02to17.rds")

# common occupations by ethnicity ----
# # version 1 of showing top most frequency occupations
# df %>%
#   group_by(ethnicity12, employ_status) %>%
#   dplyr::count(occup) %>%
#   top_n(5) %>%
#   as.data.frame()
# # version 2 with descending order for the count, but only showing first 5 instances per group
# df %>%
#   dplyr::count(ethnicity12, employ_status, occup) %>%
#   arrange(desc(n)) %>%
#   group_by(ethnicity12, employ_status) %>%
#   slice(seq_len(5)) %>%
#   as.data.frame() 

# version 3 that with descending order
nonChinese_occup_all <- df %>% 
  filter(ethnicity3=="Non-Chinese") %>%
  group_by(ethnicity12, employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  # top_n(n=9999, wt = n)  %>%
  arrange(ethnicity12, employ_status7, desc(n)) %>%
  group_by(ethnicity12, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA") 

# reshape2::dcast(., ethnicity12 + occup ~ employ_status, value.var = "n") %>%
#   convert2value(., NA, 0)

df$occup2 <-  ifelse(!(df$employ_status7 %in% c('Employed', 'Others', 'Underemployed')), 
                 df$employ_status7, 
                 df$occup)

occup_top3 <- df %>%  
  group_by(ethnicity3c, employ_status7, occup2) %>% 
  dplyr::summarise(n = n()) %>%
  top_n(n=4, wt = n)  %>%
  arrange(ethnicity3c, employ_status7, desc(n)) %>%
  group_by(ethnicity3c, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA")

Chinese_female_occup_top10 <- df %>% 
  filter(ethnicity3=="Chinese" & sex=="Female") %>%
  group_by(ethnicity3c, employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  top_n(n=10, wt = n)  %>%
  arrange(ethnicity3c, employ_status7, desc(n)) %>%
  group_by(ethnicity3c, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA")

Chinese_occup_top10 <- df %>% 
  filter(ethnicity3=="Chinese") %>%
  group_by(ethnicity3c, employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  top_n(n=10, wt = n)  %>%
  arrange(ethnicity3c, employ_status7, desc(n)) %>%
  group_by(ethnicity3c, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA")

Chinese_occup_all <- df %>% 
  filter(ethnicity3=="Chinese") %>%
  group_by(ethnicity3b, employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  # top_n(n=10, wt = n)  %>%
  arrange(ethnicity3b, employ_status7, desc(n)) %>%
  group_by(ethnicity3b, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA")

NA_occup_all <- df %>% 
  filter(is.na(ethnicity3)) %>%
  group_by(ethnicity3b, employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  # top_n(n=9999, wt = n)  %>%
  arrange(ethnicity3b, employ_status7, desc(n)) %>%
  group_by(ethnicity3b, employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA")

occup_all <- df %>% 
  group_by(employ_status7, occup) %>% 
  dplyr::summarise(n = n()) %>%
  # top_n(n=20, wt = n)  %>%
  arrange(employ_status7, desc(n)) %>%
  group_by(employ_status7) %>%
  # filter(row_number() <=5) %>% # optional: not showing more rows if there are ties
  as.data.frame() %>%
  convert2value(., NA, "NA") 

# get frequency tables ----
table_function <- function (df){
  age7_ethnicity3b <- get_freqtable(df$age7, df$ethnicity3b)
  age7_ethnicity12 <- get_freqtable(df$age7, df$ethnicity12)
  
  sex_age7_ethnicity3b <- get_freqtable(df$sex, df$age7, df$ethnicity3b)
  sex_age7_ethnicity12 <- get_freqtable(df$sex, df$age7, df$ethnicity12)
  
  ethnicity3b_employ_status7 <- get_freqtable(df$ethnicity3b, df$employ_status7)
  ethnicity12_employ_status7 <- get_freqtable(df$ethnicity12, df$employ_status7)
  
  age3_ethnicity3b_employ_status7 <- get_freqtable(df$age3, df$ethnicity3b, df$employ_status7)
  age3_ethnicity12_employ_status7 <- get_freqtable(df$age3, df$ethnicity12, df$employ_status7)
  
  ethnicity3b_income3 <- get_freqtable(df$ethnicity3b, df$income3)
  ethnicity12_income3 <-  get_freqtable(df$ethnicity12, df$income3)
  
  income3_employ_status7 <- get_freqtable(df$income3, df$employ_status7)
  
  income3_ethnicity3b_mean_median <- as.data.frame(
    df %>% # filter(income>0) %>%
      group_by(ethnicity3b, income3) %>% 
      summarise_at(.vars = 'income', list(n = ~length(.[!is.na(.)]),
                                          ~mean(., na.rm=TRUE),
                                          ~median(., na.rm=TRUE)))
  )
  income3_ethnicity3b_mean_median$mean[is.nan(income3_ethnicity3b_mean_median$mean)] <- NA
  income3_ethnicity3b_mean_median <- convert2value(income3_ethnicity3b_mean_median, NA, "NA")
  
  income3_ethnicity12_mean_median <- as.data.frame(
    df %>% # filter(income>0) %>%
      group_by(ethnicity12, income3) %>% 
      summarise_at(.vars = 'income', list(n = ~length(.[!is.na(.)]),
                                          ~mean(., na.rm=TRUE),
                                          ~median(., na.rm=TRUE)))
  )
  income3_ethnicity12_mean_median$mean[is.nan(income3_ethnicity12_mean_median$mean)] <- NA
  income3_ethnicity12_mean_median  <- convert2value(income3_ethnicity12_mean_median, NA, "NA")
  
  diemth_ethnicity3b <-  get_freqtable(df$diemth, df$ethnicity3b)
  diemth_ethnicity12 <- get_freqtable(df$diemth, df$ethnicity12)
  
  ethnicity3b_nowpsyprob<- get_freqtable(df$ethnicity3b, df$nowpsyprob)
  ethnicity12_nowpsyprob<- get_freqtable(df$ethnicity12, df$nowpsyprob)
  
  ethnicity3b_psyprob<- get_freqtable(df$ethnicity3b, df$psyprob) 
  ethnicity12_psyprob<- get_freqtable(df$ethnicity12, df$psyprob)
  
  ethnicity3b_nowpsytreat<- get_freqtable(df$ethnicity3b, df$nowpsytreat)
  ethnicity12_nowpsytreat<- get_freqtable(df$ethnicity12, df$nowpsytreat)
  
  ethnicity3b_medprob<- get_freqtable(df$ethnicity3b, df$medprob)
  ethnicity12_medprob<- get_freqtable(df$ethnicity12, df$medprob)
  
  write_excel('frequency_tables_general.xlsx', 
              diemth_ethnicity3b, diemth_ethnicity12,
              age7_ethnicity3b, age7_ethnicity12,
              sex_age7_ethnicity3b, sex_age7_ethnicity12)
  
  write_excel('frequency_tables_labour.xlsx', 
              ethnicity3b_employ_status7, ethnicity12_employ_status7, 
              age3_ethnicity3b_employ_status7, age3_ethnicity12_employ_status7,
              ethnicity3b_income3, ethnicity12_income3,
              income3_employ_status7, income3_ethnicity3b_mean_median, income3_ethnicity12_mean_median,
              occup_all, nonChinese_occup_all, Chinese_occup_top10, Chinese_occup_all, NA_occup_all)
  
  write_excel('frequency_tables_health.xlsx', 
              ethnicity3b_nowpsyprob, ethnicity12_nowpsyprob,
              ethnicity3b_psyprob, ethnicity12_psyprob,
              ethnicity3b_nowpsytreat, ethnicity12_nowpsytreat,
              ethnicity3b_medprob, ethnicity12_medprob)
}


ethnicity12_age7_year_NA <- df %$%
  table(ethnicity12, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 

ethnicity12_age7_year_NA_m <- df %>%
  filter(df$sex=='Male') %$%
  table(ethnicity12, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 

ethnicity12_age7_year_NA_f <- df %>%
  filter(df$sex=='Female') %$%
  table(ethnicity12, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 

bornarea3_age7_year_NA <- df %$%
  table(bornarea3, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 

bornarea3_age7_year_NA_m <- df %>%
  filter(df$sex=='Male') %$%
  table(bornarea3, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 

bornarea3_age7_year_NA_f <- df %>%
  filter(df$sex=='Female') %$%
  table(bornarea3, age7, year, useNA="ifany") %>%
  as.data.frame() %>%
  convert2value(., NA, "NA") 


setwd(sprintf("~%s/cc/ethnic_results", setpath))

write_excel(filename = "ethnicity12_age7_year_NA.xlsx", ethnicity12_age7_year_NA, ethnicity12_age7_year_NA_m, ethnicity12_age7_year_NA_f)

write_excel(filename = "bornarea3_age7_year_NA.xlsx", bornarea3_age7_year_NA, bornarea3_age7_year_NA_m, bornarea3_age7_year_NA_f)



table_function(df)
# xtabs(income3~ethnicity3b,aggregate(income3~ethnicity3b, df, mean))
