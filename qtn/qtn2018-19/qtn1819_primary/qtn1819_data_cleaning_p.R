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
library(descr) #  freq(as.ordered())
# library(tidyverse) # includes dplyr package
library(dplyr) # filter() recode()

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary", setpath))

df <- openxlsx::read.xlsx("Data_Universal_coded_workingfile.xlsx")
n_raw <- nrow(df)

df <- subset(df, select = c(id, ipaddr, Q01, Q02, Q03, Q04, submitdate, School.Code, `Pre_Ass1-P1-Total`,
                            `Pre_Ass1-P2-Happiness_Total`, `Pre_Ass1-P3-Total`, `Pre_Ass1-P4_-ve.thoughts`,
                            `Pre_Ass1-P4_+ve.thoughts`, `Pre_Ass1-P5-Total`, `Pre_Ass1-P6-Total`,
                            `Pre_Ass1-P7-Total`))

# # find column in df
# which(colnames(df)=="q7") or match("q7",names(df))

names(df)[names(df)=="Q01"] <- "class" 
names(df)[names(df)=="Q02"] <- "student_num"
df$Q03 <- openxlsx::convertToDateTime(df$Q03)
names(df)[names(df)=="Q03"] <- "dob"
df$submitdate <- convertToDateTime(df$submitdate)

names(df)[names(df)=="Q04"] <- "sex" 

df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

names(df)[names(df)=="Pre_Ass1-P1-Total"] <- "q1" 
names(df)[names(df)=="Pre_Ass1-P2-Happiness_Total"] <- "q2"
names(df)[names(df)=="Pre_Ass1-P3-Total"] <- "q3" 
names(df)[names(df)=="Pre_Ass1-P4_-ve.thoughts"] <- "q4neg" 
names(df)[names(df)=="Pre_Ass1-P4_+ve.thoughts"] <- "q4pos" 
names(df)[names(df)=="Pre_Ass1-P5-Total"] <- "q5" 
names(df)[names(df)=="Pre_Ass1-P6-Total"] <- "q6" 
names(df)[names(df)=="Pre_Ass1-P7-Total"] <- "q7" 

names(df)[names(df)=="School.Code"] <- "sch" 
df$sch <- as.integer(substr(df$sch,2,3))
df$sch[is.na(df$sch) & df$ipaddr == "210.3.117.89"] <- 1
df$sch[is.na(df$sch) & df$ipaddr == "218.189.204.146"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "210.177.245.124"] <- 5
df$sch[is.na(df$sch) & df$ipaddr == "210.176.51.222"] <- 7
df$sch[is.na(df$sch) & df$ipaddr == "220.241.161.78"] <- 9
df$sch[is.na(df$sch) & df$ipaddr == "115.160.146.146"] <- 10
df$sch[is.na(df$sch) & df$ipaddr == "218.188.218.194"] <- 11
df$sch[is.na(df$sch) & df$ipaddr == "210.3.199.114"] <- 14
df$sch[is.na(df$sch) & df$ipaddr == "210.3.102.78"] <- 15
df$sch[is.na(df$sch) & df$ipaddr == "210.3.74.230"] <- 16
df$sch[is.na(df$sch) & df$ipaddr == "210.176.226.78"] <- 17

df$grade <- as.integer(substr(df$class,1,1))

df$dob[df$id==6566] <- "2008-01-11" # change DOB of id 6566 to the same as id 1701, due to the same sch, class, student_numb, sex, similar DOBs with swapped month & date

df$control_paste <- ifelse(!is.na(df$sch), paste(df$sch, df$grade), NA)
df$control <- ifelse(!is.na(df$sch), 1, NA)

df$control[df$control_paste %in% c('1 5',
                                   '2 4',
                                   '3 5',
                                   '4 5',
                                   '5 5',
                                   '6 5',
                                   '7 4',
                                   '8 4',
                                   '9 5',
                                   '10 4',
                                   '11 4',
                                   '12 4',
                                   '13 4',
                                   '14 5',
                                   '15 4',
                                   '16 4',
                                   '17 5')] <- 0

df$intervention <- ifelse(df$control, 0, 1) #df$intervention <- !df$control #this will return true or false instead of binary

df$year <- as.numeric(format(as.Date(df$submitdate), "%Y"))
df$month <- as.numeric(format(as.Date(df$submitdate), "%m"))

df$T1 <- ifelse(!is.na(df$year), 0, NA)
df$T1[df$year==2019] <- 1
df$T1[df$sch == 1 & df$grade == 6 & df$year == 2019 & df$month <= 1] <- 0
df$T1[df$sch == 4 & df$grade == 4 & df$year == 2019 & df$month <= 2] <- 0
df$T1[df$sch == 6 & df$grade == 5 & df$year == 2019 & df$month <= 1] <- 0
df$T1[df$sch == 7 & df$grade == 4 & df$year == 2019 & df$month <= 2] <- 0
df$T1[df$sch == 12 & df$grade == 4 & df$year == 2019 & df$month <= 1] <- 0
df$T1[df$sch == 14 & df$grade == 6 & df$year == 2019 & df$month <= 1] <- 0
df$T1[df$sch == 15 & df$year == 2019 & df$month <= 1] <- 0

df$problem <- rep(0, nrow(df))
df$problem[df$control_paste %in% c('6 4', #n=2, two obs from school 6's T0 control
                                   '13 6', #n=1
                                   '17 6')] <- 1 #n=1

df$T2 <- rep(0, nrow(df)) #create variable for T2 of 2017-18 Intervention
df$T2[df$control_paste %in% c('1 6', #n=1
                              '2 5', #n=2 
                              '4 6', #n=1 
                              '8 5', #n=35
                             '11 5', #n=1
                             '12 5', #n=8
                             '13 5', #n=6
                             '14 6', #n=147
                             '15 5', #n=23
                             '16 5')] <- 1  #n=2

n_before <- nrow(df)
df <- df %>% filter(problem == 0 | is.na(problem), T2 == 0 | is.na(T2)) # drop 226 obs from T2, and 4 obs from problem variable
df <- df %>% filter(!is.na(sch)) # drop 24 obs missing sch
df$T2 <- NULL
df$problem <- NULL
df$control_paste <- NULL

df$q5[df$q5 == 99] <- NA
n_after <- nrow(df)

# selective program for 2019-2020 ----
df_se <- df %>% # selective
  filter(!is.na(sch)) %>%
  filter(T1 == 1, intervention == 1)

df_mean_intT1 <- df %>%
  filter(!is.na(sch)) %>%
  filter(T1 == 1, intervention == 1) %>%
  group_by(sch) %>%
  summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)

df_se <- distinct(df_se, sch, class, student_num, dob, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df_se <- df_se %>% filter(T1 == 1, intervention == 1)

df_se$sex <- ifelse(df_se$sex==2, "M", "F")
df_se <- df_se %>% arrange(df_se$sch, df_se$class, df_se$student_num)

table_loop <- function(df_se, df_mean_intT1){
  wb <- xlsx::createWorkbook(type="xlsx")
  for (i in (1:17)){
    if (i %in% df_se$sch){
      df_selective <- df_se %>% # selective
        filter(sch ==  i) %>%
        mutate(q6_rank = order(order(q6))) # rank q6 values by school from lowest to highest

      if (i==11){
        selective_n <- 48 # 25 more students  are wanted on top of the original 23
      } else {
        selective_n <- 20 # select at least 20 students
      }
      df_selective <- df_selective %>% filter((q6 <= max(.$q6[.$q6_rank<=selective_n])))

      df_selective_mean <- df_selective %>%
        filter(!is.na(sch)) %>%
        filter(T1 == 1, intervention == 1) %>%
        group_by(sch) %>%
        summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)

      table <- data.frame(matrix(ncol = 12,  nrow = 0))
      table <- subset(df_selective[df_selective$sch==i,], select = c("class",
                                                       "student_num",
                                                       "sex",
                                                       "dob",
                                                       "q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"))
      colnames(table)  <-  c("Class",
                             "Student Number",
                             "Sex",
                             "Date of Birth",
                             "Mental Health Knowledge",
                             "Subjective Happiness"	,
                             "Anxiety",
                             "Negative Thinking",
                             "Positive Thinking",
                             "Perspective Taking (Empathy)",
                             "Self-esteem",
                             "Gratitude")



      table$`Date of Birth` <- ifelse(is.na(table$`Date of Birth`), NA,
                                      as.character.Date(table$`Date of Birth`, tryFormats = "%Y/%m/%d"))
      table[nrow(table)+1,4] <- "Mean of Lowest Self-esteem Group"
      table[nrow(table)+1,4] <- "School Mean of Intervention Group"
      table[nrow(table)-1,5:length(table)] <- sapply(round(df_selective_mean[df_selective_mean$sch==i,2:length(df_selective_mean)], 2),function(x)format(x,nsmall = 2))
      table[nrow(table),5:length(table)] <- sapply(round(df_mean_intT1[df_mean_intT1$sch==i,2:length(df_mean_intT1)], 2),function(x)format(x,nsmall = 2))

      sheet <- xlsx::createSheet(wb, sheetName = paste0("S", deparse(as.numeric(i))))
      assign(paste0("sheet", deparse(as.numeric(i))), sheet)
      xlsx::addDataFrame(as.data.frame(table), get_("sheet", as.numeric(i)),
                   startRow=1, startColumn=1,
                   row.names = FALSE, showNA = FALSE)
    }
  }
  xlsx::saveWorkbook(wb, "qtn1819_pri_selective_qtn1920.xlsx", password=NULL)
}

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary/qtn1819_primary_selective_qtn1920", setpath))

table_loop(df_se, df_mean_intT1)

rm(df_mean_intT1, df_selective_mean)

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary", setpath))

# data cleaning continues ----

df <- df[order(df$submitdate),]
df <- distinct(df, sch, class, student_num, dob, T1, sex, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df[ ,c('id', 'ipaddr','year', 'month')] <- list(NULL)
n_final <- nrow(df)
saveRDS(df, file = "qtn1819_primary_long.rds")
# write_excel("qtn1819_primary_long.xlsx", df[])

### matching pupils for within-person analysis from here

# dfwide <- reshape(df,
#                   idvar = c("sch", "class", "student_num", "dob", "sex", 
#                             "control", "intervention", "grade"), # this line is to keep variables
#                   timevar = "T1", 
#                   direction = "wide")

df_pre <- df[df$T1==0,]
df_pre$T1 <- NULL
df_post <- df[df$T1==1,]
df_post$T1 <- NULL
# df_long<-rbind(df_pre,df_post) 
dfwide <- merge(df_pre, df_post, by.x = c("sch", "class", "student_num", 
                                          "dob", "sex",
                                         "control", "intervention", "grade"),
               by.y = c("sch", "class", "student_num", 
                        "dob", "sex",
                        "control", "intervention", "grade"),
               all.x=TRUE, all.y = TRUE, suffixes = c(".0", ".1"))

dfwide_both <-merge(df_pre, df_post, by.x = c("sch", "class", "student_num", 
                                              "dob", "sex",
                                        "control", "intervention", "grade"),
              by.y = c("sch", "class", "student_num", 
                       "dob", "sex",
                       "control", "intervention", "grade"),
              all.x=FALSE, all.y = FALSE, suffixes = c(".0", ".1"))

# # experimental code (which reduces the strength of the relationships in within-subject analysis using wide-format data)
# # Attempt to get more matches (by not using dob and sex in the process)
# 
# # Randomly select sex and dob from T0/T1 if they don't match
# dfwide$sexdiff <- ifelse(dfwide$sex.0!=dfwide$sex.1, 1, 0)
# dfwide$dobdiff <- ifelse(dfwide$dob.0!=dfwide$dob.1, 1, 0)
# 
# set.seed(49956) # a 5-digit random number generated from random.org with a range (1:100000), set as seed
# dfwide$rand_sex <- runif(nrow(dfwide), 0, 1)
# dfwide$rand_dob <- runif(nrow(dfwide), 0, 1)
# 
# # xlsx::write.xlsx(dfwide, "dfwide_primary.xlsx")
# 
# # This may not be an elegant solution but this has to be correct, a nested ifelse function is harder to debug
# dfwide$sex <- ifelse(dfwide$sexdiff==1 & dfwide$rand_sex<0.5, dfwide$sex.0,  NA) # > or >= should be equivalent because the probability of getting exactly 0.5 is zero in a continuous variable (but not sure if true in practice)
# dfwide$sex <- ifelse(dfwide$sexdiff==1 & dfwide$rand_sex>0.5, dfwide$sex.1,  dfwide$sex) # note that the negative condition is itself (dfwide$sex)
# dfwide$sex <- ifelse(dfwide$sexdiff==0, dfwide$sex.0,  dfwide$sex) # sex.0 or sex.1 here are the same when they are the same (same N as well)
# dfwide$sex <- ifelse(is.na(dfwide$sexdiff) & !is.na(dfwide$sex.0) & is.na(dfwide$sex.1), dfwide$sex.0, dfwide$sex) 
# dfwide$sex <- ifelse(is.na(dfwide$sexdiff) & !is.na(dfwide$sex.1) & is.na(dfwide$sex.0), dfwide$sex.1, dfwide$sex)
# 
# dfwide$dob <- ifelse(dfwide$dobdiff==1 & dfwide$rand_dob<0.5, format(dfwide$dob.0),  NA) # > or >= should be equivalent because the probability of getting exactly 0.5 is zero in a continuous variable (but not sure if true in practice)
# dfwide$dob <- ifelse(dfwide$dobdiff==1 & dfwide$rand_dob>0.5, format(dfwide$dob.1),  dfwide$dob) # note that the negative condition is itself (dfwide$dob)
# dfwide$dob <- ifelse(dfwide$dobdiff==0, format(dfwide$dob.0),  dfwide$dob) # dob.0 or dob.1 here are the same when they are the same (same N as well)
# dfwide$dob <- ifelse(is.na(dfwide$dobdiff) & !is.na(dfwide$dob.0) & is.na(dfwide$dob.1), format(dfwide$dob.0), dfwide$dob) 
# dfwide$dob <- ifelse(is.na(dfwide$dobdiff) & !is.na(dfwide$dob.1) & is.na(dfwide$dob.0), format(dfwide$dob.1), dfwide$dob)
# 
# dfwide$age.0 <- as.numeric(floor((as.Date(dfwide$submitdate.0)-as.Date(dfwide$dob))/365.2425)) # calculate age based on T1 submitdate
# dfwide$age.1 <- as.numeric(floor((as.Date(dfwide$submitdate.1)-as.Date(dfwide$dob))/365.2425)) # calculate age based on T1 submitdate
# # xlsx::write.xlsx(dfwide_both, "dfwide_both_primary.xlsx")

dfwide <- dfwide %>%
  # rowwise() %>% ## a slower method, pmax() is changed to max() with rowwise()
  mutate(age=pmax(age.0, age.1, na.rm=TRUE))

dfwide$q1 <- (dfwide$q1.1-dfwide$q1.0)
dfwide$q2 <- (dfwide$q2.1-dfwide$q2.0)
dfwide$q3 <- (dfwide$q3.1-dfwide$q3.0)
dfwide$q4neg <- (dfwide$q4neg.1-dfwide$q4neg.0)
dfwide$q4pos <- (dfwide$q4pos.1-dfwide$q4pos.0)
dfwide$q5 <- (dfwide$q5.1-dfwide$q5.0)
dfwide$q6 <- (dfwide$q6.1-dfwide$q6.0)
dfwide$q7 <- (dfwide$q7.1-dfwide$q7.0)

dfwide$T0 <- ifelse(!is.na(dfwide$age.0), 1, NA)
dfwide$T1 <- ifelse(!is.na(dfwide$age.1), 1, NA)

saveRDS(dfwide, file = "qtn1819_primary_wide.rds")