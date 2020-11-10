rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))

# S01-S14 schools ----
df_pre <- openxlsx::read.xlsx("pre_post_simple_cleaned_king.xlsx",sheet = "pre") # n=1629
df_post <- openxlsx::read.xlsx("pre_post_simple_cleaned_king.xlsx",sheet = "post") # n=1271

df_post$submitdate <-  format(as.Date(df_post$submitdate,format='%d-%b-%Y'))
df_pre$submitdate <-  format(as.Date(df_pre$submitdate, format='%Y-%m-%d'))

df_post$dob <-  format(as.Date(df_post$dob,format='%d-%b-%Y'))
df_pre$dob <-  format(as.Date(df_pre$dob, format='%Y-%m-%d'))

df_pre$T1 <- 0
df_post$T1 <- 1

df <- rbind(df_pre,df_post) 
rm(df_pre, df_post)

names(df)[names(df)=="gender"] <- "sex"
df$class <- paste0(df$form, df$class) # combine form and class letter
names(df)[names(df)=="class_num"] <- "student_num"

names(df)[names(df)=="P1_Total"] <- "q1" 
names(df)[names(df)=="P2_Total"] <- "q2"
names(df)[names(df)=="P1_P2_Total"] <- "q1_2" 
names(df)[names(df)=="P3_Total"] <- "q3"
names(df)[names(df)=="P4_Total_Positive"] <- "q4pos" 
names(df)[names(df)=="P4_Total_Negative"] <- "q4neg"
names(df)[names(df)=="P5xa_Total"] <- "q5a" 
names(df)[names(df)=="P5xb_Total"] <- "q5b"
names(df)[names(df)=="P6_Total"] <- "q6"
names(df)[names(df)=="P7_Total"] <- "q7"

df$q3[df$q3==1140] <- NA
df$q4neg[df$q4neg==950] <- NA
df$q4pos[df$q4pos==950] <- NA
df$q5a[df$q5a==665] <- NA
df$q5b[df$q5b==475] <- NA
df$q6[df$q6==570] <- NA
df$q7[df$q7==570] <- NA

# summ(df$q1, df$q2, df$q1_2, df$q3, df$q4neg, df$q4pos, df$q5a, df$q5b, df$q6, df$q7)

names(df)[names(df)=="school"] <- "sch"
df$sch <- as.integer(substr(df$sch,2,3))
df$schNA <- ifelse(is.na(df$sch), 1, 0) # flag missing sch values before adding back sch information from ip address, used for classifying int/ctrl later
df$sch[is.na(df$sch) & df$ipaddr == "165.84.186.4"] <- 5 # n=3
df$sch[is.na(df$sch) & df$ipaddr == "192.168.0.200"] <- 11 #n=3
df$sch[is.na(df$sch) & df$ipaddr == "192.168.0.9"] <- 11 #n=1
df$sch[is.na(df$sch) & df$ipaddr == "202.155.239.194"] <- 8 #n=5
df$sch[is.na(df$sch) & df$ipaddr == "210.176.116.13"] <- 3 #n=2
df$sch[is.na(df$sch) & df$ipaddr == "221.126.237.98"] <- 6 #n=4
df$sch[is.na(df$sch) & df$ipaddr == "223.197.10.33"] <- 14 #n=10
df$sch[is.na(df$sch) & df$ipaddr == "223.255.166.222"] <- 8 #n=17
df$sch[is.na(df$sch) & df$ipaddr == "61.238.98.99"] <- 11 #n=18

names(df)[names(df)=="int_ctrl"] <- "control"
df$control <- ifelse(df$control=="ctrl", 1, 0)

df$control[df$schNA==1 & df$sch == 5 & df$form == 1] <- 1
df$control[df$schNA==1 & df$sch == 11 & df$form == 1] <- 1
df$control[df$schNA==1 & df$sch == 11 & df$form == 2] <- 0
df$control[df$schNA==1 & df$sch == 8 & df$form == 1] <- 1
df$control[df$schNA==1 & df$sch == 8 & df$form == 2] <- 0
df$control[df$schNA==1 & df$sch == 3 & df$form == 3] <- 1
df$control[df$schNA==1 & df$sch == 3 & df$form == 4] <- 0
df$control[df$schNA==1 & df$sch == 6 & df$form == 1] <- 0
df$control[df$schNA==1 & df$sch == 14 & df$form == 1] <- 0
df$schNA <- NULL

df$intervention <- ifelse(df$control, 0, 1) #df$intervention <- !df$control #this will return true or false instead of binary

# atttempt to guess missing submitdate from T1 and non-missing submitdate
# median of T0/T1 submitdate from other schools are used if no submitdate available for a specific school 
# median(df$submitdate[df$T1==0], na.rm = TRUE)
# median(as.Date(df$submitdate[df$T1==1]), na.rm = TRUE)
df$submitdate[is.na(df$submitdate) & df$T1 == 0 & df$sch == 3 & df$form == 4] <- format(as.Date("28-Sep-2018", format='%d-%b-%Y'))
df$submitdate[is.na(df$submitdate) & df$T1 == 0 & df$sch == 7 & df$form == 1] <- format(as.Date("16-Oct-2018", format='%d-%b-%Y'))
df$submitdate[is.na(df$submitdate) & df$T1 == 1 & df$sch == 7 & df$form == 1] <- format(as.Date("29-May-2019", format='%d-%b-%Y'))
df$submitdate[is.na(df$submitdate) & df$T1 == 0 & df$sch == 8 & df$form == 2] <- format(as.Date("11-Oct-2018", format='%d-%b-%Y'))
df$submitdate[is.na(df$submitdate) & df$T1 == 0 & df$sch == 10 & df$form == 1] <- format(as.Date("16-Oct-2018", format='%d-%b-%Y'))
df$submitdate[is.na(df$submitdate) & df$T1 == 0 & df$sch == 14 & df$form == 1] <- format(as.Date("16-Oct-2018", format='%d-%b-%Y'))

# excel formulas for moving dob info from T1 to T0 where dob is missing
# =DATE(RIGHT(G2,4),MID(G2,4,2),LEFT(G2,2))
# =VLOOKUP(J2,$C$2:$H$31,6, FALSE)
# =TEXT(K2,"yyyy-mm-dd")

df$dob <- format(as.Date(df$dob)) # may not be needed, age formula gives error
df$age <- as.numeric(floor((as.Date(df$submitdate)-as.Date(df$dob))/365.2425))

# df$year <- as.numeric(format(as.Date(df$submitdate),"%Y"))
# df$month <- as.numeric(format(as.Date(df$submitdate),"%m"))
# df$day <- as.numeric(format(as.Date(df$submitdate),"%d"))

# remove missing sch & missing control (i.e. problematic sch&form)
df <- df[!is.na(df$sch),]
df <- df[!is.na(df$control),]
df <- df[!is.na(df$age),]

n_before <- nrow(df)
df <- df[order(df$submitdate),]
df <- distinct(df, sch, class, student_num, dob, T1, sex, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df$sex[df$sch==10 & df$class=="1A" & df$student_num==7] <- 1 # from S10 class name list
df$sex[df$sch==10 & df$class=="1A" & df$student_num==27] <- 2 # from S10 class name list

# S15 School DMS ----
dfDMS_pre <- openxlsx::read.xlsx("QTN_18-19_SecSch_DataEntry_DMS_pre_post_king.xlsx",sheet = "pre")
dfDMS_post <- openxlsx::read.xlsx("QTN_18-19_SecSch_DataEntry_DMS_pre_post_king.xlsx",sheet = "post")

dfDMS_pre$T1 <- 0
dfDMS_post$T1 <- 1

dfDMS <- plyr::rbind.fill(dfDMS_pre,dfDMS_post) 
rm(dfDMS_pre,dfDMS_post) 
dfDMS$X88 <- NULL #empty column
dfDMS$X87 <- NULL #empty column
dfDMS$'DOB(dd/mm/yyyy)' <- NULL #empty column

dfDMS[dfDMS==98] <- NA
dfDMS[dfDMS==99] <- NA

names(dfDMS)[names(dfDMS)=='1'] <- 'sch'
dfDMS$sch <- 15

names(dfDMS)[names(dfDMS)=='Sex.(1=M,2=F)'] <- 'sex'
dfDMS$sex <- ifelse(dfDMS$sex == 2, 1, 2) # recoded to 2=F, 1=M as the other schools (main dataset)
  
names(dfDMS)[names(dfDMS)=='Intervention/Control.(1=int;.2=.control)'] <- 'control'
dfDMS$control <- ifelse(dfDMS$control == '2', 1, 0)

dfDMS$intervention <- ifelse(dfDMS$control, 0, 1)

names(dfDMS)[names(dfDMS)=='Class'] <- 'class'
names(dfDMS)[names(dfDMS)=='Class.Number'] <- 'student_num'

dfDMS$form <- as.integer(substr(dfDMS$class,1,1))

dfDMS$dob <- as.Date(paste0(dfDMS$'DOB.(day)', "/", dfDMS$'DOB.(month)', "/", dfDMS$'DOB.(year)'), format='%d/%m/%Y')

names(dfDMS)[names(dfDMS)=='Ass1.Questionnaire.Date.(dd/mm/yyyy)'] <- 'submitdate'
dfDMS$submitdate <-  openxlsx::convertToDateTime(dfDMS$submitdate)

dfDMS$age <- as.numeric(floor((as.Date(dfDMS$submitdate)-as.Date(dfDMS$dob))/365.2425))

dfDMS %>% select(starts_with("Ass1-P1")) %>% colnames(.) -> qA1 #q1
dfDMS %>% select(starts_with("Ass1-P2")) %>% colnames(.) -> qA2 #q2
dfDMS %>% select(starts_with("Ass1-P3")) %>% colnames(.) -> qB #q3
dfDMS %>% select(starts_with("Ass1-P4")) %>% colnames(.) -> qC #q4pos & q4neg
dfDMS %>% select(starts_with("Ass1-P5A")) %>% colnames(.) -> qD1 #q5a
dfDMS %>% select(starts_with("Ass1-P5B")) %>% colnames(.) -> qD2 #q5b
dfDMS %>% select(starts_with("Ass1-P6")) %>% colnames(.) -> qE #q6
dfDMS %>% select(starts_with("Ass1-P7")) %>% colnames(.) -> qF #q7

# S15 Outcome Scoring ----
reverse_qA1 <- qA1[c(2, 3, 7, 8, 9, 12)] 
dfDMS[reverse_qA1] <- (dfDMS[reverse_qA1]-1)*-1  # reverse (0,1) to (1,0)

dfDMS[,qA2[1]] <- ifelse(with(dfDMS, get(qA2[1])) == 4, 1, 0)
dfDMS[,qA2[2]] <- ifelse(with(dfDMS, get(qA2[2])) == 3, 1, 0)
dfDMS[,qA2[3]] <- ifelse(with(dfDMS, get(qA2[3])) == 1, 1, 0)
dfDMS[,qA2[4]] <- ifelse(with(dfDMS, get(qA2[4])) == 4, 1, 0)
dfDMS[,qA2[5]] <- ifelse(with(dfDMS, get(qA2[5])) == 4, 1, 0)
dfDMS[,qA2[6]] <- ifelse(with(dfDMS, get(qA2[6])) == 3, 1, 0)
dfDMS[,qA2[7]] <- ifelse(with(dfDMS, get(qA2[7])) == 4, 1, 0)
dfDMS[,qA2[8]] <- ifelse(with(dfDMS, get(qA2[8])) == 4, 1, 0)

reverse_qB <- qB[c(1, 3, 4, 7, 8, 12)] # GHQ
dfDMS[reverse_qB] <- (dfDMS[reverse_qB]-3)*-1  # reverse (0:3) to (3:0)

reverse_qC <- qC[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking

reverse_qD1 <- qD1[c(3, 4)]
dfDMS[reverse_qD1] <- (dfDMS[reverse_qD1]-7)*-1 # reverse (1:6) to (6:1)

reverse_qE <- qE[c(3)] 
dfDMS[reverse_qE] <- (dfDMS[reverse_qE]-4)*-1  # reverse (0:4) to (4:0)

reverse_qF <- qF[c(3, 6)] 
dfDMS[reverse_qF] <- (dfDMS[reverse_qF]-8)*-1  # reverse (1:7) to (7:1)

dfDMS <- dfDMS %>% 
  mutate(q1 = rowSums(.[qA1], na.rm = TRUE), # NA is treated as wrong answers, so we throw away NAs
         q2 = rowSums(.[qA2], na.rm = TRUE), # Q1&2 are Mental Health Knowledge
         q3 = rowSums(.[qB], na.rm = FALSE), # GHQ Psychological Distress
         q4neg = rowSums(.[reverse_qC], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q4pos = rowSums(.[qC[!(qC %in% reverse_qC)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
         q5a = rowSums(.[qD1], na.rm = FALSE), # SLSS, range(7-42)
         q5b = rowSums(.[qD2], na.rm = FALSE), # BMSLSS, range(5-35)
         q6 = rowSums(.[qE], na.rm = FALSE), # Empathy
         q7 = rowSums(.[qF], na.rm = FALSE) # Gratitude
         )

dfDMS$q1_2 <- dfDMS$q1 + dfDMS$q2

dfDMS <- subset(dfDMS, select = c(sch, form, class, student_num, age, sex, 
                                  q1, q2, q1_2, q3, q4pos, q4neg, q5a, q5b, q6, q7, 
                                  control, submitdate, dob, T1, intervention))

df[ ,c('id', 'ipaddr')] <- list(NULL)
df <- plyr::rbind.fill(df, dfDMS)
rm(dfDMS)

n_final <- nrow(df)
saveRDS(df, file = "qtn1819_secondary_long.rds")
# write_excel("qtn1819_secondary_long.xlsx", df)

# selective program for 2019-2020 ----
df_se <- df %>% # selective
  filter(!is.na(sch)) %>%
  filter(T1 == 1, intervention == 1)

df_mean_intT1 <- df_se %>%
  group_by(sch) %>%
  summarise_at(c("q1_2", "q3", "q4neg", "q4pos", "q5a", "q5b", "q6", "q7"), mean, na.rm = TRUE)

df_se <- distinct(df_se, sch, class, student_num, dob, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df_se$sex <- ifelse(df_se$sex==2, "M", "F")
df_se <- df_se %>% arrange(df_se$sch, df_se$class, df_se$student_num)

questions <- t(array(c(c("q1_2", "Mental Health Knowledge"),  
                       c("q3", "Psychological Distress"),  
                       c("q4neg", "Negative Thinking"), 
                       c("q4pos", "Positive Thinking"), 
                       c("q5a", "Life Satisfaction"),
                       c("q5b", "Life Satisfaction"),
                       c("q6", "Empathy"),  
                       c("q7", "Gratitude")), dim = c(2,8)))

table_loop <- function(df_se, df_mean_intT1, question, question_name, school = NULL){
  wb <- xlsx::createWorkbook(type="xlsx")
  for (i in (1:17)){
    if (!is.null(school)){
      i <- school
    }
    if (i %in% df_se$sch){
      if (question %in% c('q3', 'q4neg')){
        df_selective <- df_se %>% # selective
          filter(sch ==  i) %>%
          mutate(rank = order(order(-get(question)))) # rank q6 values by school from highest to lowest
        
        selective_n <- 30 # select at least 30 students
        
        df_selective <- df_selective %>% filter((get(question) >= min(get(question)[rank<=selective_n], na.rm=TRUE)))
        lowest_highest <- 'Highest '
      } else {
        df_selective <- df_se %>% # selective
          filter(sch ==  i) %>%
          mutate(rank = order(order(get(question)))) # rank q6 values by school from lowest to highest
        
        selective_n <- 30 # select at least 30 students
        
        df_selective <- df_selective %>% filter((get(question) <= max(get(question)[rank<=selective_n], na.rm=TRUE)))
        lowest_highest <- 'Lowest '
      }

      df_selective_mean <- df_selective %>%
        group_by(sch) %>%
        summarise_at(c("q1_2", "q3", "q4neg", "q4pos", "q5a", "q5b", "q6", "q7"), mean, na.rm = TRUE)
      
      table <- data.frame(matrix(ncol = 12,  nrow = 0))
      table <- subset(df_selective[df_selective$sch==i,], select = c("class",
                                                       "student_num",
                                                       "sex",
                                                       "dob",
                                                       "q1_2", "q3", "q4neg", "q4pos", "q5a", "q5b", "q6", "q7"))
      colnames(table)  <-  c("Class",
                             "Student Number",
                             "Sex",
                             "Date of Birth",
                             "Mental Health Knowledge",
                             "Psychological Distress"	,
                             "Negative Thinking",
                             "Positive Thinking",
                             "Life Satisfaction",
                             "Life Satisfaction",
                             "Empathy",
                             "Gratitude")
      
      table$`Date of Birth` <- ifelse(is.na(table$`Date of Birth`), NA,
                                      as.character.Date(table$`Date of Birth`, tryFormats = "%Y/%m/%d"))
      
      table[nrow(table)+1,4] <- paste0("Mean of ", lowest_highest, questions[which(questions[,1]==question),2]," Group")
      table[nrow(table)+1,4] <- "School Mean of Intervention Group"
      table[nrow(table)-1,5:length(table)] <- sapply(round(df_selective_mean[df_selective_mean$sch==i,2:length(df_selective_mean)], 2),function(x)format(x,nsmall = 2))
      table[nrow(table),5:length(table)] <- sapply(round(df_mean_intT1[df_mean_intT1$sch==i,2:length(df_mean_intT1)], 2),function(x)format(x,nsmall = 2))

      sheet <- xlsx::createSheet(wb, sheetName = paste0("S", deparse(as.numeric(i))))
      assign(paste0("sheet", deparse(as.numeric(i))), sheet)
      xlsx::addDataFrame(as.data.frame(table), get_("sheet", as.numeric(i)),
                   startRow=1, startColumn=1,
                   row.names = FALSE, showNA = FALSE)
    }
    if (!is.null(school)){
      break
    }
  }
  if (!is.null(school)){
    return(
      xlsx::saveWorkbook(wb, paste0("qtn1819_sec_selective_qtn1920_", question_name, "_S", school, ".xlsx"), password=NULL)
      )
  }
  xlsx::saveWorkbook(wb, paste0("qtn1819_sec_selective_qtn1920_", question_name, ".xlsx"), password=NULL)
}

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary/qtn1819_secondary_selective_qtn1920", setpath))

table_loop(df_se, df_mean_intT1, 'q3', "GHQ", school = 15)
table_loop(df_se, df_mean_intT1, 'q6', "empathy", school = 15)
table_loop(df_se, df_mean_intT1, 'q7', "gratitude", school = 15)

rm(df_mean_intT1, df_selective_mean)

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))

# data cleaning continues ----

# dfwide <- reshape(df,
#                   idvar = c("sch", "class", "student_num", "dob", "sex", 
#                             "control", "intervention", "form"), # this line is to keep variables
#                   timevar = "T1", 
#                   direction = "wide")

df_pre <- df[df$T1==0,]
df_pre$T1 <- NULL
df_post <- df[df$T1==1,]
df_post$T1 <- NULL
dfwide <- merge(df_pre, df_post, by.x = c("sch","class","student_num", 
                                        "dob", "sex",
                                         "control", "intervention", "form", "class"),
                     by.y = c("sch","class","student_num", 
                              "dob", "sex",
                              "control", "intervention", "form", "class"),
                     all.x=TRUE, all.y = TRUE, suffixes = c(".0",".1"))

dfwide_both <- merge(df_pre, df_post, by.x = c("sch","class","student_num", 
                                             "dob", "sex",
                                             "control", "intervention", "form", "class"),
                   by.y = c("sch","class","student_num", 
                            "dob", "sex",
                            "control", "intervention", "form", "class"),
                   all.x=FALSE, all.y = FALSE, suffixes = c(".0",".1"))

# # experimental code (which reduces the strength of the relationships in within-subject analysis using wide-format data)
# # Attempt to get more matches (by not using dob and sex in the process)
# 
# # Randomly select sex and dob from T0/T1 if they don't match
# dfwide$sexdiff <- ifelse(dfwide$sex.0!=dfwide$sex.1, 1, 0)
# dfwide$dobdiff <- ifelse(dfwide$dob.0!=dfwide$dob.1, 1, 0)
# 
# set.seed(96274) # a 5-digit random number generated from random.org with a range (1:100000), set as seed
# dfwide$rand_sex <- runif(nrow(dfwide), 0, 1)
# dfwide$rand_dob <- runif(nrow(dfwide), 0, 1)
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
# # xlsx::write.xlsx(dfwide_both, "dfwide_both_secondary.xlsx")

dfwide <- dfwide %>%
  # rowwise() %>% ## a slower method, pmax() is changed to max() with rowwise()
  mutate(age=pmax(age.0, age.1, na.rm=TRUE))

dfwide$q1 <- (dfwide$q1.1-dfwide$q1.0)
dfwide$q2 <- (dfwide$q2.1-dfwide$q2.0)
dfwide$q1_2 <- (dfwide$q1_2.1-dfwide$q1_2.0)
dfwide$q3 <- (dfwide$q3.1-dfwide$q3.0)
dfwide$q4neg <- (dfwide$q4neg.1-dfwide$q4neg.0)
dfwide$q4pos <- (dfwide$q4pos.1-dfwide$q4pos.0)
dfwide$q5a <- (dfwide$q5a.1-dfwide$q5a.0)
dfwide$q5b <- (dfwide$q5b.1-dfwide$q5b.0)
dfwide$q6 <- (dfwide$q6.1-dfwide$q6.0)
dfwide$q7 <- (dfwide$q7.1-dfwide$q7.0)

dfwide$T0 <- ifelse(!is.na(dfwide$age.0), 1, NA)
dfwide$T1 <- ifelse(!is.na(dfwide$age.1), 1, NA)

saveRDS(dfwide, file = "qtn1819_secondary_wide.rds")