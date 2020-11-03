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
# library(haven)
library(lubridate) # year()

# secondary schools universal 2018-19 data cleaning ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))

# this following excel file added dob from pre_post_simple_cleaned_king for T0 of CWSFMSS, DGS, LHKSS, MHLSS, LSC
dfpre <- openxlsx::read.xlsx("results-survey776844_pre_post_cleaned_king_20200924.xlsx",sheet = "pre")
dfpost <- openxlsx::read.xlsx("results-survey776844_pre_post_cleaned_king_20200924.xlsx",sheet = "post")
dfpost <- dfpost[, -which(names(dfpost)=="token")] # drop column

dfpost$submitdate <- as.Date(dfpost$submitdate,  format = "%d-%b-%Y") 
dfpost$startdate <- as.Date(dfpost$startdate,  format = "%d-%b-%Y") 
dfpost$datestamp <- as.Date(dfpost$datestamp,  format = "%d-%b-%Y") 
dfpost$Q03 <- as.Date(dfpost$Q03,  format = "%d-%b-%Y") 

dfpre$submitdate <- as.Date(dfpre$submitdate,  format = "%Y-%m-%d") 
dfpre$startdate <- as.Date(dfpre$startdate,  format = "%Y-%m-%d") 
dfpre$datestamp <- as.Date(dfpre$datestamp,  format = "%Y-%m-%d") 
dfpre$Q03 <- as.Date(dfpre$Q03,  format = "%Y-%m-%d") 

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- rbind(dfpre, dfpost)
rm(dfpre, dfpost)

names(df)[names(df)=="Q04"] <- "sex"

names(df)[names(df)=="Q05"] <- "sch"
df$sch <-  as.numeric(substr(df$sch, 2, 3)) 

names(df)[names(df)=="Q03"] <- "dob"
names(df)[names(df)=="Q01"] <- "form"
df$class <- ifelse(!is.na(df$Q01a), paste0(df$form, df$Q01a) , 
                   ifelse(!is.na(df$Q01b), paste0(df$form, df$Q01b), 
                          ifelse(!is.na(df$Q01c), paste0(df$form, df$Q01c), df$form)))


names(df)[names(df)=="Q02"] <- "student_num" 

df$age <- as.numeric(floor((as.Date("2019-06-30")-df$dob)/365.2425))

# total <- df[, 115:128] # extract outcome-level variables (total scores)
df <- df[, -c(115:128)] # remove previously calculated total scores

df$imputed_sch[is.na(df$sch)] <- 1 # not missing school
df$sch[is.na(df$sch) & df$ipaddr == "210.176.116.13"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "165.84.186.4"] <- 5
df$sch[is.na(df$sch) & df$ipaddr == "221.126.237.98"] <- 6
df$sch[is.na(df$sch) & df$ipaddr == "223.255.166.222"] <- 8
df$sch[is.na(df$sch) & df$ipaddr == "202.155.239.194"] <- 8
df$sch[is.na(df$sch) & df$ipaddr == "192.168.0.200"] <- 11
df$sch[is.na(df$sch) & df$ipaddr == "192.168.0.9"] <- 11
df$sch[is.na(df$sch) & df$ipaddr == "61.238.98.99"] <- 11
df$sch[is.na(df$sch) & df$ipaddr == "223.197.10.33"] <- 14
df$imputed_sch[is.na(df$imputed_sch)] <- 0 # imputed school based on IP address

df$intervention <- ifelse( (df$sch == 1 & df$form == 4) | # intervention group
                             (df$sch == 2 & df$form == 2) |
                             (df$sch == 3 & df$form == 4) |
                             (df$sch == 5 & df$form == 2) |
                             (df$sch == 6 & df$form == 1) |
                             (df$sch == 7 & df$form == 1) |
                             (df$sch == 8 & df$form == 2) |
                             (df$sch == 9 & df$form == 1) |
                             (df$sch == 10 & df$form == 1) |
                             (df$sch == 11 & df$form == 2) |
                             (df$sch == 12 & df$form == 1) |
                             (df$sch == 13 & df$form == 2) |
                             (df$sch == 14 & df$form == 1) , 1, 
                           ifelse( (df$sch == 1 & df$form == 1) | # control group
                                     (df$sch == 2 & df$form == 1) |
                                     (df$sch == 3 & df$form == 3) |
                                     (df$sch == 5 & df$form == 1) |
                                     (df$sch == 8 & df$form == 1) |
                                     (df$sch == 11 & df$form == 1) |
                                     (df$sch == 13 & df$form == 1), 0, NA))
df$control <- ifelse(df$intervention, 0, 1)

df$sch <- car::recode(df$sch, "
1 = 'YLCSS';
2 =  'LWLC';
3 = 'MHLSS';

5 =  'TKP';
6 = 'LSK';
7 = 'LSC';
8 = 'CWSFMSS';
9 = 'SPCS';
10 = 'LHKSS';
11 = 'MKES';
12 = 'KTLS';
13 = 'MC';
14 = 'DGS'
")

df %>% select(starts_with("P1_")) %>% colnames(.) -> P1
df %>% select(starts_with("P1t2_")) %>% colnames(.) -> P1e # elearning
df %>% select(starts_with("P2x")) %>% colnames(.) -> P2
df %>% select(starts_with("P2t2x")) %>% colnames(.) -> P2e # elearning
df %>% select(starts_with("P3_")) %>% colnames(.) -> P3
df %>% select(starts_with("P4_")) %>% colnames(.) -> P4
df %>% select(starts_with("P5xa_")) %>% colnames(.) -> P5a 
df %>% select(starts_with("P5xb_")) %>% colnames(.) -> P5b
df %>% select(starts_with("P6_")) %>% colnames(.) -> P6
df %>% select(starts_with("P7_")) %>% colnames(.) -> P7

reverse_P1 <- P1[c(2, 3, 7, 8, 9, 12)] 
reverse_P1e <- P1e[c(2, 3, 7, 8, 9)] 
df[reverse_P1] <- (df[reverse_P1]-1)*-1  # reverse (0,1) to (1,0)
df[reverse_P1e] <- (df[reverse_P1e]-1)*-1  # reverse (0,1) to (1,0)

df[P2[1]] <- ifelse(df[P2[1]] == 'D', 1, 0) # D is correct
df[P2[2]] <- ifelse(df[P2[2]] == 'C', 1, 0) 
df[P2[3]] <- ifelse(df[P2[3]] == 'A', 1, 0) 
df[P2[4]] <- ifelse(df[P2[4]] == 'D', 1, 0) 
df[P2[5]] <- ifelse(df[P2[5]] == 'D', 1, 0) 
df[P2[6]] <- ifelse(df[P2[6]] == 'C', 1, 0) 
df[P2[7]] <- ifelse(df[P2[7]] == 'D', 1, 0) 
df[P2[8]] <- ifelse(df[P2[8]] == 'D', 1, 0) 

df[P2e[1]] <- ifelse(df[P2e[1]] == 'D', 1, 0) 
df[P2e[2]] <- ifelse(df[P2e[2]] == 'C', 1, 0) 
df[P2e[3]] <- ifelse(df[P2e[3]] == 'A', 1, 0) 
df[P2e[4]] <- ifelse(df[P2e[4]] == 'D', 1, 0) 
df[P2e[5]] <- ifelse(df[P2e[5]] == 'D', 1, 0) 
df[P2e[6]] <- ifelse(df[P2e[6]] == 'C', 1, 0) 
df[P2e[7]] <- ifelse(df[P2e[7]] == 'C', 1, 0) 
df[P2e[8]] <- ifelse(df[P2e[8]] == 'B', 1, 0) 

# GHQ
df[P3] <- (df[P3]-1) # from (1:4) to (0:3)
reverse_P3 <- P3[c(1, 3, 4, 7, 8, 12)] 
df[reverse_P3] <- (df[reverse_P3]-3)*-1  # reverse (0:3) to (3:0)

# negative thinking
reverse_P4 <- P4[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] 

# SLSS
reverse_P5a <- P5a[c(3, 4)]
df[reverse_P5a] <- (df[reverse_P5a]-7)*-1 # reverse (1:6) to (6:1)

# Empathy
reverse_P6 <- P6[c(3)] 
df[reverse_P6] <- (df[reverse_P6]-4)*-1  # reverse (0:4) to (4:0)

# Gratitude
reverse_P7 <- P7[c(3, 6)] 
df[reverse_P7] <- (df[reverse_P7]-8)*-1  # reverse (1:7) to (7:1)

df$elearning <- ifelse(!is.na(df$P1_1), 0, 
                       ifelse(!is.na(df$P1t2_1), 1, NA))

df <- df %>% 
  mutate(q1 = ifelse(!elearning, rowSums(.[P1], na.rm = FALSE), rowSums(.[P1e], na.rm = FALSE)), # Q1&2 are Mental Health Knowledge, na.rm = FALSE means any NA in an item results in NA for the scale
         q2 = ifelse(!elearning, rowSums(.[P2], na.rm = FALSE), rowSums(.[P2e], na.rm = FALSE)), 
         q3 = rowSums(.[P3], na.rm = FALSE), # GHQ Psychological Distress range(0-36)
         q4neg = rowSums(.[reverse_P4], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q4pos = rowSums(.[P4[!(P4 %in% reverse_P4)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
         q5a = rowSums(.[P5a], na.rm = FALSE), # SLSS, range(7-42)
         q5b = rowSums(.[P5b], na.rm = FALSE), # BMSLSS, range(5-35)
         q6 = rowSums(.[P6], na.rm = FALSE), # Empathy range(0-24)
         q7 = rowSums(.[P7], na.rm = FALSE), # Gratitude range(6-42)
)

df$q1_2 <- df$q1 + df$q2

# secondary schools universal 2018-19 DMS data cleaning ----
# this section of code taken & slightly modified from qtn1819_data_cleaning_s.R
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))
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
dfDMS$sch <- 'DMS'

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
dfDMS$submitdate <- as.Date(dfDMS$submitdate)

dfDMS$age <- as.numeric(floor((as.Date("2019-06-30")-as.Date(dfDMS$dob))/365.2425))

dfDMS %>% select(starts_with("Ass1-P1")) %>% colnames(.) -> qA1 #q1
dfDMS %>% select(starts_with("Ass1-P2")) %>% colnames(.) -> qA2 #q2
dfDMS %>% select(starts_with("Ass1-P3")) %>% colnames(.) -> qB #q3
dfDMS %>% select(starts_with("Ass1-P4")) %>% colnames(.) -> qC #q4pos & q4neg
dfDMS %>% select(starts_with("Ass1-P5A")) %>% colnames(.) -> qD1 #q5a
dfDMS %>% select(starts_with("Ass1-P5B")) %>% colnames(.) -> qD2 #q5b
dfDMS %>% select(starts_with("Ass1-P6")) %>% colnames(.) -> qE #q6
dfDMS %>% select(starts_with("Ass1-P7")) %>% colnames(.) -> qF #q7

# Outcome Scoring
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

# merge DMS into main data ----
names(dfDMS)[names(dfDMS) %in% c(qA1, qA2, qB, qC, qD1, qD2, qE, qF)] <-
  names(df)[names(df) %in% c(P1, P2, P3, P4, P5a, P5b, P6, P7)]

var_names <-   names(df)[names(df) %in% c(P1, P2, P3, P4, P5a, P5b, P6, P7)] # save variable names for 2017-18 data


dfDMS <- subset(dfDMS, select = c("sch", "form", "class", "student_num", "age", "sex", 
                                  P1, P2, P3, P4, P5a, P5b, P6, P7,
                                  "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                                  "control", "submitdate", "dob", "T1", "intervention"))

df <- subset(df, select = c("sch", "form", "class", "student_num", "age", "sex", 
                                  P1, P2, P3, P4, P5a, P5b, P6, P7,
                                  "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                                  "control", "submitdate", "dob", "T1", "intervention"))

df <- rbind(df, dfDMS)
rm(dfDMS)

df1819 <- df
rm(df)
# use the previously cleaned dataset and append it with correctly calculated q6 from new dataset----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))
df <- readRDS("qtn1819_secondary_long.rds") # previously cleaned dataset with higher number of matched students, but q6 was calculated wrong and no item-level information

df$sch <- car::recode(df$sch, "
1 = 'YLCSS';
2 =  'LWLC';
3 = 'MHLSS';

5 =  'TKP';
6 = 'LSK';
7 = 'LSC';
8 = 'CWSFMSS';
9 = 'SPCS';
10 = 'LHKSS';
11 = 'MKES';
12 = 'KTLS';
13 = 'MC';
14 = 'DGS';
15 = 'DMS'
")

df1819 <- subset(df1819, select = c("sch", "form", "class", "student_num", "age", "sex", 
                            "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                            "control", "submitdate", "dob", "T1", "intervention"))

# extract q6 by matching scores to all other outcomes
temp <- merge(df, unique(df1819)[, c("sch", "class", "student_num", "intervention", "q1", "q2", "q1_2", "q3", "q4neg", "q4pos", "q5a", "q5b", "q6", "q7")], 
              by=c("sch", "class", "intervention",  "q1", "q2", "q1_2",  "q3", "q4pos", "q4neg", "q5a", "q5b","q7"), all.x = TRUE)

temp$q6 <- temp$q6.y
temp$student_num <- temp$student_num.x
temp <- subset(temp, select = c("sch", "form", "class", "student_num", "age", "sex", 
                                    "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                                    "control", "submitdate", "dob", "T1", "intervention", "q6.x", "q6.y"))

select_var <- c("q6")

# extract rows from new dataset for nonempty q6 from new dataset & empty q6 from old dataset
for (index in which(!is.na(temp$q6.x) & is.na(temp$q6.y))){
  T1 <- temp$T1[index]
  sch <- temp$sch[index]
  dob <- as.Date(temp$dob[index])
  class <- temp$class[index]
  student_num <- temp$student_num[index]
  # print(paste(sch, dob, class, student_num))
  # print(length(which(df1819$sch %in% sch # check uniqueness
  #                    & df1819$T1 %in% T1
  #                    & df1819$class %in% class
  #                    & df1819$dob %in% dob 
  #                    & df1819$student_num %in% student_num)))
  if (length(which(df1819$sch %in% sch
                   & df1819$T1 %in% T1
                   & df1819$class %in% class
                   & df1819$dob %in% dob 
                   & df1819$student_num %in% student_num)) == 1){
    temp[index, select_var] <- trycatchNA(df1819[which(df1819$sch %in% sch
                                             & df1819$T1 %in% T1
                                             & df1819$class %in% class
                                             & df1819$dob %in% dob 
                                             & df1819$student_num %in% student_num), select_var])
  } 
}


rm(df1819)

temp <- temp[, -which(names(temp) %in% c("q6.x", "q6.y"))]
df <- subset(temp, select = c("sch", "form", "class", "student_num", "age", "sex", 
                                "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                                "control", "submitdate", "dob", "T1", "intervention"))
rm(temp)

# recalculate age due to old dataset using submitdate instead
df$age <- as.numeric(floor((as.Date("2019-06-30")-as.Date(df$dob))/365.2425))

df <- df[order(df$submitdate),]
df <- distinct(df, sch, class, student_num, dob, T1, sex, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df1819 <- df
rm(df)
# secondary schools universal 2017-18 data cleaning ----
setwd(sprintf("~%s/qtn/qtn2017-18/qtn1718_secondary", setpath))

dfpre <- openxlsx::read.xlsx("qtn1718_pre_post_v4+.xlsx", sheet = "pre")
dfpost <- openxlsx::read.xlsx("qtn1718_pre_post_v4+.xlsx", sheet = "post")
dfpost$submitDate <- as.Date(as.numeric(dfpost$submitDate), origin= "1899-12-30")
dfpre$submitDate <- as.Date(dfpre$submitDate)
dfpre$T1 <- 0
dfpost$T1 <- 1
df <- rbind(dfpre, dfpost)
rm(dfpre, dfpost)

df <- df[,-c(18:95)]
df[df==97] <- NA
df[df==98] <- NA
df[df==99] <- NA
df[df==999] <- NA

names(df)[names(df)=="gender"] <- "sex"
df$sex <- (df$sex-3)*-1 # changed to 1 = female & 2 = male

names(df)[names(df)=="dob"] <- "dob_old"
df$dob <-  as.Date(df$dob_r, origin = "1970-01-01")
df$dob[year(df$dob) <= 1970] <- NA
df$dob[grepl("/", df$dob_old) & is.na(df$dob)] <- as.Date(df$dob_old[grepl("/", df$dob_old) & is.na(df$dob)], format = "%d/%m/%Y")
df$dob[!grepl("-", df$dob_old) & is.na(df$dob)] <- as.Date(as.numeric(df$dob_old[!grepl("-", df$dob_old) & is.na(df$dob)]), origin = "1899-12-30")
df$dob[year(df$dob) >= 2013] <- NA
names(df)[names(df)=="age"] <- "age_old"

df$age <- as.numeric(floor((as.Date("2018-06-30")-df$dob)/365.2425))

# control and intervention groups
names(df)[names(df)=="grade"] <- "form"
names(df)[names(df)=="studentId"] <- "student_num"
df$class <- ifelse(!is.na(df$class1), paste0(df$form, df$class1) , 
                   ifelse(!is.na(df$class2), paste0(df$form, df$class2), 
                                               ifelse(!is.na(df$class3), paste0(df$form, df$class1), df$form)))
names(df)[names(df)=="submitDate"] <- "submitdate"
names(df)[names(df)=="school"] <- "sch"
df$sch[df$sch %in% "HLC"] <- "S13"
df$sch[df$sch %in% "CWSFMSS"] <- "S14"
df$sch[df$sch %in% "CWKMSS"] <- "S15"
df$sch <-  as.numeric(substr(df$sch, 2, 3)) 

# incorrect form
df$form[df$sch == 2 & df$form == 3]  <- 2
df$form[df$sch == 3 & df$form == 2 & df$studentId == 19]  <- 3
df$form[df$sch == 3 & df$form == 2 & df$studentId == 19]  <- 3

df$intervention <- ifelse( (df$sch == 1 & df$form == 4) | # intervention group
                             (df$sch == 2 & df$form == 2) |
                             (df$sch == 3 & df$form == 4) |
                             (df$sch == 4 & df$form == 1) |
                             (df$sch == 5 & df$form == 2) |
                             (df$sch == 6 & df$form == 1) |
                             (df$sch == 7 & df$form == 1) |
                             (df$sch == 7 & df$form == 4) | # assumed to be intervention
                             (df$sch == 8 & df$form == 1) |
                             (df$sch == 9 & df$form == 1) |
                             (df$sch == 10 & df$form == 1) |
                             (df$sch == 11 & df$form == 2) |
                             (df$sch == 12 & df$form == 1) |
                             (df$sch == 13 & df$form == 4) |
                             (df$sch == 14 & df$form == 2) |
                             (df$sch == 15 & df$form == 3) , 1, 
                           ifelse( (df$sch == 2 & df$form == 1) | # control group
                                     (df$sch == 3 & df$form == 3) |
                                     (df$sch == 5 & df$form == 1) |
                                     (df$sch == 7 & df$form == 2) |
                                     (df$sch == 10 & df$form == 2) |
                                     (df$sch == 11 & df$form == 1) |
                                     (df$sch == 12 & df$form == 2) |
                                     (df$sch == 13 & df$form == 3) |
                                     (df$sch == 14 & df$form == 1) |
                                     (df$sch == 15 & df$form == 2), 0, NA))

df$control <- ifelse(df$intervention, 0, 1)

df$sch <- car::recode(df$sch, "
1 = 'YLCSS';
2 =  'LWLC';
3 = 'MHLSS';
4 = 'TNSS';
5 =  'TKP';
6 = 'LSK';
7 = 'LSC';
8 = 'BPS';
9 = 'SPCS';
10 = 'LHKSS';
11 = 'MKES';
12 = 'KTLS';
13 = 'HLC';
14 = 'CWSFMSS';
15 = 'CWKMSS'
")

df %>% select(starts_with("Q1")) %>% select(ends_with("_r_s")) %>% colnames(.) -> P1 
df %>% select(starts_with("Q2")) %>% select(ends_with("_r_s")) %>% colnames(.) -> P2
df %>% select(starts_with("Q3")) %>% select(ends_with("_r")) %>% colnames(.) -> P3
df %>% select(starts_with("Q4")) %>% select(ends_with("_r")) %>% colnames(.) -> P4
df %>% select(starts_with("Q5.")) %>% select(ends_with("_r")) %>% colnames(.) -> P5
df %>% select(starts_with("Q6")) %>% select(ends_with("_r")) %>% colnames(.) -> P6
df %>% select(starts_with("Q7")) %>% select(ends_with("_r")) %>% colnames(.) -> P7

P5a <- P5[1:7]
P5b <- P5[8:12]

# GHQ
reverse_P3 <- P3[c(1, 3, 4, 7, 8, 12)] 
df[reverse_P3] <- (df[reverse_P3]-3)*-1  # reverse (0:3) to (3:0)

# negative thinking
reverse_P4 <- P4[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] 

# SLSS
reverse_P5a <- P5a[c(3, 4)]
df[reverse_P5a] <- (df[reverse_P5a]-7)*-1 # reverse (1:6) to (6:1)

# Empathy
reverse_P6 <- P6[c(3)] 
df[reverse_P6] <- (df[reverse_P6]-4)*-1  # reverse (0:4) to (4:0)

# Gratitude
reverse_P7 <- P7[c(3, 6)] 
df[reverse_P7] <- (df[reverse_P7]-8)*-1  # reverse (1:7) to (7:1)

df <- df %>% 
  mutate(q1 = rowSums(.[P1], na.rm = FALSE), # Q1&2 are Mental Health Knowledge, na.rm = FALSE means any NA in an item results in NA for the scale
         q2 = rowSums(.[P2], na.rm = FALSE), 
         q3 = rowSums(.[P3], na.rm = FALSE), # GHQ Psychological Distress range(0-36)
         q4neg = rowSums(.[reverse_P4], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q4pos = rowSums(.[P4[!(P4 %in% reverse_P4)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
         q5a = rowSums(.[P5a], na.rm = FALSE), # SLSS, range(7-42)
         q5b = rowSums(.[P5b], na.rm = FALSE), # BMSLSS, range(5-35)
         q6 = rowSums(.[P6], na.rm = FALSE), # Empathy range(0-24)
         q7 = rowSums(.[P7], na.rm = FALSE), # Gratitude range(6-42)
  )

df$q1_2 <- df$q1 + df$q2

df <- subset(df, select = c("sch", "form", "class", "student_num", "age", "sex", 
                            P1, P2, P3, P4, P5a, P5b, P6, P7,
                            "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                            "control", "submitdate", "dob", "T1", "intervention"))

df <- df[order(df$submitdate),]
df <- distinct(df, sch, class, student_num, dob, T1, sex, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df1718 <- df
rm(df)

# secondary schools universal 2015-16 & 2016-17 ----
setwd(sprintf("~%s/qtn/qtn2016-17/qtn1617_secondary", setpath))
df <- read.csv("secsch_forSAS_1516_1617_1718_dob.csv")
df <- df[which(df$schyr<=2),] # only 1516 & 1617 data
df <- df[-which(df$uniq_id %in% "1MKES2C13" & is.na(df$t1_grat)),] # remove duplicate

df <- reshape(df,
              idvar = c("uniq_id"),
              varying = list(c("t0_grat", "t1_grat"), c("t0_pt", "t1_pt")),
              v.names = c("q7","q6"),
              timevar = "time",
              direction = "long")

df$T1 <- df$time-1

df$sex <- ifelse(df$gender_1m2f == 1, 2, 1)
df$form <- as.numeric(substr(df$class, 1, 1))
names(df)[names(df)=="classno"] <- "student_num"
df$intervention <- ifelse(df$grp_1intv2ctrl == 1, 1, 0)
df$control <- ifelse(df$intervention, 0, 1)

df$dob <- as.Date(paste(df$Birth_day,
                               df$Birth_month,
                               df$Birth_year, sep = "/"), format = "%d/%m/%Y")

df$dob[is.na(df$Birth_day) | is.na(df$Birth_month) | is.na(df$Birth_year)] <- NA

df$age <- ifelse(df$schyr==1, as.numeric(floor((as.Date("2016-06-30")-df$dob)/365.2425)),
                                         ifelse(df$schyr==2, as.numeric(floor((as.Date("2017-06-30")-df$dob)/365.2425)), NA)
                  )

df$sch <- car::recode(df$schname, "
'A1' = 'TKP';
'A2' = 'TNSS';
'A3' = 'TLLF';
'A4' = 'WKF';
'A5' = 'LSK';
'A6' = 'LWLC';
'A7' = 'MHLSS';
'A8' = 'YLCSS';
'A9' = 'MKES';
'A10' = 'HLC';
'A11' = 'KTLS';
'HLC' = 'HLC';
'MKES' = 'MKES';
'YKH' = 'YKH'
")

names(df)[names(df)=="schyr"] <- "year"

df <- subset(df, select = c("year",  "sch", "form", "class", "student_num", "age", "sex",
                            "q6", "q7",
                            "control", "dob", "T1", "intervention"))

df1516 <- df[which(df$year == 1), ]
df1617 <- df[which(df$year == 2), ]
rm(df)

# merge 2015-16 & 2016-17 & 2017-18 & 2018-19 (secondary schools universal) ----
names(df1718)[names(df1718) %in% c(P1, P2, P3, P4, P5a, P5b, P6, P7)] <- var_names # replace with same variable names as other years

df1516$year <- 1
df1617$year <- 2
df1718$year <- 3
df1819$year <- 4

df <- plyr::rbind.fill(df1516, df1617, df1718, df1819)
rm(df1516, df1617, df1718, df1819)

df <- subset(df, select = c("year", "sch", "form", "class", "student_num", "age", "sex", 
                            "q1", "q2", "q1_2", "q3", "q4pos", "q4neg", "q5a", "q5b", "q6", "q7", 
                            "control", "submitdate", "dob", "T1", "intervention"))

# averaging the scores
# df$q3 <- df$q3/12 # GHQ Psychological Distress
# df$q4neg <- df$q4neg/10 # negtaive thinking 
# df$q4pos <- df$q4pos/10 # positive thinking
# df$q5a <- df$q5a/7 # SLSS
df$q5b <- df$q5b/5 # BMSLSS
# df$q6 <- df$q6/6 # Empathy
df$q7 <- df$q7/6 # Gratitude 

df$gender <- car::recode(df$sex,  "
1 = 'female';
2 = 'male'
")

setwd(sprintf("~%s/qtn/", setpath))
saveRDS(df, file = "qtn1519_secondary_long.rds")
# write_excel("qtn1519_secondary_long.xlsx", df)

# primary schools universal 2015-16 & 2016-17 ----
setwd(sprintf("~%s/qtn/qtn2016-17/qtn1617_primary", setpath))
df <- read.csv("forSAS_1516_1617.csv")

df <- reshape(df,
              idvar = c("uniq_id"),
              varying = list(c("t0_p1", "t1_p1"), c("t0_p2", "t1_p2"), 
                             c("t0_p3n", "t1_p3n"), c("t0_p3p", "t1_p3p"), 
                             c("t0_p4", "t1_p4"), c("t0_p5", "t1_p5")),
              v.names = c("p1","p2","p3n", "p3p", "p4", "p5"),
              timevar = "time",
              direction = "long")

df$T1 <- df$time-1


