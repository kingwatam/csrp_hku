rm(list=ls())
graphics.off()
source("helper_functions.R")
setpath <- "/MEGAsync/Work/RA HKU/CSRP"

library(dplyr)

# P4 (level 1) ----
setwd(sprintf("~%s/qtn/qtn2019-20/primary", setpath))
dfpre <- haven::read_sav("P4-pre-survey_450000_SPSS.sav")
dfpost <- haven::read_sav("P4-post-survey_450004_SPSS.sav")

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)

names(df)[names(df)=="Q01"] <- "class" 
df$grade <- as.integer(as.vector(substr(df$class,1,1)))
names(df)[names(df)=="Q02"] <- "student_num"
names(df)[names(df)=="Q03"] <- "dob"
df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

names(df)[names(df)=="Q04"] <- "sex" # female = 1; male = 2

names(df)[names(df)=="Q00"] <- "sch" 
df$sch[df$sch %in% ""] <- NA
df$sch <- as.integer(as.vector(substr(df$sch,2,3)))
df$imputed_sch[is.na(df$sch)] <- 1 # not missing school
df$sch[is.na(df$sch) & df$ipaddr == "210.87.255.239"] <- 1
df$sch[is.na(df$sch) & df$ipaddr == "210.3.171.211"] <- 2
df$sch[is.na(df$sch) & df$ipaddr == "202.126.220.202"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "210.176.51.222"] <- 5
df$sch[is.na(df$sch) & df$ipaddr == "220.241.161.78"] <- 7
df$sch[is.na(df$sch) & df$ipaddr == "123.202.38.7"] <- 9
df$sch[is.na(df$sch) & df$ipaddr == "210.3.199.114"] <- 10
df$sch[is.na(df$sch) & df$ipaddr == "210.3.102.78"] <- 11 
df$sch[is.na(df$sch) & df$ipaddr == "210.3.74.230"] <- 12
df$imputed_sch[is.na(df$imputed_sch)] <- 0 # imputed school based on IP address

df$intervention <- 1
df$control <- 0

df$sch <- car::recode(df$sch, "
1 = 'KMS';
2 =  'YKH';
3 = 'LBC';
4 = 'TLP';
5 =  'CKL';
6 = 'PTS';
7 = 'LPM';
8 = 'YCMC';
9 = 'SHC';
10 = 'SAP';
11 = 'WSS';
12 = 'MOSLL';
13 = 'CYS';
14 = 'HEP';
15 = 'TPM'
")

df$level <- 1

scoring_level1 <- function(df){
  df %>% select(starts_with("A01")) %>% colnames(.) -> P1a 
  df %>% select(starts_with("AC0")) %>% colnames(.) -> P1b
  df %>% select(starts_with("AB01")) %>% colnames(.) -> P2
  df %>% select(starts_with("B01")) %>% colnames(.) -> P3
  df %>% select(starts_with("C01")) %>% colnames(.) -> P4
  df %>% select(starts_with("D01")) %>% colnames(.) -> P5
  df %>% select(starts_with("E01")) %>% colnames(.) -> P6
  df %>% select(starts_with("F01")) %>% colnames(.) -> P7
  df %>% select(starts_with("G01")) %>% colnames(.) -> P9a
  df %>% select(starts_with("G02")) %>% colnames(.) -> P9b
  df %>% select(starts_with("G0") & ends_with("SQ001")) %>% colnames(.) -> P10a_b
  df %>% select(starts_with("G05")) %>% colnames(.) -> P10c
  
  reverse_P1a <- P1a[c(1,3,4,5,6,7,9,12,13,14,15)]
  df[reverse_P1a] <- (df[reverse_P1a]-1)*-1 # reverse (1:0) to (0:1)
  
  df[P1b[1]] <- ifelse(df[P1b[1]] == 1, 1, 0) # A is correct
  df[P1b[2]] <- ifelse(df[P1b[2]] == 2, 1, 0) # B is correct
  df[P1b[3]] <- ifelse(df[P1b[3]] == 2, 1, 0) 
  df[P1b[4]] <- ifelse(df[P1b[4]] == 4, 1, 0) 
  df[P1b[5]] <- ifelse(df[P1b[5]] == 3, 1, 0) 
  
  reverse_P4 <- P4[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking
  
  reverse_P5 <- P5[c(3)]
  df[reverse_P5] <- (df[reverse_P5]-4)*-1 # reverse (0:4) to (4:0)
  
  reverse_P6 <- P6[c(2, 5, 6, 8, 9)]
  df[reverse_P6] <- (df[reverse_P6]-5)*-1 # reverse (1:4) to (4:1)
  
  reverse_P7 <- P7[c(3, 6)]
  df[reverse_P7] <- (df[reverse_P7]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_P10c <- P10c[c(2, 4)]
  df[reverse_P10c] <- (df[reverse_P10c]-3)*-1 # reverse (1:2) to (2:1)
  
  P9b1 <- P9b[1:2]
  P9b2 <- P9b[3]
  P9b3 <- P9b[4:5]
  
  P10a <- P10a_b[1]
  P10b <- P10a_b[2]
  
  P10c1 <- P10c[1:2]
  P10c2 <- P10c[3]
  P10c3 <- P10c[4]
  
  df <- df %>% 
    mutate(q1a = rowSums(.[P1a], na.rm = FALSE), # True/False questions
           q1b = rowSums(.[P1b], na.rm = FALSE), # MCQ questions
           q2 = rowSums(.[P2], na.rm = FALSE), # Subjective Happiness (SHS) range(1-7)
           q3 = rowSums(.[P3], na.rm = FALSE), # Anxiety (SCARED) range(0-18)
           q4neg = rowSums(.[reverse_P4], na.rm = FALSE), # Negative Thinking (CATS-N/P) range(0-40)
           q4pos = rowSums(.[P4[!(P4 %in% reverse_P4)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
           q5 = rowSums(.[P5], na.rm = FALSE), # C-IRI perspective taking range(0-24)
           q6 = rowSums(.[P6], na.rm = FALSE), # Self-esteem (RSES) range(10-40)
           q7 = rowSums(.[P7], na.rm = FALSE), # Gratitude GQ-6 range(6-42)
           q9a = rowMeans(.[P9a], na.rm = FALSE), # Compassion - common humanity subscale range(0-4), mean instead of sum
           q9b = rowMeans(.[P9b], na.rm = FALSE), # self-compassion range(0-4), mean instead of sum
           # q9b1 = rowMeans(.[P9b1], na.rm = FALSE), # self-compassion - self-kindness subscale range(0-4), mean instead of sum
           # q9b2 = .[[P9b2]], # self-compassion - self-judgement subscale range(0-4), mean instead of sum
           # q9b3 = rowMeans(.[P9b3], na.rm = FALSE), # self-compassion - common humanity subscale range(0-4), mean instead of sum
           q10a_b = rowSums(.[P10a_b], na.rm = FALSE), # help-seeking, range(2-5)
           # q10c1 =  rowMeans(.[P10c1], na.rm = FALSE), # prejudice - fear/avoidance subscale range(0-2), mean instead of sum
           # q10c2 =  .[[P10c2]], # prejudice - unpredictability subscale range(0-2), mean instead of sum
           # q10c3 =  .[[P10c3]], # understanding subscale range(0-1), mean instead of sum
           q10c = rowMeans(.[P10c], na.rm = FALSE) # partial PPMI range(1-2), mean instead of sum
    )
  
  df$q1 <- df$q1a + df$q1b # Mental Health Knowledge range (0-21)
  
  # df$q10a <- as.factor(df[[P10a]])
  # levels(df$q10a) <- c("Yes","No","Not sure")
  # 
  # df$q10b <- as.factor(df[[P10b]])
  # levels(df$q10b) <- c("Yes","No")
  # 
  # df$q10c1_2 <- (df$q10c1*2 + df$q10c2)/3 # weighted average because q8da has two questions
  # 
  # df$q10c3 <- as.factor(df$q10c3)
  # levels(df$q10c3) <- c("Incorrect","Correct")
  
  return(subset(df, select = c(q1a, q1b, q1, q2, q3, q4neg, q4pos, q5, q6, q7, q9a, q9b, q10a_b, q10c
  )))
}

df <- cbind(df, scoring_level1(df))

df <- subset(df, select = c(sch, imputed_sch, grade, class, student_num, age, sex, 
                            q1a, q1b, q1, q2, q3, q4neg, q4pos, q5, q6, q7, q9a, q9b, q10a_b, q10c,
                            control, submitdate, dob, T1, intervention, level))

dflevel1 <- df
rm(df)

# # selective program for 2020-2021 (S2) ----
# df_se <- df %>% # selective
#   filter(!is.na(sch)) %>%
#   filter(T1 == 0 & sch == 2 & grade == 4) 
# 
# df_mean_T0 <- df %>%
#   filter(!is.na(sch)) %>%
#   filter(T1 == 0 & sch == 2) %>%
#   group_by(sch) %>%
#   summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)
# 
# # df_se <- distinct(df_se, sch, class, student_num, dob, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)
# 
# df_se$sex <- ifelse(df_se$sex==2, "M", "F")
# df_se <- df_se %>% arrange(df_se$sch, df_se$class, df_se$student_num)
# 
# table_loop <- function(df_se, df_mean){
#   wb <- xlsx::createWorkbook(type="xlsx")
#   i <- 2
#   for (i in (1:17)){
#     if (i %in% df_se$sch){
#       df_selective <- df_se %>% # selective
#         filter(sch ==  i) %>%
#         mutate(q6_rank = order(order(q6))) # rank q6 values by school from lowest to highest
#       
#       if (i==11){
#         selective_n <- 48 # 25 more students  are wanted on top of the original 23
#       } else {
#         selective_n <- 20 # select at least 20 students
#       }
#       df_selective <- df_selective %>% filter((q6 <= max(.$q6[.$q6_rank<=selective_n])))
#       
#       df_selective_mean <- df_selective %>%
#         filter(!is.na(sch)) %>%
#         filter(T1 == 0) %>%
#         group_by(sch) %>%
#         summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)
#       
#       table <- data.frame(matrix(ncol = 13,  nrow = 0))
#       table <- subset(df_selective[df_selective$sch==i,], select = c("class",
#                                                                      "student_num",
#                                                                      "sex",
#                                                                      "dob",
#                                                                      "q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7", "imputed_sch"))
#       colnames(table)  <-  c("Class",
#                              "Student Number",
#                              "Sex",
#                              "Date of Birth",
#                              "Mental Health Knowledge",
#                              "Subjective Happiness"	,
#                              "Anxiety",
#                              "Negative Thinking",
#                              "Positive Thinking",
#                              "Perspective Taking (Empathy)",
#                              "Self-esteem",
#                              "Gratitude",
#                              "Matched from IP address")
#       
#       table$`Date of Birth` <- ifelse(is.na(table$`Date of Birth`), NA,
#                                       as.character.Date(table$`Date of Birth`, tryFormats = "%Y/%m/%d"))
#       table[nrow(table)+1,4] <- "Mean of Group with Lowest Self-esteem"
#       table[nrow(table)+1,4] <- "School Mean (Pre-test)"
#       table[nrow(table)-1,5:(length(table)-1)] <- sapply(round(df_selective_mean[df_selective_mean$sch==i,2:length(df_selective_mean)], 2),function(x)format(x,nsmall = 2))
#       table[nrow(table),5:(length(table)-1)] <- sapply(round(df_mean[df_mean$sch==i,2:length(df_mean)], 2),function(x)format(x,nsmall = 2))
#       
#       sheet <- xlsx::createSheet(wb, sheetName = paste0("S", deparse(as.numeric(i))))
#       assign(paste0("sheet", deparse(as.numeric(i))), sheet)
#       xlsx::addDataFrame(as.data.frame(table), get_("sheet", as.numeric(i)),
#                          startRow=1, startColumn=1,
#                          row.names = FALSE, showNA = FALSE)
#     }
#   }
#   xlsx::saveWorkbook(wb, "qtn1920_pri_selective_qtn2021.xlsx", password=NULL)
# }
# 
# setwd(sprintf("~%s/qtn/qtn2019-20/primary/qtn1920_primary_selective_qtn2021", setpath))
# 
# table_loop(df_se, df_mean_T0)

# P5 (level 2) ----
setwd(sprintf("~%s/qtn/qtn2019-20/primary", setpath))
dfpre <- haven::read_sav("P5-pre-survey_450002_SPSS.sav")
dfpost <- haven::read_sav("P5-post-survey_450005_SPSS.sav")

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)

names(df)[names(df)=="Q01"] <- "class" 
df$grade <- as.integer(as.vector(substr(df$class,1,1)))
names(df)[names(df)=="Q02"] <- "student_num"
names(df)[names(df)=="Q03"] <- "dob"
df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

names(df)[names(df)=="Q04"] <- "sex" # female = 1; male = 2

names(df)[names(df)=="Q00"] <- "sch" 
df$sch[df$sch %in% ""] <- NA
df$sch <- as.integer(as.vector(substr(df$sch,2,3)))
df$imputed_sch[is.na(df$sch)] <- 1 # not missing school
df$sch[is.na(df$sch) & df$ipaddr == "210.3.171.211"] <- 2
df$sch[is.na(df$sch) & df$ipaddr == "202.126.220.202"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "210.3.74.230"] <- 12
df$imputed_sch[is.na(df$imputed_sch)] <- 0 # imputed school based on IP address

df$intervention <- 1
df$control <- 0

df$sch <- car::recode(df$sch, "
1 = 'KMS';
2 =  'YKH';
3 = 'LBC';
4 = 'TLP';
5 =  'CKL';
6 = 'PTS';
7 = 'LPM';
8 = 'YCMC';
9 = 'SHC';
10 = 'SAP';
11 = 'WSS';
12 = 'MOSLL';
13 = 'CYS';
14 = 'HEP';
15 = 'TPM'
")

df$level <- 2

scoring_level2 <- function(df){
  df %>% select(starts_with("Q01x01_")) %>% colnames(.) -> P1a 
  df %>% select(starts_with("Q01x") & ends_with(sprintf("0%s", 2:3))) %>% colnames(.)  -> P1b
  df %>% select(starts_with("Q02x0")) %>% colnames(.) -> P2 # subjective happiness
  df %>% select(starts_with("Q03x0")) %>% colnames(.) -> P3 # PANAS
  df %>% select(starts_with("Q04x0")) %>% colnames(.) -> P4 # SKUS Strength knowledge & use
  df %>% select(starts_with("Q05x0")) %>% colnames(.) -> P5 # CATS-P/N
  df %>% select(starts_with("Q06x01")) %>% colnames(.) -> P6 # C-IRI
  df %>% select(starts_with("Q07x0")) %>% colnames(.) -> P7 # RSES
  df %>% select(starts_with("Q08x0")) %>% colnames(.) -> P8 # GQ-6
  df %>% select(starts_with("Q09x01")) %>% colnames(.) -> P9a 
  df %>% select(starts_with("Q09x02")) %>% colnames(.) -> P9b
  df %>% select(starts_with("Q09x03")) %>% colnames(.) -> P10a
  df %>% select(starts_with("Q09x04")) %>% colnames(.) -> P10b
  df %>% select(starts_with("Q09x05")) %>% colnames(.) -> P10c
  
  P10a_b <- c(P10a, P10b)
  
  reverse_P1a <- P1a[c(2,5,6,8,9,11)]
  df[reverse_P1a] <- (df[reverse_P1a]-1)*-1 # reverse (1:0) to (0:1)
  
  df[P1b[1]] <- ifelse(df[P1b[1]] == 2, 1, 0) # B is correct
  df[P1b[2]] <- ifelse(df[P1b[2]] == 3, 1, 0) # c is correct
  
  reverse_P2 <- P2[c(4)] 
  df[reverse_P2] <- (df[reverse_P2]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_P3 <- P3[c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)] # Negative Affect questions
  
  P4a <- P4[1:8]
  P4b <- P4[9:22]
  
  reverse_P4a <- P4a[c(2)]
  df[reverse_P4a] <- (df[reverse_P4a]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_P5 <- P5[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking
  
  reverse_P6 <- P6[c(3)]
  df[reverse_P6] <- (df[reverse_P6]-4)*-1 # reverse (0:4) to (4:0)
  
  reverse_P7 <- P7[c(2, 5, 6, 8, 9)]
  df[reverse_P7] <- (df[reverse_P7]-5)*-1 # reverse (1:4) to (4:1)
  
  reverse_P8 <- P8[c(3, 6)]
  df[reverse_P8] <- (df[reverse_P8]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_P10c <- P10c[c(2, 4)]
  df[reverse_P10c] <- (df[reverse_P10c]-3)*-1 # reverse (1:2) to (2:1)
  
  P9b1 <- P9b[1:2]
  P9b2 <- P9b[3]
  P9b3 <- P9b[4:5]
  
  P10c1 <- P10c[1:2]
  P10c2 <- P10c[3]
  P10c3 <- P10c[4]
  
  df <- df %>% 
    mutate(q1a = rowSums(.[P1a], na.rm = FALSE),
           q1b = rowSums(.[P1b], na.rm = FALSE), 
           q2 = rowSums(.[P2], na.rm = FALSE), # Subjective Happiness (SHS) range(4-28)
           q3neg = rowSums(.[reverse_P3], na.rm = FALSE),# Negative Affect (PANAS) range(10-50)
           q3pos = rowSums(.[P3[!(P3 %in% reverse_P3)]], na.rm = FALSE), # Positive Affect (PANAS) range(10-50)
           q4a = rowSums(.[P4a], na.rm = FALSE), # Strengths Knowledge Scale (SKS) range(8-56)
           q4b = rowSums(.[P4b], na.rm = FALSE), # Strengths Use Scale (SUS) range(14-98)
           q5neg = rowSums(.[reverse_P5], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
           q5pos = rowSums(.[P5[!(P5 %in% reverse_P5)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
           q6 = rowSums(.[P6], na.rm = FALSE), # C-IRI perspective taking range(0-24)
           q7 = rowSums(.[P7], na.rm = FALSE), # Self-esteem (RSES) range(10-40)
           q8 = rowSums(.[P8], na.rm = FALSE), # Gratitude GQ-6 range(6-42)
           q9a = rowMeans(.[P9a], na.rm = FALSE), # Compassion - common humanity subscale range(0-4), mean instead of sum
           q9b = rowMeans(.[P9b], na.rm = FALSE), # self-compassion range(0-4), mean instead of sum
           # q9b1 = rowMeans(.[P9b1], na.rm = FALSE), # self-compassion - self-kindness subscale range(0-4), mean instead of sum
           # q9b2 = .[[P9b2]], # self-compassion - self-judgement subscale range(0-4), mean instead of sum
           # q9b3 = rowMeans(.[P9b3], na.rm = FALSE), # self-compassion - common humanity subscale range(0-4), mean instead of sum
           q10a_b = rowSums(.[P10a_b], na.rm = FALSE), # help-seeking, range(2-5)
           # q10c1 =  rowMeans(.[P10c1], na.rm = FALSE), # prejudice - fear/avoidance subscale range(0-2), mean instead of sum
           # q10c2 =  .[[P10c2]], # prejudice - unpredictability subscale range(0-2), mean instead of sum
           # q10c3 =  .[[P10c3]], # understanding subscale range(0-1), mean instead of sum
           q10c = rowMeans(.[P10c], na.rm = FALSE) # partial PPMI range(1-2), mean instead of sum
    )
  
  df$q1 <- df$q1a + df$q1b # Mental Health Knowledge range(0-15)
  
  # df$q10a <- as.factor(df[[P10a]])
  # levels(df$q10a) <- c("Yes","No","Not sure")
  # 
  # df$q10b <- as.factor(df[[P10b]])
  # levels(df$q10b) <- c("Yes","No")
  # 
  # df$q10c1_2 <- (df$q10c1*2 + df$q10c2)/3 # weighted average because q8da has two questions
  # 
  # df$q10c3 <- as.factor(df$q10c3)
  # levels(df$q10c3) <- c("Incorrect","Correct")
  
  return(subset(df, select = c(q1a, q1b, q1, q2, q3neg, q3pos, q4a, q4b, q5neg, q5pos, q6, q7, q8, q9a, q9b, q10a_b, q10c
  )))
}

df <- cbind(df, scoring_level2(df))

df <- subset(df, select = c(sch, imputed_sch, grade, class, student_num, age, sex, 
                            q1a, q1b, q1, q2, q3neg, q3pos, q4a, q4b, q5neg, q5pos, q6, q7, q8, q9a, q9b, q10a_b, q10c,
                            control, submitdate, dob, T1, intervention, level))

dflevel2 <- df
rm(df)

# P6 (level 3) ----
setwd(sprintf("~%s/qtn/qtn2019-20/primary", setpath))
dfpre <- haven::read_sav("P6-pre-survey_450003_SPSS.sav")
dfpost <- haven::read_sav("P6-post-survey_450006_SPSS.sav")

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)

names(df)[names(df)=="Q01"] <- "class" 
df$grade <- as.integer(as.vector(substr(df$class,1,1)))
names(df)[names(df)=="Q02"] <- "student_num"
names(df)[names(df)=="Q03"] <- "dob"
df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

names(df)[names(df)=="Q04"] <- "sex" # female = 1; male = 2

names(df)[names(df)=="Q00"] <- "sch" 
df$sch[df$sch %in% ""] <- NA
df$sch <- as.integer(as.vector(substr(df$sch,2,3)))
df$imputed_sch[is.na(df$sch)] <- 1 # not missing school
df$sch[is.na(df$sch) & df$ipaddr == "210.3.171.211"] <- 2
df$sch[is.na(df$sch) & df$ipaddr == "202.126.220.202"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "210.176.51.222"] <- 5
df$sch[is.na(df$sch) & df$ipaddr == "210.3.102.78"] <- 11
df$sch[is.na(df$sch) & df$ipaddr == "210.3.74.230"] <- 12
df$imputed_sch[is.na(df$imputed_sch)] <- 0 # imputed school based on IP address

df$sch <- car::recode(df$sch, "
1 = 'KMS';
2 =  'YKH';
3 = 'LBC';
4 = 'TLP';
5 =  'CKL';
6 = 'PTS';
7 = 'LPM';
8 = 'YCMC';
9 = 'SHC';
10 = 'SAP';
11 = 'WSS';
12 = 'MOSLL';
13 = 'CYS';
14 = 'HEP';
15 = 'TPM'
")