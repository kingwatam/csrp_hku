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

setwd(sprintf("~%s/qtn/qtn2019-20/primary", setpath))
# import pre & post-test data from sav files ----
dfpre <- haven::read_sav("survey_450000_SPSS.sav")
dfpost <- haven::read_sav("survey_450004_SPSS.sav")

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

df %>% select(starts_with("A01")) %>% colnames(.) -> P1a 
df %>% select(starts_with("AC0")) %>% colnames(.) -> P1b
df %>% select(starts_with("AB01")) %>% colnames(.) -> P2
df %>% select(starts_with("B01")) %>% colnames(.) -> P3
df %>% select(starts_with("C01")) %>% colnames(.) -> P4
df %>% select(starts_with("D01")) %>% colnames(.) -> P5
df %>% select(starts_with("E01")) %>% colnames(.) -> P6
df %>% select(starts_with("F01")) %>% colnames(.) -> P7
df %>% select(starts_with("G01")) %>% colnames(.) -> P8a
df %>% select(starts_with("G02")) %>% colnames(.) -> P8b
df %>% select(starts_with("G0") & ends_with("SQ001")) %>% colnames(.) -> P8c
df %>% select(starts_with("G05")) %>% colnames(.) -> P8d

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


reverse_P8d <- P8d[2]
df[reverse_P8d] <- (df[reverse_P8d]-3)*-1 # reverse (1:2) to (2:1)

df[P8d[4]] <- ifelse(df[P8d[4]] == 1, 1, 0) # 1 is correct

P8ba <- P8b[1:2]
P8bb <- P8b[3]
P8bc <- P8b[4:5]
P8da <- P8d[1:2]
P8db <- P8d[3]
P8dc <- P8d[4]

P8c1 <- P8c[1]
P8c2 <- P8c[2]

df <- df %>% 
  mutate(q1a = rowSums(.[P1a], na.rm = FALSE), # True/False questions
         q1b = rowSums(.[P1b], na.rm = FALSE), # MCQ questions
         q2 = rowSums(.[P2], na.rm = FALSE), # Subjective Happiness (SHS) range(1-7)
         q3 = rowSums(.[P3], na.rm = FALSE),# Anxiety (SCARED) range(0-18)
         q4neg = rowSums(.[reverse_P4], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q4pos = rowSums(.[P4[!(P4 %in% reverse_P4)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
         q5 = rowSums(.[P5], na.rm = FALSE), # C-IRI perspective taking range(0-24)
         q6 = rowSums(.[P6], na.rm = FALSE), # Self-esteem (RSES) range(10-40)
         q7 = rowSums(.[P7], na.rm = FALSE), # Gratitude GQ-6 range(6-42)
         
         q8a = rowMeans(.[P8a], na.rm = FALSE), # Compassion - common humanity subscale range(0-4), mean instead of sum
         q8b = rowMeans(.[P8b], na.rm = FALSE), # self-compassion range(0-4), mean instead of sum
         q8ba = rowMeans(.[P8ba], na.rm = FALSE), # self-compassion - self-kindness subscale range(0-4), mean instead of sum
         q8bb = .[[P8bb]], # self-compassion - self-judgement subscale range(0-4), mean instead of sum
         q8bc = rowMeans(.[P8bc], na.rm = FALSE), # self-compassion - common humanity subscale range(0-4), mean instead of sum
         # q8c = rowSums(.[P8c], na.rm = FALSE), # help-seeking, range(2-5)
         q8da =  rowMeans(.[P8da], na.rm = FALSE), # prejudice - fear/avoidance subscale range(0-2), mean instead of sum
         q8db =  .[[P8db]], # prejudice - unpredictability subscale range(0-2), mean instead of sum
         q8d2 =  .[[P8dc]] # understanding subscale range(0-1), mean instead of sum
  )

df$q1 <- df$q1a + df$q1b # Mental Health Knowledge range(0-15)
df$q8d1 <- (df$q8da*2 + df$q8db)/3 # weighted average because q8da has two questions

df$q8c1 <- as.factor(df[[P8c1]])
levels(df$q8c1) <- c("Yes","No","Not sure")

df$q8c2 <- as.factor(df[[P8c2]])
levels(df$q8c2) <- c("Yes","No")

df$q8d2 <- as.factor(df$q8d2)
levels(df$q8d2) <- c("Incorrect","Correct")

# selective program for 2020-2021 (S2) ----
df_se <- df %>% # selective
  filter(!is.na(sch)) %>%
  filter(T1 == 0 & sch == 2 & grade == 4) 

df_mean_T0 <- df %>%
  filter(!is.na(sch)) %>%
  filter(T1 == 0 & sch == 2) %>%
  group_by(sch) %>%
  summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)

# df_se <- distinct(df_se, sch, class, student_num, dob, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

df_se$sex <- ifelse(df_se$sex==2, "M", "F")
df_se <- df_se %>% arrange(df_se$sch, df_se$class, df_se$student_num)

table_loop <- function(df_se, df_mean){
  wb <- xlsx::createWorkbook(type="xlsx")
  i <- 2
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
        filter(T1 == 0) %>%
        group_by(sch) %>%
        summarise_at(c("q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7"), mean, na.rm = TRUE)
      
      table <- data.frame(matrix(ncol = 13,  nrow = 0))
      table <- subset(df_selective[df_selective$sch==i,], select = c("class",
                                                                     "student_num",
                                                                     "sex",
                                                                     "dob",
                                                                     "q1", "q2", "q3", "q4neg", "q4pos", "q5", "q6", "q7", "imputed_sch"))
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
                             "Gratitude",
                             "Matched from IP address")
      
      table$`Date of Birth` <- ifelse(is.na(table$`Date of Birth`), NA,
                                      as.character.Date(table$`Date of Birth`, tryFormats = "%Y/%m/%d"))
      table[nrow(table)+1,4] <- "Mean of Group with Lowest Self-esteem"
      table[nrow(table)+1,4] <- "School Mean (Pre-test)"
      table[nrow(table)-1,5:(length(table)-1)] <- sapply(round(df_selective_mean[df_selective_mean$sch==i,2:length(df_selective_mean)], 2),function(x)format(x,nsmall = 2))
      table[nrow(table),5:(length(table)-1)] <- sapply(round(df_mean[df_mean$sch==i,2:length(df_mean)], 2),function(x)format(x,nsmall = 2))
      
      sheet <- xlsx::createSheet(wb, sheetName = paste0("S", deparse(as.numeric(i))))
      assign(paste0("sheet", deparse(as.numeric(i))), sheet)
      xlsx::addDataFrame(as.data.frame(table), get_("sheet", as.numeric(i)),
                         startRow=1, startColumn=1,
                         row.names = FALSE, showNA = FALSE)
    }
  }
  xlsx::saveWorkbook(wb, "qtn1920_pri_selective_qtn2021.xlsx", password=NULL)
}

setwd(sprintf("~%s/qtn/qtn2019-20/primary/qtn1920_primary_selective_qtn2021", setpath))

table_loop(df_se, df_mean_T0)
