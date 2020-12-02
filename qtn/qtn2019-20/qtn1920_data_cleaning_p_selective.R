rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)

setwd(sprintf("~%s/qtn/qtn2019-20/primary/selective", setpath))
dfpre <- openxlsx::read.xlsx("Selective_pri_Pre_19-20.xlsx")
dfpost <- openxlsx::read.xlsx("Selective_pri_Post_19-20.xlsx")

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)

names(df)[1:5] <- c("sch", "intervention", "class", "student_num", "sex")
df$sch <- "YKH"
df$control <- ifelse(df$intervention, 0, 1)
df$sex <- ifelse(df$sex == 1, 2, 1) # from (1=M;.2=F) to (1=F;.2=M)

df$dob <-  as.Date(paste(df$`Date.of.birth.(month)`,
                         df$`Date.of.birth.(day)`,
                         df$`Date.of.birth.(year)`, sep = "/"), format = "%m/%d/%Y") 

df$submitdate <-  as.Date(paste(df$`Ass1-Questionnaire.date.(month)`,
                                df$`Ass1-Questionnaire.date.(day)`,
                                df$`Ass1-Questionnaire.date.(year)`, sep = "/"), format = "%m/%d/%Y") 

df$grade <- as.integer(as.vector(substr(df$class,1,1)))

df$age <- as.numeric(floor((as.Date("2020-06-30")-as.Date(df$dob))/365.2425))

df <- convert2NA(df, c(98, 99))

scoring <- function(df){
  df %>% select(starts_with("Ass1-P1")) %>% colnames(.) -> P1
  df %>% select(starts_with("Ass1-P2")) %>% colnames(.) -> P2 # Positive & Negative Affect (PANAS) range()
  df %>% select(starts_with("Ass1-P3")) %>% colnames(.) -> P3 # Self-esteem (RSES)
  df %>% select(starts_with("Ass1-P4")) %>% colnames(.) -> P4
  P4a <- P4[1:8]
  P4b <- P4[9:22]
  df %>% select(starts_with("Ass1-P5")) %>% colnames(.) -> P5

  questions <- c(P1, P2, P3, P4, P5)

  reverse_P1 <- P1[c(4)]
  df[reverse_P1] <- (df[reverse_P1]-8)*-1 # reverse (1:7) to (7:1)

  reverse_P2 <- P2[c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)] # Negative Affect questions

  reverse_P3 <- P3[c(2, 5, 6, 8, 9)]
  df[reverse_P3] <- (df[reverse_P3]-5)*-1 # reverse (1:4) to (4:1)

  reverse_P4a <- P4a[c(2)]
  df[reverse_P4a] <- (df[reverse_P4a]-8)*-1 # reverse (1:7) to (7:1)

  reverse_P5 <- P5[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking

  df <- df %>%
    mutate(q1 = rowSums(.[P1], na.rm = FALSE), # Subjective Happiness (SHS) range(4-28)
           q2neg = rowSums(.[reverse_P2], na.rm = FALSE),# Negative Affect (PANAS) range(10-50)
           q2pos = rowSums(.[P2[!(P2 %in% reverse_P2)]], na.rm = FALSE), # Positive Affect (PANAS) range(10-50)
           q3 = rowSums(.[P3], na.rm = FALSE),# Self-esteem (RSES) range(10-40)
           q4a = rowSums(.[P4a], na.rm = FALSE), # Strengths Knowledge Scale (SKS) range(8-56)
           q4b = rowSums(.[P4b], na.rm = FALSE), # Strengths Use Scale (SUS) range(14-98)
           q5neg = rowSums(.[reverse_P5], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
           q5pos = rowSums(.[P5[!(P5 %in% reverse_P5)]], na.rm = FALSE))  # Positive Thinking (CATS-N/P) range(0-40)

  return(subset(df, select = c(q1, q2neg, q2pos, q3, q4a, q4b, q5neg, q5pos
  )))
}


df <- cbind(df, scoring(df))

selective <- df

df <- subset(df, select = c(sch, grade, class, student_num, age, sex,
                            q1, q2neg, q2pos, q3, q4a, q4b, q5neg, q5pos,
                            control, submitdate, dob, T1, intervention))

saveRDS(df, file = "qtn1920_primary_selective.rds")
# write_excel("qtn1920_primary_selective.xlsx", selective)
