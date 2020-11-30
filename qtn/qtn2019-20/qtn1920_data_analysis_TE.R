rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/RA HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(ordinal) # clm
library(lme4) # lmer
library(lmerTest) # calculate p-values in summary()
library(nlme)

# Secondary school TE data cleaning ---- 
setwd(sprintf("~%s/qtn/qtn2019-20/teacher_efficacy/secondary", setpath))
dfpre <- openxlsx::read.xlsx("Teacher efficacy _data entry_secsch_2019-2020_Pre.xlsx")
dfpost <- haven::read_sav("survey_700000_SPSS.sav")

variable_labels <- sapply(dfpost[,25:61], function(x) attr(x, "label")) %>% as.data.frame()
variable_labels$var <- row.names(variable_labels)
names(variable_labels)[1] <- "label"
variable_labels$label[13] <- gsub("[[:punct:]]", "", variable_labels$label[13])

names(dfpre)[names(dfpre) == "Last.4.digits.mobile.number"] <- "mobile_num"
names(dfpre)[names(dfpre) == "School.Name"] <- "sch"

names(dfpost)[names(dfpost) == "D01x01"] <- "mobile_num"
names(dfpost)[names(dfpost) == "D01x02"] <- "sch"

dfpost$sch <- car::recode(as.character(dfpost$sch), "
'S05' = 'KTLS';
'S07' = 'TKP';
'S08' = 'LWLC'
")

dfpre <- dfpre %>% select(c("mobile_num", "sch"), everything())
dfpost <- dfpost %>% select(c("mobile_num", "sch"), everything())

dfpre <- dfpre[, c(1:2, 17:53)]
dfpost <- dfpost[, c(1:2, 25:61)]

temp <- dfpost %>% select(starts_with("S05x01_")) %>% colnames(.) 
dfpost[, temp] <- dfpost[, temp]-1 # from 1:4 to 0:3
rm(temp)

dfpre$T1 <- 0
dfpost$T1 <- 1

names(dfpost)[1:39] <- names(dfpre)[1:39]
variable_labels$var[1:37] <- names(dfpre)[3:39]

df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)
df <- convert2NA(df, c(98, 99))
df$primary <- 0

# Primary school TE data cleaning ---- 
setwd(sprintf("~%s/qtn/qtn2019-20/teacher_efficacy/primary", setpath))
dfpre <- openxlsx::read.xlsx("Teacher efficacy _data entry_prisch_2019-2020_Pre_king.xlsx")
dfpost <- openxlsx::read.xlsx("Teacher efficacy _data entry_prisch_2019-2020_Post_king.xlsx")

dfpre <- dfpre[, c(1:2, 17:53)]
dfpost <- dfpost[, c(1:2, 17:53)]

dfpre$T1 <- 0
dfpost$T1 <- 1

dfpri <- rbind(dfpre, dfpost)
rm(dfpre, dfpost)

names(dfpri)[names(dfpri) == "Last.4.digits.mobile.number"] <- "mobile_num"
names(dfpri)[names(dfpri) == "School.Name"] <- "sch"

dfpri <- convert2NA(dfpri, c(99, 999))
dfpri$primary <- 1

names(dfpri) <- names(df)

df <- rbind(df, dfpri)
rm(dfpri)

# Outcome scoring ----
scoring <- function(df){
  df %>% select(starts_with("Ass1_P1")) %>% colnames(.) -> P1
  df %>% select(starts_with("Ass1_P2")) %>% colnames(.) -> P2
  df %>% select(starts_with("Ass1_P")) %>% colnames(.) -> P1_2
  df %>% select(starts_with("Ass2_P3")) %>% colnames(.) -> P3
  df %>% select(starts_with("Ass3_P4")) %>% colnames(.) -> P4
  df %>% select(starts_with("Ass4_P5")) %>% colnames(.) -> P5
  
  reverse_P3 <- P3[c(4)] 
  df[reverse_P3] <- (df[reverse_P3]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_P5 <- P5[c(1, 3, 4, 7, 8, 12)]
  df[reverse_P5] <- (df[reverse_P5]-3)*-1 # reverse (0:3) to (3:0)
  
  df <- df %>% 
    mutate(q1 = rowMeans(.[P1], na.rm = FALSE),
           q2 = rowMeans(.[P2], na.rm = FALSE),
           q1_2 = rowMeans(.[P1_2], na.rm = FALSE),
           q3 = rowSums(.[P3], na.rm = FALSE), # P3 Subjective Happiness, range(4-28)
           q4 = rowSums(.[P4], na.rm = FALSE), # P4 Life Satisfaction (SWLS), range(5-35)
           q5 = rowSums(.[P5], na.rm = FALSE)) # P5 Psychological Distress (GHQ), range(0-36)
  
  return(subset(df, select = c(q1, q2, q1_2, q3, q4, q5
  )))
}

df <- cbind(df, scoring(df))

# Generate uid ----
df <- df %>%
  mutate(uid = paste(primary, sch, mobile_num))  # create unique IDs 
df$uid <- as.factor(df$uid)
levels(df$uid) <- 1:n_distinct(df$uid)
df$uid[is.na(df$sch) | is.na(df$mobile_num)] <- NA

# Save cleaned dataset
setwd(sprintf("~%s/qtn/qtn2019-20/teacher_efficacy", setpath))

primary <- df[which(df$primary==1),]
secondary <- df[which(df$primary==0),]

write_excel("qtn1920_teacher_efficacy_data.xlsx", primary, secondary)

# Generate results ----
get_results <- function(df, wilcox = FALSE, MLM =  FALSE){ 
  # CLM is used to replace the Wilcoxon rank-sum test (unpaired) 
  # CLMM is the multilevel version of CLM to replace the (paired) Wilcoxon signed-rank test 
  DECIMAL_PLACES <- 2
  
  dftemp <- df %>%
    filter(!is.na(T1)) %>%
    group_by(primary, uid) %>%
    filter(n() == 1 | (n() == 2 & T1 == 0))  # keep T1 if both T0 & T1 avaiable
  
  if (!wilcox & MLM){
    table  <-  data.frame(matrix(ncol = 9,  nrow = 0))
    colnames(table)  <-  c("Variable Names", "Question item", 
                           "N", "Mean T0", "Mean T1",
                           "Regression (Odds Ratios)", "Regression (p-value)",
                           "Multilevel Regression (Odds Ratios)", "Multilevel Regression (p-value)")
  } else if (!wilcox & !MLM){
    table  <-  data.frame(matrix(ncol = 7,  nrow = 0))
    colnames(table)  <-  c("Variable Names", "Question item", 
                           "N", "Mean T0", "Mean T1",
                           "Regression (Odds Ratios)", "Regression (p-value)")
  } else if (wilcox & !MLM){
    table  <-  data.frame(matrix(ncol = 7,  nrow = 0))
    colnames(table)  <-  c("Variable Names", "Question item", 
                           "N", "Mean T0", "Mean T1",
                           "Wilcox rank-sum (unpaired)", "p-value")
  } else {
    stop("Error: CLMM option can only be enabled when wilcox option is disabled!")
  }
  
  j <-0 # initialize row count
  for (i in df %>% select(starts_with("Ass")) %>% colnames(.)){
    j <- j+1
    
    table[j, 1] <- i
    table[j, 2] <- variable_labels$label[variable_labels$var %in% i]
    
    table[j, 3] <- summ(eval_(paste0("dftemp$", i, "[dftemp$T1==0]")))[1,2] + summ(eval_(paste0("dftemp$", i, "[dftemp$T1==1]")))[1,2]
    
    table[j, 4] <- round_format(summ(eval_(paste0("df$", i, "[df$T1==0]")))[1,3], DECIMAL_PLACES)
    table[j, 5] <- round_format(summ(eval_(paste0("df$", i, "[df$T1==1]")))[1,3], DECIMAL_PLACES)
    
    if (wilcox){
      table[j, 6] <- starred_p(wilcox.test(eval_(paste0("df$", i, "[df$T1==0]")),eval_(paste0("df$", i, "[df$T1==1]")), paired = FALSE)$p.value, DECIMAL_PLACES,
                               wilcox.test(eval_(paste0("df$", i, "[df$T1==0]")),eval_(paste0("df$", i, "[df$T1==1]")), paired = FALSE)$statistic)
      table[j, 7] <- round_format(wilcox.test(eval_(paste0("df$", i, "[df$T1==0]")),eval_(paste0("df$", i, "[df$T1==1]")), paired = FALSE)$p.value, DECIMAL_PLACES)
      
    } else if (!wilcox & !MLM){
      clm_model <- clm(as.factor(get(i)) ~ T1, data = df, link="logit", Hess=TRUE, na.action=na.omit)
      table[j, 6] <- starred_p(summary(clm_model)$coef["T1",4], DECIMAL_PLACES, exp(summary(clm_model)$coef["T1",1]))
      table[j, 7] <- round_format(summary(clm_model)$coef["T1",4], DECIMAL_PLACES)
    } else if (!wilcox & MLM){
      clm_model <- clm(as.factor(get(i)) ~ T1, data = df, link="logit", Hess=TRUE, na.action=na.omit)
      table[j, 6] <- starred_p(summary(clm_model)$coef["T1",4], DECIMAL_PLACES, exp(summary(clm_model)$coef["T1",1]))
      table[j, 7] <- round_format(summary(clm_model)$coef["T1",4], DECIMAL_PLACES)
      
      count <- 2 # starting at 2 due to 1 sometimes would give erroneous highly significant p-value results
      while(is.na(table[j, 9]) & count <= 10){
        clmm_model <- clmm(as.factor(get(i)) ~ T1 + (1 | uid), data = df, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=count)
        trycatchNA(table[j, 8] <- starred_p(summary(clmm_model)$coef["T1",4], DECIMAL_PLACES, exp(summary(clmm_model)$coef["T1",1])))
        trycatchNA(table[j, 9] <- round_format(summary(clmm_model)$coef["T1",4], DECIMAL_PLACES))
        # print(paste(deparse(substitute(df)), i ,count))
        count <- count + 1
      }        
    }
  }
  
  if (MLM){
    table[j+1, 1:9] <- c("Variable Names", "", 
                         "N", "Mean T0", "Mean T1", 
                         "Regression (Difference)", "Regression (p-value)",
                         "Multilevel Regression (Difference)", "Multilevel Regression (p-value)")
  } else {
    table[j+1, 1:7] <- c("Variable Names", "", 
                         "N", "Mean T0", "Mean T1", 
                         "Regression (Difference)", "Regression (p-value)")
  }
  
  table[j+2, 1] <- "Part 1 Average"
  table[j+3, 1] <- "Part 2 Average"
  table[j+4, 1] <- "Parts 1 & 2 Average"
  table[j+5, 1] <- "Subjective Happiness"
  table[j+6, 1] <- "Life Satisfaction (SWLS)"
  table[j+7, 1] <- "Psychological Distress (GHQ)"
  
  for (question in df %>% select(starts_with("q")) %>% colnames(.)){
    q_num <- which(question ==  df %>% select(starts_with("q")) %>% colnames(.))
    
    table[j+1+q_num, 3] <- summ(eval_(paste0("dftemp$", question, "[dftemp$T1==0]")))[1,2] + summ(eval_(paste0("dftemp$", question, "[dftemp$T1==1]")))[1,2]
    
    table[j+1+q_num, 4] <- round_format(summ(eval_(paste0("df$", question, "[df$T1==0]")))[1,3], DECIMAL_PLACES)
    table[j+1+q_num, 5] <- round_format(summ(eval_(paste0("df$", question, "[df$T1==1]")))[1,3], DECIMAL_PLACES)
  
    lm_model <- lm(get(question) ~ T1, data = df, na.action=na.omit)
    table[j+1+q_num, 6] <- starred_p(summary(lm_model)$coef["T1", 4], DECIMAL_PLACES, summary(lm_model)$coef["T1", 1])
    table[j+1+q_num, 7] <- round_format(summary(lm_model)$coef["T1", 4], DECIMAL_PLACES)
    
    if (MLM){
      lme_model <- lmer(get(question)~ T1+ (1 | uid), REML = TRUE, data =  df, control=lmerControl(optimizer='nloptwrap'))
      table[j+1+q_num, 8] <- starred_p(summary(lme_model)$coef["T1", 5], DECIMAL_PLACES, summary(lme_model)$coef["T1", 1])
      table[j+1+q_num, 9] <- round_format(summary(lme_model)$coef["T1", 5], DECIMAL_PLACES)
    }
  }
  
  return(table)
}

primary_results <- get_results(df[df$primary %in% 1, ], wilcox = FALSE, MLM = FALSE) # only 3 teachers matched with pre- & post-test
secondary_results <- get_results(df[df$primary %in% 0, ], wilcox = FALSE, MLM = FALSE) # only 1 teacher matched with pre- & post-test

write_excel("qtn1920_teacher_efficacy_results.xlsx", primary_results, secondary_results)
