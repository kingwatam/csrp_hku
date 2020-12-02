rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))

source("helper_functions.R")
library(openxlsx)
library(stringr) # str_detect
library(dplyr) # select
library(janitor) # remove_empty
library(ordinal) # clm
library(nlme) #lme

# Primary School TE data cleaning ---- 
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_teacher_efficacy/primary", setpath))

dfpre <- openxlsx::read.xlsx("Teacher efficacy _data entry_prisch_2018-2019_Pre_combined.xlsx")
dfpost <- openxlsx::read.xlsx("Teacher efficacy _data entry_prisch_2018-2019_Post.xlsx")

dfpre <- dfpre[,1:53] # subset only the first 53 columns
dfpost <- dfpost[,1:53]

dfpre <- dfpre %>% janitor::remove_empty("rows")

dfpre$T1 <- 0
dfpost$T1 <- 1

# print.default <- function(x) cat(deparse(x),"\n") # helps print colnames for subsetting
# print(colnames(dfpost))

df<-plyr::rbind.fill(dfpre,dfpost) # dplyr::bind_rows gives an error
rm(dfpre, dfpost)

df <- subset(df, select = c("Last.4.digits.mobile.number", "School.Name", "Day.of.Questionnaire", 
                            "Month.of.Questionnaire", "Year.of.Questionnaire", "Gender:.(1=M.;.2=F)", 
                            "Years.of.teaching.(1=0-3years;.2=.4-6.years;.3=7-9years;.4=.10.years.or.more)",  
                            "Job.Title.(1=老師;.2=社工;.3=輔導員;.4=.科任老師;.5=其他.)", "科任老師.(任教科目)", "其他:",
                            "Will.you.teach.QTN:.(1=Yes;.0=No)",  "Teaching.School.Year", 
                            "Number.of.lessons.taught.(1=.less.than.3.classes;.2=.4-7.classes;.3=8.or.above.classes)",  
                            "Relatoinship.to.student.(1=.班主任.;.2=社工.;.3=.輔導員.;.4=.科任老師r.;.5=.其他)",  
                            "科任老師.(任教科目:__________)", "其他:___________",
                            df %>% select(starts_with("Ass")) %>% colnames(.),
                            "T1"))

names(df)[names(df)=="Last.4.digits.mobile.number"] <- "id"
names(df)[names(df)=="School.Name"] <- "sch"
names(df)[names(df)=="Day.of.Questionnaire"] <- "day"
names(df)[names(df)=="Month.of.Questionnaire"] <- "month"
names(df)[names(df)=="Year.of.Questionnaire"] <- "year"
names(df)[names(df)=="Gender:.(1=M.;.2=F)"] <- "sex"
names(df)[names(df)=="Years.of.teaching.(1=0-3years;.2=.4-6.years;.3=7-9years;.4=.10.years.or.more)"] <- "years_teaching"
names(df)[names(df)=="Job.Title.(1=老師;.2=社工;.3=輔導員;.4=.科任老師;.5=其他.)"] <- "job"
names(df)[names(df)=="Will.you.teach.QTN:.(1=Yes;.0=No)"] <- "teach_qtn"
names(df)[names(df)=="Teaching.School.Year"] <- "class"
names(df)[names(df)=="Number.of.lessons.taught.(1=.less.than.3.classes;.2=.4-7.classes;.3=8.or.above.classes)"] <- "lesson_num"
names(df)[names(df)=="Relatoinship.to.student.(1=.班主任.;.2=社工.;.3=.輔導員.;.4=.科任老師r.;.5=.其他)"] <- "relationship"
names(df)[names(df)=="科任老師.(任教科目:__________)"] <- "subject"
names(df)[names(df)=="其他:___________"] <- "other"

names(df)[names(df)=="Ass4_P5_1.(0-3)"] <- "Ass4_P5_1" # this causes problems later because the original variable name requires quotes

df <- df %>% select(-c("科任老師.(任教科目)", "其他:"))  # drop mostly empty variables

df$id <- gsub("\\D", "", df$id)

df[df=="99"] <- NA # replace all 99 values to NA
df[df=="98"] <- NA # replace all 99 values to NA

df$sex[df$sex=="F"] <- 2

# df$dup[duplicated(df$id)] <- 1

df$grade_form[str_detect(df$class, "6")] <- 6
df$grade_form[str_detect(df$class, "5")] <- 5
df$grade_form[str_detect(df$class, "4")] <- 4
df$grade_form[str_detect(df$class, "四")] <- 4
df$grade_form[str_detect(df$class, "五")] <- 5
df$grade_form[df$class == "TBC"] <- NA

df_primary <- df
rm(df)

# Secondary School TE data cleaning ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_teacher_efficacy/secondary", setpath))

dfpre <- openxlsx::read.xlsx("Teacher efficacy _data entry_secsch_2018-2019_Pre.xlsx")
dfpost <- openxlsx::read.xlsx("Teacher efficacy _data entry_secsch_2018-2019_Post.xlsx")

dfpre <- dfpre[,1:53] # subset only the first 53 columns
dfpost <- dfpost[,1:53]

dfpost <- dfpost %>% janitor::remove_empty("rows")

dfpre$T1 <- 0
dfpost$T1 <- 1

# print.default <- function(x) cat(deparse(x),"\n") # helps print colnames for subsetting
# print(colnames(dfpost))

df<-plyr::rbind.fill(dfpre,dfpost) 
rm(dfpre, dfpost)

df <- subset(df, select = c("Last.4.digits.mobile.number", "School.Name", "Day.of.Questionnaire", 
                            "Month.of.Questionnaire", "Year.of.Questionnaire", "Gender:.(1=M.;.2=F)", 
                            "Years.of.teaching.(1=0-3years;.2=.4-6.years;.3=7-9years;.4=.10.years.or.more)",  
                            "Job.Title.(1=老師;.2=社工;.3=輔導員;.4=.科任老師;.5=其他.)", "科任老師.(任教科目)", "其他:",
                            "Will.you.teach.QTN:.(1=Yes;.0=No)",  "Teaching.School.Year", 
                            "Number.of.lessons.taught.(1=.less.than.3.classes;.2=.4-7.classes;.3=8.or.above.classes)",  
                            "Relatoinship.to.student.(1=.班主任.;.2=社工.;.3=.輔導員.;.4=.科任老師r.;.5=.其他)",  
                            "科任老師.(任教科目:__________)", "其他:___________",
                            df %>% select(starts_with("Ass")) %>% colnames(.),
                            "T1"))

names(df)[names(df)=="Last.4.digits.mobile.number"] <- "id"
names(df)[names(df)=="School.Name"] <- "sch"
names(df)[names(df)=="Day.of.Questionnaire"] <- "day"
names(df)[names(df)=="Month.of.Questionnaire"] <- "month"
names(df)[names(df)=="Year.of.Questionnaire"] <- "year"
names(df)[names(df)=="Gender:.(1=M.;.2=F)"] <- "sex"
names(df)[names(df)=="Years.of.teaching.(1=0-3years;.2=.4-6.years;.3=7-9years;.4=.10.years.or.more)"] <- "years_teaching"
names(df)[names(df)=="Job.Title.(1=老師;.2=社工;.3=輔導員;.4=.科任老師;.5=其他.)"] <- "job"
names(df)[names(df)=="Will.you.teach.QTN:.(1=Yes;.0=No)"] <- "teach_qtn"
names(df)[names(df)=="Teaching.School.Year"] <- "class"
names(df)[names(df)=="Number.of.lessons.taught.(1=.less.than.3.classes;.2=.4-7.classes;.3=8.or.above.classes)"] <- "lesson_num"
names(df)[names(df)=="Relatoinship.to.student.(1=.班主任.;.2=社工.;.3=.輔導員.;.4=.科任老師r.;.5=.其他)"] <- "relationship"
names(df)[names(df)=="科任老師.(任教科目:__________)"] <- "subject"
names(df)[names(df)=="其他:___________"] <- "other"

df <- df %>% select(-c("科任老師.(任教科目)", "其他:"))  # drop empty variables


df$id <- gsub("\\D", "", df$id)

df[df=="99"] <- NA # replace all 99 values to NA
df[df=="98"] <- NA # replace all 99 values to NA

# df$dup[duplicated(df$id)] <- 1

df$grade_form[str_detect(df$class, "6")] <- 3  # value 3 means S3+
df$grade_form[str_detect(df$class, "1")] <- 1
df$grade_form[str_detect(df$class, "5")] <- 3 
df$grade_form[str_detect(df$class, "2")] <- 2
df$grade_form[str_detect(df$class, "3")] <- 3
df$grade_form[str_detect(df$class, "4")] <- 3
df$grade_form[str_detect(df$class, "一")] <- 1
df$grade_form[str_detect(df$class, "二")] <- 2
df$grade_form[str_detect(df$class, "三")] <- 3
df$grade_form[str_detect(df$class, "四")] <- 3
df$grade_form[df$class == "TBC"] <- NA

df_secondary <- df
rm(df)

# merge pri & sec datasets ----
df_primary$pri <- 1
df_secondary$pri <- 0

df <- plyr::rbind.fill(df_primary,df_secondary)
rm(df_primary, df_secondary)

df <- df %>%
  mutate(uid = paste(pri, id))  # create unique IDs from students (same as N in dfwide)
# df %>%
#   summarize(unique_id = n_distinct(uid))
df$uid <- as.factor(df$uid)
levels(df$uid) <- 1:n_distinct(df$uid)

df$uid[is.na(df$id)] <- NA

# outcome scoring ----
scoring <- function(df){
  df %>% select(starts_with("Ass1_P1")) %>% colnames(.) -> qA
  df %>% select(starts_with("Ass1_P2")) %>% colnames(.) -> qB
  df %>% select(starts_with("Ass1_P")) %>% colnames(.) -> qAB
  df %>% select(starts_with("Ass2_P3")) %>% colnames(.) -> qC
  df %>% select(starts_with("Ass3_P4")) %>% colnames(.) -> qD
  df %>% select(starts_with("Ass4_P5")) %>% colnames(.) -> qE
  
  reverse_qC <- qC[c(4)] 
  df[reverse_qC] <- (df[reverse_qC]-8)*-1 # reverse (1:7) to (7:1)
  
  reverse_qE <- qE[c(1, 3, 4, 7, 8, 12)]
  df[reverse_qE] <- (df[reverse_qE]-3)*-1 # reverse (0:3) to (3:0)
  
  df <- df %>% 
    mutate(q1 = rowMeans(.[qA], na.rm = FALSE),
           q2 = rowMeans(.[qB], na.rm = FALSE),
           q1_2 = rowMeans(.[qAB], na.rm = FALSE),
           q3 = rowSums(.[qC], na.rm = FALSE), # P3 Subjective Happiness, range(4-28)
           q4 = rowSums(.[qD], na.rm = FALSE), # P4 Life Satisfaction (SWLS), range(5-35)
           q5 = rowSums(.[qE], na.rm = FALSE)) # P5 Psychological Distress (GHQ), range(0-36)
  
  return(subset(df, select = c(q1, q2, q1_2, q3, q4, q5
  )))
}

df <- cbind(df, scoring(df))

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_teacher_efficacy", setpath))

primary <- df[which(df$pri==1),]
secondary <- df[which(df$pri==0),]

write_excel("qtn1819_teacher_efficacy_data.xlsx", primary, secondary)

get_results <- function(df) {
  df_pre <- df[!is.na(df$id) & df$T1==0,] # remove missing IDs for merging
  df_post <- df[!is.na(df$id) & df$T1==1,]
  
  dfwide<-merge(df_pre, df_post, by.x = c("id"),
                by.y = c("id"),
                all.x=TRUE, all.y = TRUE, suffixes = c(".0",".1"))
  
  dfwide_both<-merge(df_pre, df_post, by.x = c("id"),
                     by.y = c("id"),
                     all.x=FALSE, all.y = FALSE, suffixes = c(".0",".1"))
  
  for (i in df %>% select(starts_with("Ass")) %>% colnames(.)){
    dfwide_both$question <- eval_(paste0("dfwide_both$", i, ".1"))-eval_(paste0("dfwide_both$", i, ".0"))
    names(dfwide_both)[names(dfwide_both)=="question"] <- i
  }
  
  table  <-  data.frame(matrix(ncol = 8,  nrow = 0))
  colnames(table)  <-  c("Variable Names", 
                         "N", "Mean T0", "Mean T1", 
                         "Regression (Odds Ratios)", "Regression (p-value)",
                         "Multilevel Regression (Odds Ratios)", "Multilevel Regression (p-value)")
  DECIMAL_PLACES <- 2
  j<-0
  for (i in df %>% select(starts_with("Ass")) %>% colnames(.)){
    j <- j+1
    table[j, 1] <- i
    # table[j, 2] <- summ(eval_(paste0("dfwide_both$", i)))[1,2] 
    # table[j, 3] <- round_format(summ(eval_(paste0("dfwide_both$", i, ".0")))[1,3], DECIMAL_PLACES)
    # table[j, 4] <- round_format(summ(eval_(paste0("dfwide_both$", i, ".1")))[1,3], DECIMAL_PLACES)
    # 
    # table[j, 5] <- starred_p(wilcox.test(eval_(paste0("dfwide_both$", i, ".0")),eval_(paste0("dfwide_both$", i, ".1")), paired = TRUE)$p.value, DECIMAL_PLACES)
    # table[j, 6] <- starred_p(t.test(eval_(paste0("dfwide_both$", i, ".0")),eval_(paste0("dfwide_both$", i, ".1")), paired = TRUE)$p.value, DECIMAL_PLACES)
    
    table[j, 2] <- summ(eval_(paste0("df$", i, "[df$T1==0]")))[1,2] + summ(eval_(paste0("df$", i, "[df$T1==1]")))[1,2]
    
    table[j, 3] <- round_format(summ(eval_(paste0("df$", i, "[df$T1==0]")))[1,3], DECIMAL_PLACES)
    table[j, 4] <- round_format(summ(eval_(paste0("df$", i, "[df$T1==1]")))[1,3], DECIMAL_PLACES)
    
    # table[j, 10] <- starred_p(wilcox.test(eval_(paste0("df$", i, "[df$T1==0]")),eval_(paste0("df$", i, "[df$T1==1]")), paired = FALSE)$p.value, DECIMAL_PLACES)
    # table[j, 11] <- starred_p(t.test(eval_(paste0("df$", i, "[df$T1==0]")),eval_(paste0("df$", i, "[df$T1==1]")), paired = FALSE)$p.value, DECIMAL_PLACES)

    clm_model <- clm(as.factor(get(i)) ~ T1, data = df, link="logit", Hess=TRUE, na.action=na.omit)
    table[j, 5] <- round_format(exp(summary(clm_model)$coef["T1",1]), DECIMAL_PLACES)
    table[j, 6] <- starred_p(summary(clm_model)$coef["T1",4], DECIMAL_PLACES)

    count <- 2 # starting at 2 due to 1 sometimes would give erroneous highly significant p-value results
    while(is.na(table[j, 8]) & count <= 10){
      clmm_model <- clmm(as.factor(get(i)) ~ T1 + (1 | uid), data = df, link="logit", Hess=TRUE, na.action=na.omit, nAGQ=count)
      trycatchNA(table[j, 7] <- round_format(exp(summary(clmm_model)$coef["T1",1]), DECIMAL_PLACES))
      trycatchNA(table[j, 8] <- starred_p(summary(clmm_model)$coef["T1",4], DECIMAL_PLACES))
      # print(paste(deparse(substitute(df)), i ,count))
      count <- count + 1
    }
  }
  table[j+1, 1:8] <- c("Variable Names", 
                     "N", "Mean T0", "Mean T1", 
                     "Regression (Difference)", "Regression (p-value)",
                     "Multilevel Regression (Difference)", "Multilevel Regression (p-value)")
  table[j+2, 1] <- "Part 1 Average"
  table[j+3, 1] <- "Part 2 Average"
  table[j+4, 1] <- "Parts 1 & 2 Average"
  table[j+5, 1] <- "Subjective Happiness"
  table[j+6, 1] <- "Life Satisfaction (SWLS)"
  table[j+7, 1] <- "Psychological Distress (GHQ)"
  for (question in df %>% select(starts_with("q")) %>% colnames(.)){
    q_num <- which(question ==  df %>% select(starts_with("q")) %>% colnames(.))
    
    table[j+1+q_num, 2] <- summ(eval_(paste0("df$", question, "[df$T1==0]")))[1,2] + summ(eval_(paste0("df$", question, "[df$T1==1]")))[1,2]
    
    table[j+1+q_num, 3] <- round_format(summ(eval_(paste0("df$", question, "[df$T1==0]")))[1,3], DECIMAL_PLACES)
    table[j+1+q_num, 4] <- round_format(summ(eval_(paste0("df$", question, "[df$T1==1]")))[1,3], DECIMAL_PLACES)
    
    lm_model <- lm(get(question) ~ T1, data = df, na.action=na.omit)
    table[j+1+q_num, 5] <- round_format(summary(lm_model)$coef["T1",1], DECIMAL_PLACES)
    table[j+1+q_num, 6] <- starred_p(summary(lm_model)$coef["T1",4], DECIMAL_PLACES)
    
    lme_model <- eval_("lme(", question, "~ T1, data = df, random = ~ 1 | uid, na.action=na.omit, control=lmeControl(opt='optim'))")
    table[j+1+q_num, 7] <- round_format(summary(lme_model)$tTable["T1",1], DECIMAL_PLACES)
    table[j+1+q_num, 8] <- starred_p(summary(lme_model)$tTable["T1",5], DECIMAL_PLACES)
  }
  # rm(j)
  return(table)
}

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_teacher_efficacy/", setpath))

primary <- get_results(df[which(df$pri==1),])
secondary <- get_results(df[which(df$pri==0),])

write_excel("qtn1819_teacher_efficacy_results.xlsx", primary, secondary)
