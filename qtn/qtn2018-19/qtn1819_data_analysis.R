rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

SCHOOL <- array(c(c("primary", "secondary"),                      # switch between universal primary schools [1,1] & universal secondary schools [2,1]
                  c("primary_selective", "secondary_selective")), # switch between selective primary schools [1,2] & selective secondary schools [2,2]
                dim=c(2,2))                                         [2,1] # <- switch here

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_%s", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
library(scales, quietly = TRUE) # percent()
library(rowr, quietly = TRUE) # cbind.fill()
# install.packages('https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz', type="source", repos = NULL)
# library(plyr) # rbind.fill()
# library(rlist) # list.append()
library(xlsx, quietly = TRUE) # createWorkbook
# library(tidyverse)
library(dplyr, quietly = TRUE) # mutate()
library(nlme, quietly = TRUE) # lme
library(magrittr, quietly = TRUE) # pipe

df <- readRDS(sprintf("qtn1819_%s_long.rds", SCHOOL))
dfwide <- readRDS(sprintf("qtn1819_%s_wide.rds", SCHOOL))

df <- df %>%
  mutate(uid = paste(sch, class, student_num, dob, sex))  # create unique IDs from students (same as N in dfwide)
# df %>%
#   summarize(unique_id = n_distinct(uid))
df$uid <- as.factor(df$uid)
levels(df$uid) <- 1:n_distinct(df$uid)

dfwide$uid <- rep(1:nrow(dfwide))

df$gender <- car::recode(df$sex,  "
1 = 'female';
2 = 'male'
")
dfwide$gender <- car::recode(dfwide$sex,  "
1 = 'female';
2 = 'male'
")
df$male <- car::recode(df$sex,  "
1 = 0;
2 = 1
")

# define questions for datasets ----
{
if (SCHOOL == "primary"){
  SCHOOL_N <- 17
  DECIMAL_PLACES <- 2
  questions <- t(array(c(c("q1", "Mental Health Knowledge"), 
                         c("q2", "Subjective Happiness"),  
                         c("q3", "Anxiety (SCARED)"),  
                         c("q4neg", "Negative Thinking"), # CATS-N/P
                         c("q4pos", "Positive Thinking"),  # CATS-N/P
                         c("q5", "Empathy: Perspective Taking (C-IRI)"), 
                         c("q6", "Self-esteem (RSES)"),  
                         c("q7", "Gratitude (GQ-6)")), dim = c(2,8)))
  full_analysis_list<-c(3, 9)
  int_t0_t1_list<-c(2, 4:8, 11, 13:17)
  int_ctrl_list<-c(1)
  int_t0_list<-c(10, 12)
  } else if(SCHOOL == "secondary") {
  SCHOOL_N <- 14
  DECIMAL_PLACES <- 2
  questions <- t(array(c(c("q1_2", "Mental Health Knowledge"),  
                         c("q3", "Psychological Distress (GHQ)"),  
                         c("q4neg", "Negative Thinking"), 
                         c("q4pos", "Positive Thinking"), 
                         c("q5a", "Life Satisfaction (SLSS)"),
                         c("q5b", "Life Satisfaction (BMSLSS)"),
                         c("q6", "Empathy: Perspective Taking (C-IRI)"),  
                         c("q7", "Gratitude (GQ-6)")), dim = c(2,8)))
  full_analysis_list<-c(3, 5, 8, 11)
  int_t0_t1_list<-c(6, 7, 10, 14)
  int_ctrl_list<-c(1, 2)
  int_t0_list<-c(9, 12, 13)
  } else if(SCHOOL == "primary_selective") {
    SCHOOL_N <- 16 # no obs in schools 12, 15
    DECIMAL_PLACES <- 2
    questions <- t(array(c(c("q1", "Subjective Happiness (SHS)"), 
                           c("q2neg", "Negative Affect (PANAS)"),  
                           c("q2pos", "Positive Affect (PANAS)"),
                           c("q3", "Self-esteem (RSES)"),  
                           c("q4a", "Strengths Knowledge (SKS)"), 
                           c("q4b", "Strengths Use (SUS)"), 
                           c("q5neg", "Negative Thinking"),
                           c("q5pos", "Positive Thinking")), dim = c(2,8)))
    # the following code needs to be edited
    full_analysis_list<-c(11, 14)
    int_t0_t1_list<-c(1, 2, 4, 5, 16)
    int_ctrl_list<-c(10)
    int_t0_list<-c(3) # place holder
  } else if(SCHOOL == "secondary_selective") {
    SCHOOL_N <- 7 # only 1:5, 7 (total number of schools = 6)
    DECIMAL_PLACES <- 2
    questions <- t(array(c(c("A_PD", "Personal Distress (C-IRI)"),
                           c("A_FS", "Fantasy Scale (C-IRI)"),  
                           c("A_ES", "Empathy Scale (C-IRI)"),  
                           c("B01", "Empathy Quotient (EQ)"), 
                           c("C01", "Emotional Competence"), 
                           c("C02", "Behavioural Competence"), 
                           c("D01", "Life Satisfaction (SLSS)"),
                           c("D02", "Life Satisfaction (BMSLSS)"),
                           c("E01", "Psychological Distress (GHQ)")), dim = c(2,9)))
    full_analysis_list<-c(3, 4, 5)
    int_t0_t1_list<-c(1, 2, 7)
    int_ctrl_list<-c(8) # place holder
    int_t0_list<-c(8) # place holder
  }
  
}

tablecolnames <- c("Group",  "Gender",  "Participants",  "Average Age", "", 
                  "Question", "",  "Control T0", "Control T1", "Intervention T0",  "Intervention T1", "",
                  "OLS", "", "", "",
                  "MLM", "", "")

# functions for generating results for final report ----
na_if_ <- function(input, output = input, value = 0, check_object = FALSE){ # modify output to NA when input is NA/NaN/0 (default), default value can be changed to any number
  if (check_object == TRUE){
    if (!exists(deparse(substitute(input)))){
      return(NA)
    }
  }
  value <- ifelse(is.na(value), 0, value)# value cannot be NA because (input==NA) next line doesn't work like is.na(input)
  input <- ifelse(input=="NA", NA, input)
  input <- ifelse(input=="NaN", NaN, input)
  return(ifelse( !(is.na(input) | is.nan(input) | input==value), output, NA))
} 

get_demog <- function(df, school = NULL){
  table  <-  data.frame(matrix(ncol = 4,  nrow = 0))
  colnames(table)  <-  c("Group",  "Gender",  "Participants",  "Average Age")
  table[1, 1]  <- "Intervention Group"
  table[1, 2]  <- "Male"
  table[2, 2]  <- "Female"
  table[3, 1]  <- "Subtotal"
  table[4, 1]  <- "Control Group"
  table[4, 2]  <- "Male"
  table[5, 2]  <- "Female"
  table[6, 1]  <- "Subtotal"
  table[7, 1]  <- "Total"
  
  filter_sex <- function(df, school, c, s){
    if (is.null(school)){
      data  <-  filter(df, control == c,  sex == s)
    } else {
      data  <-  filter(df, sch == school, control == c,  sex == s)
    }
    return(data)
  }
  
  int_male <- filter_sex(df, school, 0, 2)
  int_female <- filter_sex(df, school, 0, 1)
  ctrl_male <- filter_sex(df, school, 1, 2)
  ctrl_female <- filter_sex(df, school, 1, 1)

  n_int_male  <-  nrow(int_male)
  n_int_female  <-  nrow(int_female)
  n_ctrl_male  <-  nrow(ctrl_male)
  n_ctrl_female  <-  nrow(ctrl_female)
  
  n_int_total  <-  n_int_male + n_int_female
  n_ctrl_total  <-  n_ctrl_male + n_ctrl_female
  n_total  <-  n_int_total + n_ctrl_total

  n_int_male <- nrow(int_male)
  n_int_female <- nrow(int_female)
  n_ctrl_male <- nrow(ctrl_male)
  n_ctrl_female <- nrow(ctrl_female)
  
  "%//%" <- function(x,y) ifelse(y==0,0,(x/y)) # user-defined infix operator for division, return zero instead of Inf when denominator is zero
  
  per_int_male <- percent(n_int_male%//%n_int_total)
  per_int_female <- percent(n_int_female%//%n_int_total)
  per_ctrl_male <- percent(n_ctrl_male%//%n_ctrl_total)
  per_ctrl_female <- percent(n_ctrl_female%//%n_ctrl_total)
  
  mean_int <- round_format(mean(c(int_male$age, int_female$age), na.rm = TRUE), DECIMAL_PLACES)
  mean_ctrl <- round_format(mean(c(ctrl_male$age, ctrl_female$age), na.rm = TRUE), DECIMAL_PLACES)
  mean_total <- round_format(mean(c(int_male$age, int_female$age, ctrl_male$age, ctrl_female$age), na.rm = TRUE), DECIMAL_PLACES)
  
  sd_int <- round_format(sd(c(int_male$age, int_female$age), na.rm = TRUE), DECIMAL_PLACES)
  sd_ctrl <- round_format(sd(c(ctrl_male$age, ctrl_female$age), na.rm = TRUE), DECIMAL_PLACES)
  sd_total <- round_format(sd(c(int_male$age, int_female$age, ctrl_male$age, ctrl_female$age), na.rm = TRUE), DECIMAL_PLACES)
  
  table[1,3]<-na_if_(n_int_male, paste0(n_int_male, " (",per_int_male, ")"))
  table[2,3]<-na_if_(n_int_female, paste0(n_int_female, " (",per_int_female, ")"))
  table[3,3]<-na_if_(n_int_total)
  table[4,3]<-na_if_(n_ctrl_male, paste0(n_ctrl_male, " (",per_ctrl_male, ")"))
  table[5,3]<-na_if_(n_ctrl_female, paste0(n_ctrl_female, " (",per_ctrl_female, ")"))
  table[6,3]<-na_if_(n_ctrl_total)
  table[8,3]<-na_if_(n_total)
  
  table[1,4]<-na_if_(mean_int)
  table[2,4]<-na_if_(sd_int, paste0(" (S.D. = ",sd_int, ")"))
  table[4,4]<-na_if_(mean_ctrl)
  table[5,4]<-na_if_(sd_ctrl, paste0(" (S.D. = ",sd_ctrl, ")"))
  table[7,4]<-na_if_(mean_total)
  table[8,4]<-na_if_(sd_total, paste0(" (S.D. = ",sd_total, ")")) 
  return(table)
}

get_lm <- function(df, question, question_name = "Question", school = NULL, wide_format = FALSE){
  table  <-  data.frame(matrix(ncol = 3,  nrow = 0))
  
  table[1,1] <- question_name
  table[2,1] <- "N"
  table[3,1] <- "Regression coefficient (β)"
  table[4,1] <- "p-value"
  
  table[5,1] <- "Regression coefficient (β) (adjusted)"
  table[6,1] <- "p-value (adjusted for age and gender)"
  table[7,1] <- NA
  
  table[1,2] <- "Group"
  table[1,3] <- "Group x Time"

  if (wide_format){
    tryCatch({
    question_pre <- paste0(question, ".0")
    question_post <-  paste0(question, ".1")
    
    df <- eval_("df[!is.na(df$", question, "),]") # must select only those with both T0 and T1 responses
    
    reg_group <- eval_("lm(", question_pre, "~intervention, data = df)")
    n_group <- nobs(reg_group)
    beta_group <- summary(reg_group)$coefficients["intervention",1]
    p_group <- summary(reg_group)$coefficients["intervention",4]
    
    reg_group_adj <- eval_("lm(", question_pre, "~intervention+age+sex, data = df)")
    beta_group_adj <- summary(reg_group_adj)$coefficients["intervention",1]
    p_group_adj <- summary(reg_group_adj)$coefficients["intervention",4]  
    
    reg_groupxtime <- eval_("lm(", question, "~intervention, data = df)")
    n_groupxtime <- nobs(reg_groupxtime)
    beta_groupxtime <- summary(reg_groupxtime)$coefficients["intervention",1]
    p_groupxtime <- summary(reg_groupxtime)$coefficients["intervention",4]
    
    reg_groupxtime_adj <- eval_("lm(", question, "~intervention+age+sex, data = df)")
    beta_groupxtime_adj <- summary(reg_groupxtime_adj)$coefficients["intervention",1]
    p_groupxtime_adj  <- summary(reg_groupxtime_adj)$coefficients["intervention",4]
    
    table[2,2:3] <- c(n_group, n_groupxtime)
    
    table[3,2:3] <- c(round_format(beta_group, DECIMAL_PLACES), round_format(beta_groupxtime, DECIMAL_PLACES))
    table[4,2:3] <- c(starred_p(p_group, DECIMAL_PLACES), starred_p(p_groupxtime, DECIMAL_PLACES))
    
    table[5,2:3] <- c(round_format(beta_group_adj, DECIMAL_PLACES), round_format(beta_groupxtime_adj, DECIMAL_PLACES))
    table[6,2:3] <- c(starred_p(p_group_adj, DECIMAL_PLACES), starred_p(p_groupxtime_adj, DECIMAL_PLACES))
    
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
  } else {
    filter_t1 <- function(df, school, t1){
      if (is.null(school)){
        data  <-  filter(df, T1 == t1)
      } else {
        data  <-  filter(df, sch == school, T1 == t1)
      }
      return(data)
    }
    
    df_t0 <- filter_t1(df, school, 0)
    
    reg_groupxtime <- eval_("lm(", question, "~intervention*T1, data = df)")
    n_groupxtime <- nobs(reg_groupxtime)
    beta_groupxtime <- summary(reg_groupxtime)$coefficients["intervention:T1",1]
    p_groupxtime <- summary(reg_groupxtime)$coefficients["intervention:T1",4]
    
    reg_groupxtime_adj <- eval_("lm(", question, "~intervention*T1+age+sex, data = df)")
    beta_groupxtime_adj <- summary(reg_groupxtime_adj)$coefficients["intervention:T1",1]
    p_groupxtime_adj  <- summary(reg_groupxtime_adj)$coefficients["intervention:T1",4]
    
    n_group <- nobs(reg_groupxtime)
    beta_group <- summary(reg_groupxtime)$coefficients["intervention",1]
    p_group <- summary(reg_groupxtime)$coefficients["intervention",4]
    
    beta_group_adj <- summary(reg_groupxtime_adj)$coefficients["intervention",1]
    p_group_adj <- summary(reg_groupxtime_adj)$coefficients["intervention",4]
    
    table[2,2:3] <- c(n_group, n_groupxtime)
    
    table[3,2:3] <- c(round_format(beta_group, DECIMAL_PLACES), round_format(beta_groupxtime, DECIMAL_PLACES))
    table[4,2:3] <- c(starred_p(p_group, DECIMAL_PLACES), starred_p(p_groupxtime, DECIMAL_PLACES))
    
    table[5,2:3] <- c(round_format(beta_group_adj, DECIMAL_PLACES), round_format(beta_groupxtime_adj, DECIMAL_PLACES))
    table[6,2:3] <- c(starred_p(p_group_adj, DECIMAL_PLACES), starred_p(p_groupxtime_adj, DECIMAL_PLACES))
  }

  return(table)
}

get_lme <- function(df, question, question_name = "Question", school = NULL, wide_format = FALSE, multilevel_lvls = "1 | uid"){
  require(nlme)
  table  <-  data.frame(matrix(ncol = 3,  nrow = 0))
  
  table[1,1] <- question_name
  table[2,1] <- "N"
  table[3,1] <- "Regression coefficient (β)"
  table[4,1] <- "p-value"
  
  table[5,1] <- "Regression coefficient (β) (adjusted)"
  table[6,1] <- "p-value (adjusted for age and gender)"
  table[7,1] <- NA
  
  table[1,2] <- "Group"
  table[1,3] <- "Group x Time"

  if (wide_format){
  tryCatch({
    question_pre <- paste0(question, ".0")
    question_post <-  paste0(question, ".1")
    
    df <- eval_("df[!is.na(df$", question, "),]") # must select only those with both T0 and T1 responses

    reg_group <- eval_("lme(", question_pre, "~intervention, data = df, random = ~ 1 ", sub(".*\\|", "|", multilevel_lvls), # sub() "." is any char, "*" at least zero times
                       ", na.action=na.omit, control=lmeControl(opt='optim'))")
    n_group <- nobs(reg_group)
    beta_group <- summary(reg_group)$tTable["intervention",1]
    p_group <- summary(reg_group)$tTable["intervention",5]
    
    reg_group_adj <- eval_("lme(", question_pre, "~intervention+age+sex, data = df, random = ~ 1 ", sub(".*\\|", "|", multilevel_lvls), 
                           ", na.action=na.omit, control=lmeControl(opt='optim'))")
    beta_group_adj <- summary(reg_group_adj)$tTable["intervention",1]
    p_group_adj <- summary(reg_group_adj)$tTable["intervention",5]
    
    reg_groupxtime <- eval_("lme(", question, "~intervention, data = df, random = ~ ", multilevel_lvls, 
                            ", na.action=na.omit, control=lmeControl(opt='optim'))")
    n_groupxtime <- nobs(reg_groupxtime)
    beta_groupxtime <- summary(reg_groupxtime)$tTable["intervention",1]
    p_groupxtime <- summary(reg_groupxtime)$tTable["intervention",5]

    reg_groupxtime_adj <- eval_("lme(", question, "~intervention+age+sex, data = df, random = ~ ", multilevel_lvls,
                                ", na.action=na.omit, control=lmeControl(opt='optim'))")
    beta_groupxtime_adj <- summary(reg_groupxtime_adj)$tTable["intervention",1]
    p_groupxtime_adj  <- summary(reg_groupxtime_adj)$tTable["intervention",5]
    
    table[2,2:3] <- c(n_group, n_groupxtime)

    table[3,2:3] <- c(round_format(beta_group, DECIMAL_PLACES), round_format(beta_groupxtime, DECIMAL_PLACES))
    table[4,2:3] <- c(starred_p(p_group, DECIMAL_PLACES), starred_p(p_groupxtime, DECIMAL_PLACES))

    table[5,2:3] <- c(round_format(beta_group_adj, DECIMAL_PLACES), round_format(beta_groupxtime_adj, DECIMAL_PLACES))
    table[6,2:3] <- c(starred_p(p_group_adj, DECIMAL_PLACES), starred_p(p_groupxtime_adj, DECIMAL_PLACES))

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
  } else {
    tryCatch({
      filter_t1 <- function(df, school, t1){
        if (is.null(school)){
          data  <-  filter(df, T1 == t1)
        } else {
          data  <-  filter(df, sch == school, T1 == t1)
        }
        return(data)
      }
      
      df_t0 <- filter_t1(df, school, 0)
      df_t1 <- filter_t1(df, school, 1)  
      
      reg_groupxtime <- eval_("lme(", question, "~intervention*T1, data = df, random = ~ ", multilevel_lvls,
                              ", na.action=na.omit, control=lmeControl(opt='optim'))")
      n_groupxtime <- nobs(reg_groupxtime)
      beta_groupxtime <- summary(reg_groupxtime)$tTable["intervention:T1",1]
      p_groupxtime <- summary(reg_groupxtime)$tTable["intervention:T1",5]
      
      reg_groupxtime_adj <- eval_("lme(", question, "~intervention*T1+age+sex, data = df, random = ~ ", multilevel_lvls,
                                  ", na.action=na.omit, control=lmeControl(opt='optim'))")
      beta_groupxtime_adj <- summary(reg_groupxtime_adj)$tTable["intervention:T1",1]
      p_groupxtime_adj  <- summary(reg_groupxtime_adj)$tTable["intervention:T1",5]
      
      n_group <- nobs(reg_groupxtime)
      beta_group <- summary(reg_groupxtime)$tTable["intervention",1]
      p_group <- summary(reg_groupxtime)$tTable["intervention",5]
      
      beta_group_adj <- summary(reg_groupxtime_adj)$tTable["intervention",1]
      p_group_adj <- summary(reg_groupxtime_adj)$tTable["intervention",5]
      
      table[2,2:3] <- c(n_group, n_groupxtime)
      
      table[3,2:3] <- c(round_format(beta_group, DECIMAL_PLACES), round_format(beta_groupxtime, DECIMAL_PLACES))
      table[4,2:3] <- c(starred_p(p_group, DECIMAL_PLACES), starred_p(p_groupxtime, DECIMAL_PLACES))
      
      table[5,2:3] <- c(round_format(beta_group_adj, DECIMAL_PLACES), round_format(beta_groupxtime_adj, DECIMAL_PLACES))
      table[6,2:3] <- c(starred_p(p_group_adj, DECIMAL_PLACES), starred_p(p_groupxtime_adj, DECIMAL_PLACES))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
  }
  return(table)
}

get_full <- function(df, question, question_name = "Question", school = NULL){
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  # table[1,7] <- question_name
  # table[2,7] <- "N"
  # table[3,7] <- "Regression coefficient (β)"
  # table[4,7] <- "p-value"
  # 
  # table[5,7] <- "Regression coefficient (β) (adjusted)"
  # table[6,7] <- "p-value (adjusted for age and gender)"
  # 
  # table[1,8] <- "Group"
  # table[1,9] <- "Group x Time"
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  filter_c_t1 <- function(df, school, c, t1){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == t1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T1 == t1)
    }
    return(data)
  }
  
  filter_t1 <- function(df, school, t1){
    if (is.null(school)){
      data  <-  filter(df, T1 == t1)
    } else {
      data  <-  filter(df, sch == school, T1 == t1)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)
  ctrl <- filter_control(df, school,  1)
  
  df_t0 <- filter_t1(df, school, 0)
  df_t1 <- filter_t1(df, school, 1)
  
  int_t0 <- filter_c_t1(df, school, 0, 0)
  int_t1 <- filter_c_t1(df, school, 0, 1)
  ctrl_t0 <- filter_c_t1(df, school, 1, 0)
  ctrl_t1 <- filter_c_t1(df, school, 1, 1)
  
  mean_int_t0 <- eval_("summary(lm(", question, "~1, data = int_t0))")$coefficients[1,1]
  mean_int_t1 <- eval_("summary(lm(", question, "~1, data = int_t1))")$coefficients[1,1]
  mean_ctrl_t0 <- eval_("summary(lm(", question, "~1, data = ctrl_t0))")$coefficients[1,1]
  mean_ctrl_t1 <- eval_("summary(lm(", question, "~1, data = ctrl_t1))")$coefficients[1,1]
  
  sd_int_t0 <- summ(eval_("int_t0$", question))[1,5]
  sd_int_t1 <- summ(eval_("int_t1$", question))[1,5]
  sd_ctrl_t0 <- summ(eval_("ctrl_t0$", question))[1,5]
  sd_ctrl_t1 <- summ(eval_("ctrl_t1$", question))[1,5]
  
  mean_diff_int <- mean_int_t1 - mean_int_t0
  mean_diff_ctrl <- mean_ctrl_t1 - mean_ctrl_t0
  
  p_int <-  eval_("summary(lm(", question, "~T1, data = int))")$coefficients[2,4]
  p_ctrl <- eval_("summary(lm(", question, "~T1, data = ctrl))")$coefficients[2,4]
  
  p_int_adj <-  eval_("summary(lm(", question, "~T1+age+sex, data = int))")$coefficients[2,4]
  p_ctrl_adj <- eval_("summary(lm(", question, "~T1+age+sex, data = ctrl))")$coefficients[2,4]
  
  table[1,3:6] <- c(summ(eval_("ctrl$", question))[1,2], NA, summ(eval_("int$", question))[1,2], NA)
  table[2,3:6] <- c(summ(eval_("ctrl_t0$", question))[1,2], summ(eval_("ctrl_t1$", question))[1,2], summ(eval_("int_t0$", question))[1,2], summ(eval_("int_t1$", question))[1,2])
  table[3,3:6] <- round_format(c(mean_ctrl_t0, mean_ctrl_t1, mean_int_t0, mean_int_t1), DECIMAL_PLACES)
  table[4,3] <- na_if_(sd_ctrl_t0, round_format(sd_ctrl_t0, DECIMAL_PLACES))
  table[4,4] <- na_if_(sd_ctrl_t1, round_format(sd_ctrl_t1, DECIMAL_PLACES))
  table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
  table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
  table[5,3:6] <- c(round_format(mean_diff_ctrl, DECIMAL_PLACES), NA, round_format(mean_diff_int, DECIMAL_PLACES), NA)
  table[6,3:6] <- c(starred_p(p_ctrl, DECIMAL_PLACES), NA, starred_p(p_int, DECIMAL_PLACES), NA)
  table[7,3:6] <- c(starred_p(p_ctrl_adj, DECIMAL_PLACES), NA, starred_p(p_int_adj, DECIMAL_PLACES), NA)
  
  return(table)
}

get_full_wide <- function(df, question, question_name = "Question", school = NULL){
  # df<-dfwide
  # question<-"q1"
  # question_name <- "Question 1"
  # school <- 3
  question_pre <- paste0(question, ".0")
  question_post <-  paste0(question, ".1")
  
  df <- eval_("df[!is.na(df$", question, "),]") # must select only those with both T0 and T1 responses
  
  # print(c(question, question_pre, question_post))
  
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  # table[1,7] <- question_name
  # table[2,7] <- "N"
  # table[3,7] <- "Regression coefficient (β)"
  # table[4,7] <- "p-value"
  # 
  # table[5,7] <- "Regression coefficient (β) (adjusted)"
  # table[6,7] <- "p-value (adjusted for age and gender)"
  # 
  # table[1,8] <- "Group"
  # table[1,9] <- "Group x Time"
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  filter_c_t1 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c, T1 == 1)
    }
    return(data)
  }
  
  filter_c_t0 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T0 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T0 == 1)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)
  ctrl <- filter_control(df, school,  1)
  
  int_t0 <- filter_c_t0(df, school, 0)
  int_t1 <- filter_c_t1(df, school, 0)
  ctrl_t0 <- filter_c_t0(df, school, 1)
  ctrl_t1 <- filter_c_t1(df, school, 1)
  
  # print(summ(int_t0$sch,int_t1$sch,ctrl_t0$sch,ctrl_t1$sch))
  # print(summ(int$q1.0,int$q1.1,ctrl$q1.0,ctrl$q1.1))
  
  tryCatch({
    mean_int_t0 <- summary(lm(get(question_pre)~1, data = int, na.action=na.omit))$coefficients[1,1]
    mean_int_t1 <- summary(lm(get(question_post)~1, data = int, na.action=na.omit))$coefficients[1,1]
    mean_ctrl_t0 <-  summary(lm(get(question_pre)~1, data = ctrl, na.action=na.omit))$coefficients[1,1]
    mean_ctrl_t1 <- summary(lm(get(question_post)~1, data = ctrl, na.action=na.omit))$coefficients[1,1]
    
    sd_int_t0 <- summ(eval_("int$", question_pre))[1,5]
    sd_int_t1 <- summ(eval_("int$", question_post))[1,5]
    sd_ctrl_t0 <- summ(eval_("ctrl$", question_pre))[1,5]
    sd_ctrl_t1 <- summ(eval_("ctrl$", question_post))[1,5]
    
    mean_diff_int <- summary(lm(with(int, get(question))~1, data = int, na.action=na.omit))$coefficients[1,1]
    mean_diff_ctrl <- summary(lm(with(ctrl, get(question))~1, data = ctrl, na.action=na.omit))$coefficients[1,1]
    
    p_int <-  summary(lm(with(int, get(question))~1, data = int, na.action=na.omit))$coefficients[1,4]
    p_ctrl <- summary(lm(with(ctrl, get(question))~1, data = ctrl, na.action=na.omit))$coefficients[1,4]
    
    p_int_adj <-  summary(lm(with(int, get(question))~age+sex, data = int, na.action=na.omit))$coefficients[1,4]
    p_ctrl_adj <- summary(lm(with(ctrl, get(question))~age+sex, data = ctrl, na.action=na.omit))$coefficients[1,4]
    
    table[1,3:6] <- c(nrow(ctrl)
                      , NA, 
                      nrow(int)
                      , NA)
    table[2,3:6] <- c(nrow(ctrl_t0), 
                      nrow(ctrl_t1), 
                      nrow(int_t0),
                      nrow(int_t1))
    
    table[3,3] <- round_format(mean_ctrl_t0, DECIMAL_PLACES)
    table[3,4] <- round_format(mean_ctrl_t1, DECIMAL_PLACES)
    table[3,5] <- round_format(mean_int_t0, DECIMAL_PLACES)
    table[3,6] <- round_format(mean_int_t1, DECIMAL_PLACES)
    
    table[4,3] <- na_if_(sd_ctrl_t0, round_format(sd_ctrl_t0, DECIMAL_PLACES))
    table[4,4] <- na_if_(sd_ctrl_t1, round_format(sd_ctrl_t1, DECIMAL_PLACES))
    table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
    table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
    table[5,3:6] <- c(round_format(mean_diff_ctrl, DECIMAL_PLACES), NA, round_format(mean_diff_int, DECIMAL_PLACES), NA)
    table[6,3:6] <- c(starred_p(p_ctrl, DECIMAL_PLACES), NA, starred_p(p_int, DECIMAL_PLACES), NA)
    table[7,3:6] <- c(starred_p(p_ctrl_adj, DECIMAL_PLACES), NA, starred_p(p_int_adj, DECIMAL_PLACES), NA)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
  return(table)
}

get_int <- function(df, question, question_name = "Question", school = NULL){
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  # table[1,7] <- question_name
  # table[2,7] <- "N"
  # table[3,7] <- "Regression coefficient (β)"
  # table[4,7] <- "p-value"
  # 
  # table[5,7] <- "Regression coefficient (β) (adjusted)"
  # table[6,7] <- "p-value (adjusted for age and gender)"
  # 
  # table[1,8] <- "Group"
  # table[1,9] <- "Group x Time"
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  filter_c_t1 <- function(df, school, c, t1){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == t1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T1 == t1)
    }
    return(data)
  }
  
  filter_t1 <- function(df, school, t1){
    if (is.null(school)){
      data  <-  filter(df, T1 == t1)
    } else {
      data  <-  filter(df, sch == school, T1 == t1)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)

  df_t0 <- filter_t1(df, school, 0)
  df_t1 <- filter_t1(df, school, 1)
  
  int_t0 <- filter_c_t1(df, school, 0, 0)
  int_t1 <- filter_c_t1(df, school, 0, 1)

  mean_int_t0 <- eval_("summary(lm(", question, "~1, data = int_t0))")$coefficients[1,1]
  mean_int_t1 <- eval_("summary(lm(", question, "~1, data = int_t1))")$coefficients[1,1]

  sd_int_t0 <- summ(eval_("int_t0$", question))[1,5]
  sd_int_t1 <- summ(eval_("int_t1$", question))[1,5]

  mean_diff_int <- mean_int_t1 - mean_int_t0

  p_int <-  eval_("summary(lm(", question, "~T1, data = int))")$coefficients[2,4]

  p_int_adj <-  eval_("summary(lm(", question, "~T1+age+sex, data = int))")$coefficients[2,4]

  table[1,3:6] <- c(NA, NA, summ(eval_("int$", question))[1,2], NA)
  table[2,3:6] <- c(NA, NA, summ(eval_("int_t0$", question))[1,2], summ(eval_("int_t1$", question))[1,2])
  table[3,3:6] <- round_format(c(NA, NA, mean_int_t0, mean_int_t1), DECIMAL_PLACES)
  table[4,3] <- NA
  table[4,4] <- NA
  table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
  table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
  table[5,3:6] <- c(NA, NA, round_format(mean_diff_int, DECIMAL_PLACES), NA)
  table[6,3:6] <- c(NA, NA, starred_p(p_int, DECIMAL_PLACES), NA)
  table[7,3:6] <- c(NA, NA, starred_p(p_int_adj, DECIMAL_PLACES), NA)
  
  return(table)
}

get_int_wide <- function(df, question, question_name = "Question", school = NULL){
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  question_pre <- paste0(question, ".0")
  question_post <-  paste0(question, ".1")
  
  df <- eval_("df[!is.na(df$", question, "),]") # must select only those with both T0 and T1 responses
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  # table[1,7] <- question_name
  # table[2,7] <- "N"
  # table[3,7] <- "Regression coefficient (β)"
  # table[4,7] <- "p-value"
  # 
  # table[5,7] <- "Regression coefficient (β) (adjusted)"
  # table[6,7] <- "p-value (adjusted for age and gender)"
  # 
  # table[1,8] <- "Group"
  # table[1,9] <- "Group x Time"
  
  filter_c_t1 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c, T1 == 1)
    }
    return(data)
  }
  
  filter_c_t0 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T0 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T0 == 1)
    }
    return(data)
  }
  
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  filter_t1 <- function(df, school, t1){
    if (is.null(school)){
      data  <-  filter(df, T1 == t1)
    } else {
      data  <-  filter(df, sch == school, T1 == t1)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)
  
  int_t0 <- filter_c_t0(df, school, 0)
  int_t1 <- filter_c_t1(df, school, 0)

  mean_int_t0 <- summary(lm(get(question_pre)~1, data = int, na.action=na.omit))$coefficients[1,1]
  mean_int_t1 <- summary(lm(get(question_post)~1, data = int, na.action=na.omit))$coefficients[1,1]

  sd_int_t0 <- summ(eval_("int$", question_pre))[1,5]
  sd_int_t1 <- summ(eval_("int$", question_post))[1,5]

  mean_diff_int <- summary(lm(with(int, get(question))~1, data = int, na.action=na.omit))$coefficients[1,1]

  p_int <-  summary(lm(with(int, get(question))~1, data = int, na.action=na.omit))$coefficients[1,4]

  p_int_adj <-  summary(lm(with(int, get(question))~age+sex, data = int, na.action=na.omit))$coefficients[1,4]

    table[1,3:6] <- c(NA, NA, summ(eval_("int$", question))[1,2], NA)
  table[2,3:6] <- c(NA, NA, summ(eval_("int_t0$", question))[1,2], summ(eval_("int_t1$", question))[1,2])
  table[3,3:6] <- round_format(c(NA, NA, mean_int_t0, mean_int_t1), DECIMAL_PLACES)
  table[4,3] <- NA
  table[4,4] <- NA
  table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
  table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
  table[5,3:6] <- c(NA, NA, round_format(mean_diff_int, DECIMAL_PLACES), NA)
  table[6,3:6] <- c(NA, NA, starred_p(p_int, DECIMAL_PLACES), NA)
  table[7,3:6] <- c(NA, NA, starred_p(p_int_adj, DECIMAL_PLACES), NA)
  
  return(table)
}

# int vs ctrl at T0 or T1
get_int_ctrl_wide <- function(df, question, question_name = "Question", school = NULL){
  question_pre <- paste0(question, ".0")
  question_post <-  paste0(question, ".1")
  
  df <- eval_("df[!is.na(df$", question, "),]") # must select only those with both T0 and T1 responses
  
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  filter_t1 <- function(df, school){
    if (is.null(school)){
      data  <-  filter(df, T1 == 1)
    } else {
      data  <-  filter(df, sch == school, T1 == 1)
    }
    return(data)
  }
  
  filter_t0 <- function(df, school){
    if (is.null(school)){
      data  <-  filter(df, T0 == 1)
    } else {
      data  <-  filter(df, sch == school, T0 == 1)
    }
    return(data)
  }
  
  filter_c_t1 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c, T1 == 1)
    }
    return(data)
  }
  
  filter_c_t0 <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T0 == 1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T0 == 1)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)
  ctrl <- filter_control(df, school,  1)
  
  df_t0 <- filter_t0(df, school)
  df_t1 <- filter_t1(df, school)
  
  int_t0 <- filter_c_t0(df, school, 0)
  int_t1 <- filter_c_t1(df, school, 0)
  ctrl_t0 <- filter_c_t0(df, school, 1)
  ctrl_t1 <- filter_c_t1(df, school, 1)
  
  mean_int_t0 <- trycatchNA(summary(lm(get(question_pre)~1, data = int, na.action=na.omit))$coefficients[1,1])
  mean_int_t1 <- trycatchNA(summary(lm(get(question_post)~1, data = int, na.action=na.omit))$coefficients[1,1])
  mean_ctrl_t0 <-  trycatchNA(summary(lm(get(question_pre)~1, data = ctrl, na.action=na.omit))$coefficients[1,1])
  mean_ctrl_t1 <- trycatchNA(summary(lm(get(question_post)~1, data = ctrl, na.action=na.omit))$coefficients[1,1])
  
  tryCatch({
  
  sd_int_t0 <-trycatchNA(summ(eval_("int$", question_pre))[1,5])
  sd_int_t1 <- trycatchNA(summ(eval_("int$", question_post))[1,5])
  sd_ctrl_t0 <- trycatchNA(summ(eval_("ctrl$", question_pre))[1,5])
  sd_ctrl_t1 <- trycatchNA(summ(eval_("ctrl$", question_post))[1,5])
  
  mean_diff_t0 <- trycatchNA(summary(lm(with(df_t0, get(question_pre))~intervention, data = df_t0, na.action=na.omit))$coefficients[2,1])
  mean_diff_t1 <- trycatchNA(summary(lm(with(df_t1, get(question_post))~intervention, data = df_t1, na.action=na.omit))$coefficients[2,1])
  
  p_t0 <-  trycatchNA(summary(lm(with(df_t0, get(question_pre))~intervention, data = df_t0, na.action=na.omit))$coefficients[2,4])
  p_t1 <-  trycatchNA(summary(lm(with(df_t1, get(question_post))~intervention, data = df_t1, na.action=na.omit))$coefficients[2,4])
  
  p_t0_adj <- trycatchNA(summary(lm(with(df_t0, get(question_pre))~intervention+age+sex, data = df_t0, na.action=na.omit))$coefficients[2,4])
  p_t1_adj <- trycatchNA(summary(lm(with(df_t1, get(question_post))~intervention+age+sex, data = df_t1, na.action=na.omit))$coefficients[2,4])
  
  table[1,3:6] <- c(summ(eval_("ctrl$", question_pre))[1,2] + summ(eval_("ctrl$", question_post))[1,2]
                    , NA, summ(eval_("int$", question_pre))[1,2] + summ(eval_("int$", question_post))[1,2]
                    , NA)
  table[2,3:6] <- c(summ(eval_("ctrl_t0$", question_pre))[1,2] + summ(eval_("ctrl_t0$", question_post))[1,2], 
                    summ(eval_("ctrl_t1$", question_pre))[1,2] + summ(eval_("ctrl_t1$", question_post))[1,2], 
                    summ(eval_("int_t0$", question_pre))[1,2] + summ(eval_("int_t0$", question_post))[1,2], 
                    summ(eval_("int_t1$", question_pre))[1,2] + summ(eval_("int_t1$", question_post))[1,2])
  
  table[3,3] <- round_format(mean_ctrl_t0, DECIMAL_PLACES)
  table[3,4] <- round_format(mean_ctrl_t1, DECIMAL_PLACES)
  table[3,5] <- round_format(mean_int_t0, DECIMAL_PLACES)
  table[3,6] <- round_format(mean_int_t1, DECIMAL_PLACES)
  
  table[4,3] <- na_if_(sd_ctrl_t0, round_format(sd_ctrl_t0, DECIMAL_PLACES))
  table[4,4] <- na_if_(sd_ctrl_t1, round_format(sd_ctrl_t1, DECIMAL_PLACES))
  table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
  table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
  
  table[5,3] <- round_format(mean_diff_t0, DECIMAL_PLACES)
  table[5,5] <- round_format(mean_diff_t1, DECIMAL_PLACES)
  
  if (!is.na(mean_diff_t0)){
    table[6,3] <- starred_p(p_t0, DECIMAL_PLACES)
    table[7,3] <- starred_p(p_t0_adj, DECIMAL_PLACES)
  }
  if (!is.na(mean_diff_t1)){
    table[6,5] <- starred_p(p_t1, DECIMAL_PLACES)
    table[7,5] <- starred_p(p_t1_adj, DECIMAL_PLACES)
  }
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
  
  return(table)
}

# int vs ctrl at T0 or T1
get_int_ctrl <- function(df, question, question_name = "Question", school = NULL){
  table  <-  data.frame(matrix(ncol = 6,  nrow = 0))
  
  table[1,1] <- question_name
  table[1,2] <- "N"
  table[3,2] <- "Mean"
  table[4,2] <- "Standard Deviation"
  table[5,2] <- "Mean Difference"
  table[6,2] <- "p-value"
  table[7,2] <- "p-value (adjusted for age and gender)"
  
  filter_c_t1 <- function(df, school, c, t1){
    if (is.null(school)){
      data  <-  filter(df, control == c,  T1 == t1)
    } else {
      data  <-  filter(df, sch == school, control == c,  T1 == t1)
    }
    return(data)
  }
  
  filter_t1 <- function(df, school, t1){
    if (is.null(school)){
      data  <-  filter(df, T1 == t1)
    } else {
      data  <-  filter(df, sch == school, T1 == t1)
    }
    return(data)
  }
  
  filter_control <- function(df, school, c){
    if (is.null(school)){
      data  <-  filter(df, control == c)
    } else {
      data  <-  filter(df, sch == school, control == c)
    }
    return(data)
  }
  
  int <- filter_control(df, school, 0)
  ctrl <- filter_control(df, school,  1)
  
  df_t0 <- filter_t1(df, school, 0)
  df_t1 <- filter_t1(df, school, 1)

  int_t0 <- filter_c_t1(df, school, 0, 0)
  int_t1 <- filter_c_t1(df, school, 0, 1)
  ctrl_t0 <- filter_c_t1(df, school, 1, 0)
  ctrl_t1 <- filter_c_t1(df, school, 1, 1)
  
  mean_int_t0 <- eval_("summary(lm(", question, "~1, data = int_t0))")$coefficients[1,1]
  mean_int_t1 <- eval_("summary(lm(", question, "~1, data = int_t1))")$coefficients[1,1]
  mean_ctrl_t0 <- eval_("summary(lm(", question, "~1, data = ctrl_t0))")$coefficients[1,1]
  mean_ctrl_t1 <- eval_("summary(lm(", question, "~1, data = ctrl_t1))")$coefficients[1,1]
  
  sd_int_t0 <- summ(eval_("int_t0$", question))[1,5]
  sd_int_t1 <- summ(eval_("int_t1$", question))[1,5]
  sd_ctrl_t0 <- summ(eval_("ctrl_t0$", question))[1,5]
  sd_ctrl_t1 <- summ(eval_("ctrl_t1$", question))[1,5]
  
  mean_diff_t0 <- mean_int_t0 - mean_ctrl_t0
  mean_diff_t1 <- mean_int_t1 - mean_ctrl_t1
  
  p_t0 <-  eval_("summary(lm(", question, "~intervention, data = df_t0))")$coefficients[2,4]
  p_t1 <- eval_("summary(lm(", question, "~intervention, data = df_t1))")$coefficients[2,4]
  
  p_t0_adj <-  eval_("summary(lm(", question, "~intervention+age+sex, data = df_t0))")$coefficients[2,4]
  p_t1_adj <- eval_("summary(lm(", question, "~intervention+age+sex, data = df_t1))")$coefficients[2,4]
  
  table[1,3:6] <- c(summ(eval_("ctrl$", question))[1,2], NA, summ(eval_("int$", question))[1,2], NA)
  table[2,3:6] <- c(summ(eval_("ctrl_t0$", question))[1,2], summ(eval_("ctrl_t1$", question))[1,2], summ(eval_("int_t0$", question))[1,2], summ(eval_("int_t1$", question))[1,2])
  table[3,3:6] <- round_format(c(mean_ctrl_t0, mean_ctrl_t1, mean_int_t0, mean_int_t1), DECIMAL_PLACES)
  table[4,3] <- na_if_(sd_ctrl_t0, round_format(sd_ctrl_t0, DECIMAL_PLACES))
  table[4,4] <- na_if_(sd_ctrl_t1, round_format(sd_ctrl_t1, DECIMAL_PLACES))
  table[4,5] <- na_if_(sd_int_t0, round_format(sd_int_t0, DECIMAL_PLACES))
  table[4,6] <- na_if_(sd_int_t1, round_format(sd_int_t1, DECIMAL_PLACES))
  table[5,3:6] <- c(round_format(mean_diff_t0, DECIMAL_PLACES), NA, round_format(mean_diff_t1, DECIMAL_PLACES), NA)
  table[6,3:6] <- c(starred_p(p_t0, DECIMAL_PLACES), NA, starred_p(p_t1, DECIMAL_PLACES), NA)
  table[7,3:6] <- c(starred_p(p_t0_adj, DECIMAL_PLACES), NA, starred_p(p_t1_adj, DECIMAL_PLACES), NA)
  return(table)
}

sch_q_loop <- function(df, filename = "sch_q_loop.xlsx", school_loop = FALSE, wide_format = FALSE, mlm = FALSE, multilevel_lvls = "1 | uid"){ 
  require(xlsx)
  wb <- xlsx::createWorkbook(type="xlsx")
  for (i in (1:SCHOOL_N)){ # school i
    if (!school_loop) {
      i <- 0
      school <- NULL
    } else {
      school <- i
    }
    tryCatch({
      demog <- get_demog(df, school)
      assign(paste0("sch",deparse(as.numeric(i))), demog)
      for (j in 1:length(questions[,1])){ # question j
        if (wide_format){
          if (i %in% full_analysis_list){
            stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_t1_list){
            stats <- get_int_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_ctrl_list){
            stats <- get_int_ctrl_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_list){
            stats <- get_full_wide(df, questions[j,1], questions[j,2], school) # needs to be replaced
          } else {
            stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
          }
        } else {
          if (i %in% full_analysis_list){
            stats <- get_full(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_t1_list){
            stats <- get_int(df, questions[j,1], questions[j,2], school) 
          } else if (i %in% int_ctrl_list){
            stats <- get_int_ctrl(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_list){
            stats <- get_full(df, questions[j,1], questions[j,2], school) # needs to be replaced
          } else {
            stats <- get_full(df, questions[j,1], questions[j,2], school)
          }
        }
        assign(paste0("sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), stats)
        
        reg_tables <- get_lm(df, questions[j,1], questions[j,2], school, wide_format)
        assign(paste0("reg_sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), reg_tables)
        
        if (mlm){
          mlm_tables <- get_lme(df, questions[j,1], questions[j,2], school, wide_format, multilevel_lvls)
          assign(paste0("mlm_sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), mlm_tables)
        }
      }
      all_stats <- do.call(rbind, mget(sprintf("sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      # all_stats <- rbind(eval_("sch", i, "q1"),
      #                    eval_("sch", i, "q2"),
      #                    eval_("sch", i, "q3"), 
      #                    eval_("sch", i, "q4"), 
      #                    eval_("sch", i, "q5"), 
      #                    eval_("sch", i, "q6"), 
      #                    eval_("sch", i, "q7"), 
      #                    eval_("sch", i, "q8"))
      all_reg <- do.call(rbind, mget(sprintf("reg_sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      if (mlm){
      all_mlm <- do.call(rbind, mget(sprintf("mlm_sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      demog_stats <- cbind.fill(eval_("sch", i), array(), all_stats, array(), all_reg, array(), all_mlm, fill = NA)
      } else {
      demog_stats <- cbind.fill(eval_("sch", i), array(), all_stats, array(), all_reg, fill = NA)
      }
            # for (j in 1:length(questions[,1])){ # remove temporary tables
      #   eval_("rm(sch", i, "q", j, ")")
      # }
      # colnames(demog_stats)[5] <- ""
      colnames(demog_stats)  <-  tablecolnames[1:length(demog_stats)]
      if (length(demog_stats)>(length(tablecolnames)-3)){
        colnames(demog_stats)[length(tablecolnames)-1] <- paste0("1 ",sub(".*\\|", "|", multilevel_lvls))
        colnames(demog_stats)[length(tablecolnames)] <- multilevel_lvls
      }
      assign(paste0("sch", deparse(as.numeric(i)), "_all"), demog_stats)
      
      sheet <- createSheet(wb, sheetName = paste0("S", deparse(as.numeric(i))))
      assign(paste0("sheet", deparse(as.numeric(i))), sheet)
      addDataFrame(get_("sch", deparse(as.numeric(i)), "_all"), get_("sheet", as.numeric(i)), 
                   startRow=1, startColumn=1,
                   row.names = FALSE, showNA = FALSE)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "for school", deparse(as.numeric(i)), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
    if (!school_loop) {
      break
    }
    #eval_("rm(sch", i, ")") # remove temporary tables
  }
  xlsx::saveWorkbook(wb, filename, password=NULL)
}

q_loop <- function(df, type = "full", wide_format = FALSE, s = NULL, mlm = FALSE , multilevel_lvls = "1 | uid"){ 
    school <- s
    if (type=="full"){
      i <- full_analysis_list[1]
    } else if (type=="int_t0_t1"){
      i <- int_t0_t1_list[1]
    } else if (type=="int_ctrl"){
      i <- int_ctrl_list[1]
    } else if (type=="int_t0"){
      i <- int_t0_list[1]
    }  
    # tryCatch({
      demog <- get_demog(df, school)
      assign(paste0("sch",deparse(as.numeric(i))), demog)
      for (j in 1:length(questions[,1])){ # question j
        if (wide_format){
          if (i %in% full_analysis_list){
            stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_t1_list){
            stats <- get_int_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_ctrl_list){
            stats <- get_int_ctrl_wide(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_list){
            
          } else {
            stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
          }
        } else {
          if (i %in% full_analysis_list){
            stats <- get_full(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_t1_list){
            stats <- get_int(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_ctrl_list){
            stats <- get_int_ctrl(df, questions[j,1], questions[j,2], school)
          } else if (i %in% int_t0_list){
            
          } else {
            stats <- get_full(df, questions[j,1], questions[j,2], school)
          }
        }
        assign(paste0("sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), stats)
        
        reg_tables <- get_lm(df, questions[j,1], questions[j,2], school, wide_format)
        assign(paste0("reg_sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), reg_tables)
        if (mlm){
        mlm_tables <- get_lme(df, questions[j,1], questions[j,2], school, wide_format, multilevel_lvls)
        assign(paste0("mlm_sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), mlm_tables)
        }
      }
      all_stats <- do.call(rbind, mget(sprintf("sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      # all_stats <- rbind(eval_("sch", i, "q1"),
      #                    eval_("sch", i, "q2"),
      #                    eval_("sch", i, "q3"), 
      #                    eval_("sch", i, "q4"), 
      #                    eval_("sch", i, "q5"), 
      #                    eval_("sch", i, "q6"), 
      #                    eval_("sch", i, "q7"), 
      #                    eval_("sch", i, "q8"))
      all_reg <- do.call(rbind, mget(sprintf("reg_sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      if (mlm){
      all_mlm <- do.call(rbind, mget(sprintf("mlm_sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
      demog_stats <- cbind.fill(eval_("sch", i), array(), all_stats, array(), all_reg, array(), all_mlm, fill = NA)
      } else {
      demog_stats <- cbind.fill(eval_("sch", i), array(), all_stats, array(), all_reg, fill = NA)
      }
      # for (j in 1:length(questions[,1])){ # remove temporary tables
      #   eval_("rm(sch", i, "q", j, ")")
      # }
      colnames(demog_stats)[5] <- ""
      colnames(demog_stats)  <-  tablecolnames[1:length(demog_stats)]
      if (length(demog_stats)>(length(tablecolnames)-3)){
        colnames(demog_stats)[length(tablecolnames)-1] <- paste0("1 ",sub(".*\\|", "|", multilevel_lvls))
        colnames(demog_stats)[length(tablecolnames)] <- multilevel_lvls
        }
      return(demog_stats)
    # }, error=function(e){cat("ERROR :",conditionMessage(e), "for school", deparse(as.numeric(i)), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
}

q_loop_noreg <- function(df, type = "full", wide_format = FALSE, s = NULL, mlm = FALSE , multilevel_lvls = "1 | uid"){ 
  school <- s
  if (type=="full"){
    i <- full_analysis_list[1]
  } else if (type=="int_t0_t1"){
    i <- int_t0_t1_list[1]
  } else if (type=="int_ctrl"){
    i <- int_ctrl_list[1]
  } else if (type=="int_t0"){
    i <- int_t0_list[1]
  }  
  # tryCatch({
  demog <- get_demog(df, school)
  assign(paste0("sch",deparse(as.numeric(i))), demog)
  for (j in 1:length(questions[,1])){ # question j
    if (wide_format){
      if (i %in% full_analysis_list){
        stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_t0_t1_list){
        stats <- get_int_wide(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_ctrl_list){
        stats <- get_int_ctrl_wide(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_t0_list){
        
      } else {
        stats <- get_full_wide(df, questions[j,1], questions[j,2], school)
      }
    } else {
      if (i %in% full_analysis_list){
        stats <- get_full(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_t0_t1_list){
        stats <- get_int(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_ctrl_list){
        stats <- get_int_ctrl(df, questions[j,1], questions[j,2], school)
      } else if (i %in% int_t0_list){
        
      } else {
        stats <- get_full(df, questions[j,1], questions[j,2], school)
      }
    }
    assign(paste0("sch",deparse(as.numeric(i)), "q",deparse(as.numeric(j))), stats)
  }
  all_stats <- do.call(rbind, mget(sprintf("sch%iq%i", i, (1:length(questions[,1]))))) #mget() instead of get() to get objects into a list
  # all_stats <- rbind(eval_("sch", i, "q1"),
  #                    eval_("sch", i, "q2"),
  #                    eval_("sch", i, "q3"), 
  #                    eval_("sch", i, "q4"), 
  #                    eval_("sch", i, "q5"), 
  #                    eval_("sch", i, "q6"), 
  #                    eval_("sch", i, "q7"), 
  #                    eval_("sch", i, "q8"))

  demog_stats <- cbind.fill(eval_("sch", i), array(), all_stats, fill = NA)

  # for (j in 1:length(questions[,1])){ # remove temporary tables
  #   eval_("rm(sch", i, "q", j, ")")
  # }
  colnames(demog_stats)[5] <- ""
  colnames(demog_stats)  <-  tablecolnames[1:length(demog_stats)]
  if (length(demog_stats)>(length(tablecolnames)-3)){
    colnames(demog_stats)[length(tablecolnames)-1] <- paste0("1 ",sub(".*\\|", "|", multilevel_lvls))
    colnames(demog_stats)[length(tablecolnames)] <- multilevel_lvls
  }
  return(demog_stats)
  # }, error=function(e){cat("ERROR :",conditionMessage(e), "for school", deparse(as.numeric(i)), "\n")}, warning=function(cond){cat("WARNING :",conditionMessage(cond), "\n")})
}


# final report results ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_%s/qtn1819_%s_results", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2), SCHOOL))

# pri_selective_lm <-  q_loop(df,  type = "full", wide_format = FALSE, mlm = FALSE)

int_ctrl <- q_loop(df, type = "int_ctrl", wide_format = FALSE)
# int_ctrl_wide <- q_loop(dfwide, type = "int_ctrl",  wide_format = TRUE) # same results as int_ctrl
full_sch_id <- q_loop(df,  type = "full", wide_format = FALSE, mlm = TRUE, multilevel_lvls = "1 + T1 | sch/uid")
full_wide_sch <-  q_loop(dfwide,  type = "full", wide_format = TRUE, mlm = TRUE, multilevel_lvls = "1 + T1 | sch")
write_excel(sprintf("qtn1819_%s_results_raw.xlsx", SCHOOL)
            , int_ctrl, full_sch_id, full_wide_sch)
rm(int_ctrl, full_sch_id, full_wide_sch)

# after adding S15 (DMS) to the secondary universal data
# MLM (lme) throws an error for q5b [Life Satisfaction (BMSLSS] (see problematic formula below)
# lme(q5b~intervention, data = dfwide, random = ~ 1 + T1 | sch ,na.action=na.omit, control=lmeControl(opt='optim'))  

# S02_int <- q_loop_noreg(df, type = "int_t0_t1", wide_format = FALSE, s = 2)
# S02_int_wide <- q_loop_noreg(dfwide, type = "int_t0_t1", wide_format = TRUE, s = 2)
# 
# full_sch_id_S02 <- q_loop(df[which(df$sch==2 | (df$sch!=2 & df$control==1)),],  type = "full", wide_format = FALSE, mlm = TRUE, multilevel_lvls = "1 + T1 | sch/uid")
# full_wide_sch_S02 <-  q_loop(dfwide[which(dfwide$sch==2 | (dfwide$sch!=2 & dfwide$control==1)),],  type = "full", wide_format = TRUE, mlm = TRUE, multilevel_lvls = "1 + T1 | sch")
# write_excel(sprintf("qtn1819_%s_S02_results_raw.xlsx", SCHOOL)
#             , S02_int, S02_int_wide, full_sch_id_S02, full_wide_sch_S02)


# sch_q_loop(dfwide, "between_wide_tables.xlsx", school_loop = FALSE, wide_format = TRUE)

# sch_q_loop(dfwide, ".xlsx", school_loop = TRUE, wide_format = TRUE)

# full_wide_form1 <- q_loop(dfwide[dfwide$form==1,],  type = "full", wide_format = TRUE)
# write_excel("full_wide_form1.xlsx", full_wide_form1)

# FDR correction ----
reg_table <- function(data, within_group = FALSE, random_effect = '1 + T1 | sch/uid'){
  require(nlme)
  table <- data.frame(matrix(ncol = 10,  nrow = 0))
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    df <- eval_("data[which(!is.na(data$", dep_var, ")),]")
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    row_count <- row_count + 1
    
    colnames(table)  <-  c("Variable", "N", "Beta", "Lower CI", "Upper CI",  "p-value", "significance", "adj. p-value", "significance2", "L.Ratio")
    
    if (within_group){
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~T1+age+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ~ ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~T1+age+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    } else {
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~intervention*T1+age+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ~ ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      # iferror(
      #   fit_lm <- eval_(
      #     "lme(", dep_var, "~intervention*T1+age+gender, 
      # data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      # random = ~ 1  | sch/uid, 
      # na.action=na.omit, control=lmeControl(opt='optim'))")
      #   , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~intervention*T1+age+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                     round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                     sep = ", ")
    }
    
    for (var in c('(Intercept)', 'gendermale', 'age', 'T1', 'intervention', 'intervention:T1')){
      if (within_group){
        if (var %in% c('intervention', 'intervention:T1')){
          next
        }
      }
      
      table[row_count, 1] <- var
      
      n <- iferror(summary(fit)$dim$N, NA)
      beta <- iferror(summary(fit)$tTable[var, 1], NA)
      se <-  iferror(summary(fit)$tTable[var, 2], NA)
      
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
      p_value <- iferror(summary(fit)$tTable[var, 5], NA)
      table[row_count, 2] <-  n/2
      table[row_count, 3] <-  round_format(beta, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  format(p_value, scientific = FALSE)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    }
    table <- format(table, scientific = FALSE)
    table[is.na(table)] <- ""
    table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  }
  return(table)
}

table <- reg_table(df, random_effect = '1 + T1 | sch/uid')
table <- reg_table(df[which(df$intervention==1),], within_group = TRUE)

for (var in c('(Intercept)', 'gendermale', 'age', 'T1'
              , 'intervention', 'intervention:T1'
              )){
  # BKY (two-stage BH)
  # library(cp4p) # this package require installing three bioconductor packages, qvalue, multtest, & limma. e.g. BiocManager::install("limma")
  table[which(table$Variable==var), 8] <- 
  cp4p::adjust.p(table[which(table$Variable==var), 6], pi0.method="bky", alpha = 0.05)$adjp[[2]]
  
  # p.adjust(table[which(table$Variable==var), 6], method = 'BH') 
  
  # pvalue <- mutoss::two.stage(table[which(table$Variable==var), 6], alpha = 0.05)$adjPValues
  # if (is.null(pvalue)){
  #   pvalue <- mutoss::two.stage(table[which(table$Variable==var), 6], alpha = 0.05)$Pvals[['adjusted.pvals']] # somehow when unadjusted pvalue is too small, a different format is returned
  # }
  # table[which(table$Variable==var), 9] <- pvalue
}
table$significance2 <- lapply(table[, 8], starred_p, 3)

# calculate ICC
ICC.lme <- function(out) {
  varests <- as.numeric(VarCorr(out)[1:2])
  return(paste("ICC =", varests[1]/sum(varests))) 
}

# descriptive statistics ----
df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]  %>% 
  group_by(intervention, gender) %>% filter(T1==1 & !is.na(gender)) %>% summarise(n=n(), mean=mean(age), sd=sd(age)) %>% as.data.frame() %>% View(.)

summary(lme(q1~intervention*T1+age+gender, data = df, random = ~ 1 + T1 | sch/uid
            , na.action=na.omit, control=lmeControl(opt='optim')))


summary(lme(q1~T1+age+gender, data = df[which(df$intervention==1),], random = ~ 1 + T1 | sch/uid
            , na.action=na.omit, control=lmeControl(opt='optim')))
