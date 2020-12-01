rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/RA HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(lme4)
library(lmerTest) # calculate p-values in summary()
library(multcomp) # glht
library(dplyr)

SCHOOL <- array(c(c("primary", "secondary"),                      # switch between universal primary schools [1,1] & universal secondary schools [2,1]
                  c("primary_selective", "secondary_selective")), # switch between selective primary schools [1,2] & selective secondary schools [2,2]
                dim=c(2,2))                                         [1,2] # <- switch here

if (SCHOOL == "primary_selective"){
  setwd(sprintf("~%s/qtn/qtn2019-20/%s/selective", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
  df <- readRDS(sprintf("qtn1920_%s.rds", SCHOOL))
} else {
  setwd(sprintf("~%s/qtn/qtn2019-20/%s", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
  df1 <- readRDS(sprintf("qtn1920_%s_level1.rds", SCHOOL))
  df2 <- readRDS(sprintf("qtn1920_%s_level2.rds", SCHOOL))
  df3 <- readRDS(sprintf("qtn1920_%s_level3.rds", SCHOOL))
}

# Outcomes ----
if (SCHOOL == "primary"){
  questions_level1 <- t(array(c(c("q1", "Mental Health Knowledge", "0-21"),  
                                c("q2", "Subjective Happiness", "1-7"),  
                                c("q3", "Anxiety", "0-18"),  
                                c("q4neg", "Negative Thinking", "0-40"), 
                                c("q4pos", "Positive Thinking", "0-40"),
                                c("q5", "Empathy","0-24"),
                                c("q6", "Self-esteem", "10-40"), 
                                c("q7", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 8)))
  
  questions_level2 <- t(array(c(c("q1", "Mental Health Knowledge", "0-13"),  
                                c("q2", "Subjective Happiness", "4-28"),  
                                c("q3neg", "Negative Affect", "10-50"),  
                                c("q3pos", "Positive Affect", "10-50"),  
                                c("q4a", "Strengths Knowledge", "8-56"),  
                                c("q4b", "Strengths Use", "14-98"),  
                                c("q5neg", "Negative Thinking", "0-40"), 
                                c("q5pos", "Positive Thinking", "0-40"),
                                c("q6", "Empathy","0-24"),
                                c("q7", "Self-esteem", "10-40"), 
                                c("q8", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 11)))
  
  questions_level3 <- t(array(c(c("q1", "Mental Health Knowledge", "0-15"),  
                                c("q2", "Subjective Happiness", "4-28"),  
                                c("q3neg", "Negative Affect", "10-50"),  
                                c("q3pos", "Positive Affect", "10-50"),  
                                c("q4a", "Strengths Knowledge", "8-56"),  
                                c("q4b", "Strengths Use", "14-98"),  
                                c("q5neg", "Negative Thinking", "0-40"), 
                                c("q5pos", "Positive Thinking", "0-40"),
                                c("q6", "Empathy","0-24"),
                                c("q7", "Self-esteem", "10-40"), 
                                c("q8", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 11)))
  
  
} else if(SCHOOL == "secondary") {
  questions_level1 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-20"),  
                                c("q3", "Psychological Distress", "0-36"),  
                                c("q4neg", "Negative Thinking", "0-40"), 
                                c("q4pos", "Positive Thinking", "0-40"),
                                # c("q5a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q5a2", "Life Satisfaction (1-item)", "1-7"),
                                c("q5b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q6", "Empathy", "0-24"), 
                                c("q7", "Gratitude", "6-42")
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 7)))
  questions_level2 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-14"),  
                                c("q3", "Empathy (C-IRI)", "0-88"),  
                                c("q3_PD", "- Personal Distress", "0-28"),
                                c("q3_FS", "- Fantasy", "0-16"),
                                c("q3_ES", "- Empathy", "0-44"),
                                c("q4", "Empathy Quotient", "0-44"),
                                c("q5a", "Emotional Competence", "6-36"),
                                c("q5b", "Behavioural Competence", "6-36"),
                                # c("q6a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q6a2", "Life Satisfaction (1-item)","1-7"),
                                c("q6b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q7", "Psychological Distress", "0-36") 
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-2")
  ), dim = c(3, 10)))
  
  questions_level3 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-14"),  
                                c("q3", "Empathy (C-IRI)", "0-88"),  
                                c("q3_PD", "- Personal Distress", "0-28"),
                                c("q3_FS", "- Fantasy", "0-16"),
                                c("q3_ES", "- Empathy", "0-44"),
                                c("q5a", "Emotional Competence", "6-36"),
                                c("q5b", "Behavioural Competence", "6-36"),
                                # c("q6a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q6a2", "Life Satisfaction (1-item)","1-7"),
                                c("q6b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q7", "Psychological Distress", "0-36") 
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-2")
  ), dim = c(3, 9)))
  
  
} else if(SCHOOL == "primary_selective") {
  questions <- t(array(c(c("q1", "Subjective Happiness", "4-28"),  
                         c("q2neg", "Negative Affect", "10-50"),  
                         c("q2pos", "Positive Affect", "10-50"),  
                         c("q3", "Self-esteem", "10-40"), 
                         c("q4a", "Strengths Knowledge", "8-56"),  
                         c("q4b", "Strengths Use", "14-98"),  
                         c("q5neg", "Negative Thinking", "0-40"), 
                         c("q5pos", "Positive Thinking", "0-40")
  ), dim = c(3, 8)))
  
} else if(SCHOOL == "secondary_selective") {
  
}

# Main functions ----

pre_process <- function(df, both = FALSE){
  df <- as.data.frame(df)
  
  # pick highest N based on outcome variables
  if (is.null(df$level)){
    QUESTIONS <- questions
  } else if (1 %in% unique(df$level)){
    QUESTIONS <- questions_level1
  } else if (2 %in% unique(df$level)){
    QUESTIONS <- questions_level2
  } else if (3 %in% unique(df$level)){
    QUESTIONS <- questions_level3
  }
  question_max_n <- QUESTIONS[1,1]
  n_df <- 0
  for (question in QUESTIONS[,1]){
    n_new <- length(df[which(!(df[, question] %in% NA)), question])
    if (n_new > n_df){      
      n_df <- n_new
      question_max_n <- question
    }
    # print(paste(question, n_new))
  }
  df <- df[which(!(df[, question_max_n] %in% NA)),]
  
  # remove NAs and generate uids
  df <- df %>%
    filter(!is.na(sex) & !is.na(age) & !is.na(intervention) & !is.na(T1) & !is.na(sch))
  df <- df %>%
    mutate(uid = paste(sch, class, student_num, dob, sex))

  df$sex <- as.numeric(df$sex)
  df$gender <- car::recode(df$sex,  "
  1 = 'female';
  2 = 'male'
  ")

  ## examine duplicates
  # df %>%
  #   add_count(sch, class, student_num, dob, T1, sex) %>%
  #   filter(n >= 2) %>% View()

  # remove duplicates
  df <- df[order(df$submitdate, decreasing = FALSE),] # order by submitdate so only earliest observation is kept for duplicates
  df <- distinct(df, sch, class, student_num, dob, T1, sex, .keep_all = TRUE) # keep only first instance of same student & T1 (i.e. remove any repeats)

  df$uid <- as.factor(df$uid)
  levels(df$uid) <- 1:n_distinct(df$uid)

  if (both){
    df <- df %>%
      group_by(uid) %>%
      filter(n() == 2)
  }
  
  return(df)
}

desc_stat <- function(df){
  df <- df %>%
    filter(!is.na(gender) & !is.na(age) & !is.na(intervention) & !is.na(T1) & !is.na(sch)) %>%
    group_by(uid) %>%
    filter(n() == 1 | (n() == 2 & T1 == 0))  # keep T1 if both T0 & T1 avaiable
  
  df$intervention <- ifelse(df$intervention %in% 0, "Control Group", "Intervention Group")
  df$gender <- ifelse(df$sex %in% 2, "Male", "Female")
  
  int_gender <- df %>%
    ungroup() %>%
    group_by(intervention, gender) %>%
    summarise(n = n(), "Mean Age" =  mean(age, na.rm = TRUE), SD = sd(age, na.rm = TRUE), .groups = 'drop') 
  int <- df %>%
    ungroup() %>%
    group_by(intervention) %>%
    summarise(n = n(), "Mean Age" =  mean(age, na.rm = TRUE), SD = sd(age, na.rm = TRUE), .groups = 'drop')
  all <- df %>%
    ungroup() %>%
    summarise(n = n(), "Mean Age" =  mean(age, na.rm = TRUE), SD = sd(age, na.rm = TRUE), .groups = 'drop')
  df <- plyr::rbind.fill(int_gender, int, all)
  
  df <- rbind(df[which(df$intervention %in% "Intervention Group" & df$gender %in% "Male"),],
               df[which(df$intervention %in% "Intervention Group" & df$gender %in% "Female"),],
               df[which(df$intervention %in% "Intervention Group" & df$gender %in% NA),],
               df[which(df$intervention %in% "Control Group" & df$gender %in% "Male"),],
               df[which(df$intervention %in% "Control Group" & df$gender %in% "Female"),],
               df[which(df$intervention %in% "Control Group" & df$gender %in% NA),],
               df[which(df$intervention %in% NA & df$gender %in% NA),]
  )
  df$gender[which(df$intervention %in% "Intervention Group" & df$gender %in% NA)] <- "Sub-total"
  df$gender[which(df$intervention %in% "Control Group" & df$gender %in% NA)] <-  "Sub-total"
  df$gender[which(df$intervention %in% NA & df$gender %in% NA)] <- "Total"
  df$'Mean Age' <- round_format(df$'Mean Age', 2)
  df$SD <- round_format(df$SD, 2)
  names(df)[names(df)=="gender"] <- "Gender"
  names(df)[names(df)=="intervention"] <- "Group"
  names(df)[names(df)=="n"] <- "No. of Participants"
  return(as.data.frame(df))
}

sch_stat<- function(df){
  df$intervention <- ifelse(df$intervention %in% 0, "Control Group", "Intervention Group")
  df$T1 <- ifelse(df$T1 %in% 0, "T0", "T1")
  table <- data.frame(table(df$intervention, df$sch, df$T1))
  names(table)[names(table)=="Var1"] <- "Group"
  names(table)[names(table)=="Var2"] <- "School"
  names(table)[names(table)=="Var3"] <- "T1"
  table <- reshape(data=table,idvar= c("Group", "School"),
          timevar = "T1",
          direction="wide") 
  
  table <- reshape(data=table,idvar= c("School"),
                   timevar = "Group",
                   direction="wide") 
  trycatchNA({
  names(table)[names(table)=="Freq.T0.Control Group"] <- "Control T0"
  names(table)[names(table)=="Freq.T1.Control Group"] <- "Control T1"
  names(table)[names(table)=="Freq.T0.Intervention Group"] <- "Intervention T0"
  names(table)[names(table)=="Freq.T1.Intervention Group"] <- "Intervention T1"})
  table <- arrange(table, School)
  table[nrow(table)+1,2:ncol(table)] <- colSums(table[2:ncol(table)])
  table$School <- as.character(table$School)
  table[nrow(table), "School"] <- "Total"
  return(table)
}

full_results <- function(df, questions){
  table <- data.frame(matrix(ncol = 14,  nrow = 0))
  colnames(table)  <-  c("Outcome Measure", "Range", "N", "Intervention Group T0", "Intervention Group T1", "Post minus Pre", 
                         "Control Group T0", "Control Group T1", "Post minus Pre", "MLM Group at T0", "MLM Group x Time", "OLS Group x Time",
                         "Adjusted MLM Group x Time", "Adjusted OLS Group x Time")
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    table[row_count, 2] <- questions[which(questions==dep_var),3]
    
    # intervention group & control group
    # MLM results
    iferror(
      fit <- eval_(
        "lmer(", dep_var, " ~ 1+T1*intervention+ (1 | sch/uid),  REML = TRUE, data =  df, control=lmerControl(optimizer='nloptwrap'))") # use nloptwrap or bobyqa instead of default
      , {iferror(
        fit <- eval_(
          "lmer(", dep_var, " ~ 1+T1*intervention+ (1 | uid),  REML = TRUE, data =  df, control=lmerControl(optimizer='nloptwrap'))") # use nloptwrap or bobyqa instead of default
        , {row_count <- row_count + 2 ; next})})
    
    N <- summary(fit)$ngrps["uid:sch"]
    contrast_matrix <-  matrix(c(0, 0, 0, 0), 1)
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "(Intercept)")] <- 1
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "intervention")] <- 1
    outputs <- glht(fit, linfct = contrast_matrix)  %>% summary()
    int_T0 <- outputs$test$coefficients[1] 
    int_T0_se <- outputs$test$sigma[1]
    
    contrast_matrix <-  matrix(c(0, 0, 0, 0), 1)
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "(Intercept)")] <- 1
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "intervention")] <- 1
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "T1")] <- 1
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "T1:intervention")] <- 1
    outputs <- glht(fit, linfct = contrast_matrix)  %>% summary()
    int_T1 <- outputs$test$coefficients[1]
    int_T1_se <- outputs$test$sigma[1]
    
    # # alternative manual delta method 
    # int_T1 <- summary(fit)$coef["(Intercept)", 1] +
    #   summary(fit)$coef["intervention", 1] +
    #   summary(fit)$coef["T1", 1] +
    #   summary(fit)$coef["T1:intervention", 1] 
    # int_T1_se <- sqrt(t(contrast_matrix[1,]) %*% vcov(fit) %*% contrast_matrix[1,])[1]
    
    outputs <- glht(fit, linfct = "T1 + T1:intervention = 0")  %>% summary()
    int_dif <- outputs$test$coefficients[1]
    int_dif_se <- outputs$test$sigma[1]
    int_dif_p <- outputs$test$pvalues[1]
    
    ctrl_T0 <- iferror(summary(fit)$coef["(Intercept)", 1], NA)
    ctrl_T0_se <- iferror(summary(fit)$coef["(Intercept)", 2], NA)
    
    ## somehow glht doesn't produce any results for this while it works for (Intercept)+intervention above
    # contrast_matrix <-  matrix(c(0, 0, 0, 0, 0, 0), 1)
    # contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "(Intercept)")] <- 1
    # contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "T1")] <- 1
    # outputs <- glht(fit, linfct = contrast_matrix) 
    # ctrl_T1 <- outputs$test$coefficients[1]
    # ctrl_T1_se <- outputs$test$sigma[1]
    
    # replace the above glht with the manual delta method
    ctrl_T1 <- summary(fit)$coef["(Intercept)", 1] + summary(fit)$coef["T1", 1]
    vcov_A <- vcov(fit)["(Intercept)", "(Intercept)"]
    vcov_B <- vcov(fit)["T1", "T1"]
    vcov_AB <- vcov(fit)["(Intercept)", "T1"]
    ctrl_T1_se <-  sqrt(vcov_A + vcov_B + 2* vcov_AB)
    
    ctrl_T1 <- summary(fit)$coef["(Intercept)", 1] + summary(fit)$coef["T1", 1]
    
    outputs <- glht(fit, linfct = "T1 = 0")  %>% summary()
    ctrl_dif <- outputs$test$coefficients[1]
    ctrl_dif_se <- outputs$test$sigma[1]
    ctrl_dif_p <- outputs$test$pvalues[1]
    
    table[row_count, 3] <-  N
    table[row_count, 4] <-  iferror(round_format(int_T0, 2), NA)
    table[row_count, 5] <-  iferror(round_format(int_T1, 2), NA)
    table[row_count, 6] <-  iferror(starred_p(int_dif_p, 2, int_dif), NA)
    table[row_count, 7] <-  iferror(round_format(ctrl_T0, 2), NA)
    table[row_count, 8] <-  iferror(round_format(ctrl_T1, 2), NA)
    table[row_count, 9] <-  iferror(starred_p(ctrl_dif_p, 2, ctrl_dif), NA)
    
    table[row_count + 1, 4] <-  iferror(paste0("(", round_format(int_T0_se, 2), ")"), NA)
    table[row_count + 1, 5] <-  iferror(paste0("(", round_format(int_T1_se, 2), ")"), NA)
    table[row_count + 1, 6] <-  iferror(paste0("(", round_format(int_dif_se, 2), ")"), NA)
    table[row_count + 1, 7] <-  iferror(paste0("(", round_format(ctrl_T0_se, 2), ")"), NA)
    table[row_count + 1, 8] <-  iferror(paste0("(", round_format(ctrl_T1_se, 2), ")"), NA)
    table[row_count + 1, 9] <-  iferror(paste0("(", round_format(ctrl_dif_se, 2), ")"), NA)
    
    beta <- iferror(summary(fit)$coef["intervention", 1], NA)
    p_value <- iferror(summary(fit)$coef["intervention", 5], NA)
    se <- iferror(summary(fit)$coef["intervention", 2], NA)
    
    table[row_count, 10] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 10] <-  paste0("(", round_format(se, 2), ")")
    
    beta <- iferror(summary(fit)$coef["T1:intervention", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1:intervention", 5], NA)
    se <- iferror(summary(fit)$coef["T1:intervention", 2], NA)
    
    table[row_count, 11] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 11] <-  paste0("(", round_format(se, 2), ")")
    
    # adjusted MLM
    fit <- update(fit, .~. +age+gender)
    beta <- iferror(summary(fit)$coef["T1:intervention", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1:intervention", 5], NA)
    se <- iferror(summary(fit)$coef["T1:intervention", 2], NA)
    
    table[row_count, 13] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 13] <-  paste0("(", round_format(se, 2), ")")
    
    # OLS
    iferror(
      fit <- eval_(
        "lm(", dep_var, " ~ 1+T1*intervention, data =  df)")
      , {row_count <- row_count + 2 ; next})
    
    beta <- iferror(summary(fit)$coef["T1:intervention", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1:intervention", 4], NA)
    se <- iferror(summary(fit)$coef["T1:intervention", 2], NA)
    
    table[row_count, 12] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 12] <-  paste0("(", round_format(se, 2), ")")
    
    # adjusted OLS
    fit <- update(fit, .~. +age+gender)    
    beta <- iferror(summary(fit)$coef["T1:intervention", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1:intervention", 4], NA)
    se <- iferror(summary(fit)$coef["T1:intervention", 2], NA)
    
    table[row_count, 14] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 14] <-  paste0("(", round_format(se, 2), ")")
    
    row_count <- row_count + 2
  }
  return(table)
}

int_results <- function(df, questions){
  table <- data.frame(matrix(ncol = 9,  nrow = 0))
  colnames(table)  <-  c("Outcome Measure", "Range", "N", "Intervention Group T0", "Intervention Group T1", "MLM Post minus Pre", "OLS Post minus Pre",  "Adjusted MLM Post minus Pre", "Adjusted OLS Post minus Pre")
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    table[row_count, 2] <- questions[which(questions==dep_var),3]
    
    # MLM results
    iferror(
      fit <- eval_(
        "lmer(", dep_var, " ~ 1+T1+ (1 | sch/uid),  REML = TRUE, data =  df, control=lmerControl(optimizer='nloptwrap'))") # use nloptwrap or bobyqa instead of default
      , {iferror(
        fit <- eval_(
          "lmer(", dep_var, " ~ 1+T1+ (1 | uid),  REML = TRUE, data =  df, control=lmerControl(optimizer='nloptwrap'))") # use nloptwrap or bobyqa instead of default
        , {row_count <- row_count + 2 ; next})})
    
    N <- summary(fit)$ngrps["uid:sch"]
    contrast_matrix <-  matrix(c(0, 0), 1)
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "(Intercept)")] <- 1
    outputs <- glht(fit, linfct = contrast_matrix)  %>% summary()
    int_T0 <- outputs$test$coefficients[1] 
    int_T0_se <- outputs$test$sigma[1]
    
    contrast_matrix <-  matrix(c(0, 0), 1)
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "(Intercept)")] <- 1
    contrast_matrix[which(summary(fit)$coef %>% rownames() %in% "T1")] <- 1
    outputs <- glht(fit, linfct = contrast_matrix)  %>% summary()
    int_T1 <- outputs$test$coefficients[1]
    int_T1_se <- outputs$test$sigma[1]
    
    # # alternative manual delta method 
    # int_T1 <- summary(fit)$coef["(Intercept)", 1] +
    #   summary(fit)$coef["intervention", 1] +
    #   summary(fit)$coef["T1", 1] +
    #   summary(fit)$coef["T1:intervention", 1] 
    # int_T1_se <- sqrt(t(contrast_matrix[1,]) %*% vcov(fit) %*% contrast_matrix[1,])[1]
    
    outputs <- glht(fit, linfct = "T1 = 0")  %>% summary()
    int_dif <- outputs$test$coefficients[1]
    int_dif_se <- outputs$test$sigma[1]
    int_dif_p <- outputs$test$pvalues[1]
    
    table[row_count, 3] <-  N
    table[row_count, 4] <-  iferror(round_format(int_T0, 2), NA)
    table[row_count, 5] <-  iferror(round_format(int_T1, 2), NA)
    table[row_count, 6] <-  iferror(starred_p(int_dif_p, 2, int_dif), NA)
    
    table[row_count + 1, 4] <-  iferror(paste0("(", round_format(int_T0_se, 2), ")"), NA)
    table[row_count + 1, 5] <-  iferror(paste0("(", round_format(int_T1_se, 2), ")"), NA)
    table[row_count + 1, 6] <-  iferror(paste0("(", round_format(int_dif_se, 2), ")"), NA)
    
    # adjusted MLM
    fit <- update(fit, .~. +age+gender)
    outputs <- glht(fit, linfct = "T1 = 0")  %>% summary()
    int_dif <- outputs$test$coefficients[1]
    int_dif_se <- outputs$test$sigma[1]
    int_dif_p <- outputs$test$pvalues[1]
    
    table[row_count, 8] <-  iferror(starred_p(int_dif_p, 2, int_dif), NA)
    table[row_count + 1, 8] <-  iferror(paste0("(", round_format(int_dif_se, 2), ")"), NA)
    
    # OLS
    fit <- eval_(
        "lm(", dep_var, " ~ 1+T1, data =  df)")
    
    beta <- iferror(summary(fit)$coef["T1", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1", 4], NA)
    se <- iferror(summary(fit)$coef["T1", 2], NA)
    
    table[row_count, 7] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 7] <-  paste0("(", round_format(se, 2), ")")
    
    # adjusted OLS
    fit <- update(fit, .~. +age+gender)
    beta <- iferror(summary(fit)$coef["T1", 1], NA)
    p_value <- iferror(summary(fit)$coef["T1", 4], NA)
    se <- iferror(summary(fit)$coef["T1", 2], NA)
    
    table[row_count, 9] <-  starred_p(p_value, 2, beta)
    table[row_count + 1, 9] <-  paste0("(", round_format(se, 2), ")")
    
    row_count <- row_count + 2
  }
  return(table)
}

# Generate results ----
# level 1 results 
if (SCHOOL == "primary"){
  df <- df1
  df <- pre_process(df)
  level1_table <- int_results(df, questions_level1)
  level1_desc <- desc_stat(df)
  level1_sch <- sch_stat(df)
  df <- pre_process(df, both = TRUE)
  level1_table_both <- int_results(df, questions_level1)
  level1_desc_both <- desc_stat(df)
  level1_sch_both <- sch_stat(df)
} else if (SCHOOL == "secondary") {
  df <- df1
  df <- pre_process(df)
  level1_table <- full_results(df, questions_level1)
  level1_desc <- desc_stat(df)
  level1_sch <- sch_stat(df)
  df <- pre_process(df, both = TRUE)
  level1_table_both <- full_results(df, questions_level1)
  level1_desc_both <- desc_stat(df)
  level1_sch_both <- sch_stat(df)
} 

# level 2 & 3 results
if (SCHOOL == "primary_selective"){
  df <- pre_process(df, both = TRUE)
  selective_table_both <- int_results(df, questions)
  selective_desc_both <- desc_stat(df)
  selective_sch_both <- sch_stat(df)
} else {
  df <- df2
  df <- pre_process(df)
  level2_table <- int_results(df, questions_level2)
  level2_desc <- desc_stat(df)
  level2_sch <- sch_stat(df)
  df <- pre_process(df, both = TRUE)
  level2_table_both <- int_results(df, questions_level2)
  level2_desc_both <- desc_stat(df)
  level2_sch_both <- sch_stat(df)
}


if (SCHOOL == "primary"){
  df <- df3
  df <- pre_process(df)
  level3_table <- int_results(df, questions_level3)
  level3_desc <- desc_stat(df)
  level3_sch <- sch_stat(df)
  df <- pre_process(df, both = TRUE)
  level3_table_both <- int_results(df, questions_level3)
  level3_desc_both <- desc_stat(df)
  level3_sch_both <- sch_stat(df)
} else if (SCHOOL == "secondary") {
  df <- plyr::rbind.fill(df2, df3)
  df <- pre_process(df)
  level23_table <- int_results(df, questions_level2)
  level23_desc <- desc_stat(df)
  df <- pre_process(df, both = TRUE)
  
  df <- df3
  df <- pre_process(df)
  level3_desc <- desc_stat(df)
  level3_sch <- sch_stat(df)
}

# install rowr package
# install.packages("https://cran.r-project.org/src/contrib/Archive/rowr/rowr_1.1.3.tar.gz", repo=NULL, type="source", dependencies = TRUE)

if (SCHOOL == "primary_selective"){
  selective_both <- rowr::cbind.fill(selective_desc_both, selective_sch_both, selective_table_both, fill = NA)
  colnames(selective_both) <- c(colnames(selective_desc_both), colnames(selective_sch_both), colnames(selective_table_both))
  
  rm(selective_desc_both, selective_sch_both, selective_table_both)
  
  setwd(sprintf("~%s/qtn/qtn2019-20/%s/results", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
  write_excel(sprintf("qtn1920_%s_results.xlsx", SCHOOL), 
              selective_both)
  
} else {
  level1 <- rowr::cbind.fill(level1_desc, level1_sch, level1_table, fill = NA)
  colnames(level1) <- c(colnames(level1_desc), colnames(level1_sch), colnames(level1_table))
  
  level1_both <- rowr::cbind.fill(level1_desc_both, level1_sch_both, level1_table_both, fill = NA)
  colnames(level1_both) <- c(colnames(level1_desc_both), colnames(level1_sch_both), colnames(level1_table_both))
  
  level2 <- rowr::cbind.fill(level2_desc, level2_sch, level2_table, fill = NA)
  colnames(level2) <- c(colnames(level2_desc), colnames(level2_sch), colnames(level2_table))
  
  level2_both <- rowr::cbind.fill(level2_desc_both, level2_sch_both, level2_table_both, fill = NA)
  colnames(level2_both) <- c(colnames(level2_desc_both), colnames(level2_sch_both), colnames(level2_table_both))
}

if (SCHOOL == "primary"){
  level3 <- rowr::cbind.fill(level3_desc, level3_sch, level3_table, fill = NA)
  colnames(level3) <- c(colnames(level3_desc), colnames(level3_sch), colnames(level3_table))
  
  level3_both <- rowr::cbind.fill(level3_desc_both, level3_sch_both, level3_table_both, fill = NA)
  colnames(level3_both) <- c(colnames(level3_desc_both), colnames(level3_sch_both), colnames(level3_table_both))
  
  rm(level1_desc, level1_sch, level1_table, 
     level1_desc_both, level1_sch_both, level1_table_both, 
     level2_desc, level2_sch, level2_table, 
     level2_desc_both, level2_sch_both, level2_table_both, 
     level3_desc, level3_sch, level3_table, 
     level3_desc_both, level3_sch_both, level3_table_both)
  
  setwd(sprintf("~%s/qtn/qtn2019-20/%s/results", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
  write_excel(sprintf("qtn1920_%s_results.xlsx", substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)),
              level1, level1_both, level2, level2_both, level3, level3_both)
  
} else if (SCHOOL == "secondary") {
  level23 <- rowr::cbind.fill(level23_desc, level23_table, fill = NA)
  colnames(level23) <- c(colnames(level23_desc), colnames(level23_table))
  
  level3 <- rowr::cbind.fill(level3_desc, level3_sch, fill = NA)
  colnames(level3) <- c(colnames(level3_desc), colnames(level3_sch))
  
  rm(level1_desc, level1_sch, level1_table, 
     level1_desc_both, level1_sch_both, level1_table_both, 
     level2_desc, level2_sch, level2_table, 
     level2_desc_both, level2_sch_both, level2_table_both, 
     level23_desc, level23_table,
     level3_desc, level3_sch)
  
  setwd(sprintf("~%s/qtn/qtn2019-20/%s/results", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
  write_excel(sprintf("qtn1920_%s_results.xlsx", substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)),
              level1, level1_both, level2, level2_both, level23, level3)
}

# # Other stuff 
# df %>%  group_by(intervention, gender) %>% filter(year==2019 & sch==11) %>% filter(T1==1 & !is.na(gender)) %>% summarise(n=n(), mean=mean(age), sd=sd(age)) %>% as.data.frame() %>% View(.)
