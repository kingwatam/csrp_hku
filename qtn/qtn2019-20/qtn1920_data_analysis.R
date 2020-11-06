rm(list=ls())
graphics.off()
source("helper_functions.R")
setpath <- "/MEGAsync/Work/RA HKU/CSRP"

library(dplyr)

setwd(sprintf("~%s/qtn/qtn2018-19", setpath))
import_func('qtn1819_data_analysis.R')

SCHOOL <- array(c(c("primary", "secondary"),                      # switch between universal primary schools [1,1] & universal secondary schools [2,1]
                  c("primary_selective", "secondary_selective")), # switch between selective primary schools [1,2] & selective secondary schools [2,2]
                dim=c(2,2))                                         [2,1] # <- switch here

setwd(sprintf("~%s/qtn/qtn2019-20/qtn1920_%s", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
df <- readRDS(sprintf("qtn1920_%s_long.rds", SCHOOL))

# legacy code ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_%s", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))
df1819 <- readRDS(sprintf("qtn1819_%s_long.rds", SCHOOL))

setwd(sprintf("~%s/qtn/qtn2019-20/%s", setpath, substring(SCHOOL, 1, regexpr("ary", SCHOOL)+2)))

df <- readRDS(sprintf("qtn1920_%s_long.rds", SCHOOL))

df <- df %>%
  mutate(uid = paste(sch, class, student_num, dob, sex))  # create unique IDs from students
df <- df[order(df$submitdate, decreasing = FALSE),] # order by submitdate so only earliest observation is kept for duplicates
df <- df %>% filter(sch == 11) %>% distinct(uid, .keep_all = TRUE) # remove duplicates for sch 11 (there is no pre-test, so everyone should only have one submission)
# df %>%
#   summarize(unique_id = n_distinct(uid))
df$uid <- as.factor(df$uid)
levels(df$uid) <- 1:n_distinct(df$uid)

df1819$year <- 2018
df$year <- 2019
# df <- plyr::rbind.fill(df, df1819[which(df1819$sch==11),])

df$sex <- as.numeric(df$sex)
df$gender <- car::recode(df$sex,  "
1 = 'female';
2 = 'male'
")

{
  if (SCHOOL == "primary"){

  } else if(SCHOOL == "secondary") {
    SCHOOL_N <- 12
    DECIMAL_PLACES <- 2
    questions <- t(array(c(c("q1_2", "Mental Health Knowledge"),  
                           c("q3", "Psychological Distress (GHQ)"),  
                           c("q4neg", "Negative Thinking"), 
                           c("q4pos", "Positive Thinking"), 
                           # c("q5a", "Life Satisfaction (SLSS)"),
                           # c("q5a2", "Life Satisfaction"),
                           c("q5b", "Life Satisfaction (BMSLSS)"),
                           c("q6", "Empathy: Perspective Taking (C-IRI)"), 
                           c("q7", "Gratitude (GQ-6)"), 
                           c("q8a", "Compassion (CS)"), 
                           c("q8b", "Self-compassion (SCS)"), 
                           c("q9a_b", "Help-seeking"), 
                           c("q9c", "Prejudice (PPMI)")), dim = c(2,11)))
    full_analysis_list<-c(13) # place holder
    int_t0_t1_list<-c(13) # place holder
    int_ctrl_list<-c(11)
    int_t0_list<-c(13) # place holder
  } else if(SCHOOL == "primary_selective") {

  } else if(SCHOOL == "secondary_selective") {

  }
}

df <- df[complete.cases(df)]
int_ctrl <- q_loop(df[which(df$sch==11),], type = "int_ctrl", wide_format = FALSE)

df %>%  group_by(intervention, gender) %>% filter(year==2019 & sch==11) %>% filter(T1==1 & !is.na(gender)) %>% summarise(n=n(), mean=mean(age), sd=sd(age)) %>% as.data.frame() %>% View(.)