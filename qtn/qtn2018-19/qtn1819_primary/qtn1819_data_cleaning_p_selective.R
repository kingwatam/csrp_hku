rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary/qtn1819_primary_selective", setpath))
library(dplyr) # pipe

df <- haven::read_sav("survey_345439_SPSS.sav")
intctrl <- openxlsx::read.xlsx("intervention_control.xlsx")
df_S03 <- openxlsx::read.xlsx("QTN_Pri_Selecitve_1819_KHP_renamed.xlsx")

df_S03$Q03 <-  as.Date(paste(df_S03$`Date.of.birth.(month)`,
                             df_S03$`Date.of.birth.(date)`,
                             df_S03$`Date.of.birth.(year)`, sep = "/"), format = "%m/%d/%Y") # DoB

df_S03$submitdate <-  as.Date(paste(df_S03$`Ass1-Questionnaire.date.(month)`,
                                     df_S03$`Ass1-Questionnaire.date.(day)`,
                                     df_S03$`Ass1-Questionnaire.date.(year)`, sep = "/"), format = "%m/%d/%Y") # submitedate

df_S03$submitdate[is.na(df_S03$submitdate) & df_S03$Q01=="6D"] <- as.Date("2019-01-25", format = "%Y-%m-%d") # Add info for NA submitdate
df_S03$submitdate[is.na(df_S03$submitdate) & df_S03$Q01=="6A"] <- as.Date("2019-01-28", format = "%Y-%m-%d")

df_S03[df_S03==98] <- NA
df_S03[df_S03==99] <- NA

df_S03 <- df_S03[,-(7:12)] # remove unused date columns

df <- plyr::rbind.fill(df, df_S03)

# Outcome scoring ----
df %>% select(starts_with("AB")) %>% colnames(.) -> qA 
df %>% select(starts_with("P2")) %>% colnames(.) -> qB # Positive & Negative Affect (PANAS) range()
df %>% select(starts_with("E0")) %>% colnames(.) -> qC # Self-esteem (RSES) 
df %>% select(starts_with("P4")) %>% colnames(.) -> qD
qD1 <- qD[1:8]
qD2 <- qD[9:22]
df %>% select(starts_with("C0")) %>% colnames(.) -> qE

questions <- c(qA, qB, qC, qD, qE)

reverse_qA <- qA[c(4)] 
df[reverse_qA] <- (df[reverse_qA]-8)*-1 # reverse (1:7) to (7:1)

reverse_qB <- qB[c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)] # Negative Affect questions

reverse_qC <- qC[c(2, 5, 6, 8, 9)]
df[reverse_qC] <- (df[reverse_qC]-5)*-1 # reverse (1:4) to (4:1)

reverse_qD1 <- qD1[c(2)]
df[reverse_qD1] <- (df[reverse_qD1]-8)*-1 # reverse (1:7) to (7:1)

reverse_qE <- qE[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking

df <- df %>% 
  mutate(q1 = rowSums(.[qA], na.rm = FALSE), # Subjective Happiness (SHS) range(4-28)
         q2neg = rowSums(.[reverse_qB], na.rm = FALSE),# Negative Affect (PANAS) range(10-50)
         q2pos = rowSums(.[qB[!(qB %in% reverse_qB)]], na.rm = FALSE), # Positive Affect (PANAS) range(10-50)
         q3 = rowSums(.[qC], na.rm = FALSE),# Self-esteem (RSES) range(10-40)
         q4a = rowSums(.[qD1], na.rm = FALSE), # Strengths Knowledge Scale (SKS) range(8-56)
         q4b = rowSums(.[qD2], na.rm = FALSE), # Strengths Use Scale (SUS) range(14-98)
         q5neg = rowSums(.[reverse_qE], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q5pos = rowSums(.[qE[!(qE %in% reverse_qE)]], na.rm = FALSE))  # Positive Thinking (CATS-N/P) range(0-40)

# Data cleaning ----
names(df)[names(df)=="Q00"] <- "sch"

df$sch[df$ipaddr=="115.160.146.146"] <- "S10"

df$sch <- as.integer(as.vector(substr(df$sch,2,3)))

names(df)[names(df)=="Q01"] <- "class"
df$grade <- as.integer(as.vector(substr(df$class,1,1)))
names(df)[names(df)=="Q02"] <- "student_num"

# likely entry errors based on name list, these changes are required to get int/ctrl information
df$student_num[df$sch==10 & df$class=="5C" & df$student_num==8] <- 9
df$student_num[df$sch==10 & df$class=="5C" & df$student_num==21] <- 22
df$student_num[df$sch==10 & df$class=="5C" & df$student_num==23] <- 24

names(df)[names(df)=="Q03"] <- "dob"

names(df)[names(df)=="Q04"] <- "sex" # 1=female, 2=male
# names(df)[names(df)=="GROUPSEL"] <- "intervention"

df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

df$year <- as.numeric(format(as.Date(df$submitdate), "%Y"))
df$month <- as.numeric(format(as.Date(df$submitdate), "%m"))
df$day <- as.numeric(format(as.Date(df$submitdate), "%d"))

# likely entry errors on DoB
df$dob[df$sch==2 & df$class=="5A" & df$student_num==13 ] <- as.Date("2008-01-15")

df_original <- df

df_S03 <- df[df$sch==3,]
df <- df[df$sch!=3,]

df$id2<- paste(df$sch, df$class, df$student_num,  # id2 is used to match students
               df$sex, df$dob,
               sep="_")
dup_list<-df[which(duplicated(df$id2)==TRUE),]$id2

df_unknown <- df %>% 
  filter(!(.$id2 %in% dup_list))

df <- df %>% 
  filter(.$id2 %in% dup_list) %>% 
  group_by(sch, class, student_num) %>%
  mutate(T1 = order(order(submitdate))) %>% # create new variable T1 by ordering submitdate based on groups
  ungroup()

df_unknown <- plyr::rbind.fill(df_unknown, df[df$id %in% c(522, 567, 645, 650, 566, 570),])
df_unknown$T1 <- NULL # remove T1 column which is from the last line of code (n=7)

# xlsx::write.xlsx(df, "mydata.xlsx")
df <- df[!(df$id %in% c(522, 567, 645, 650, 566, 570)),] # remove duplicates based on submitdates (there should be sufficient time gap between T0 & T1 submitdate)
df$class[df$id==988] <- "5A" # based on their DoB, likely

df$T1[df$T1>=2] <- 2
df$T1 <- plyr::mapvalues(df$T1, c(1,2), c(0,1))

# Schhool (first T1 date in 2019) - 4 (11 June), 5 (24 May), 8 (12 June), 9 (23 May), 11 (17 June), 13 (11 July), 14 (8 July), 16 (27 June)
# check if using "2019-05-23" as cut-off point for T1 works for all schools
df_unknown$submitdatediff <- as.Date(df_unknown$submitdate) - as.Date("2019-05-23")
df_unknown$T1 <- ifelse(df_unknown$submitdatediff>=0, 1, 0)
df <- plyr::rbind.fill(df, df_unknown)
rm(df_unknown)

df <- plyr::rbind.fill(df, df_S03)
rm(df_S03)
df$T1[df$sch==3] <- df$"Pre/Post.(1=Pre;.2=Post)"[df$sch==3] - 1
df$"Pre/Post.(1=Pre;.2=Post)" <- NULL

df[ ,c('year', 'month', 'day', 'id2', 'submitdatediff', 
       'id', 'token', 'lastpage', 'startlanguage', 'startdate', 'datestamp', 'ipaddr', 'refurl', 'Q05'
       )] <- list(NULL) # remove columns

# adding back intctrl information ----
# df$sch_class_num <- with(df, paste(sch, class, student_num, sep="_"))
# intctrl$sch_class_num <- with(intctrl, paste(sch, class, student_num, sep="_"))

# similar to vlookup function in excel
df <- merge(df, unique(intctrl)[, c("sch", "class", "student_num", "intervention")], by=c("sch", "class", "student_num"), all.x = TRUE)
names(df)[names(df)=="intervention.y"] <- "intervention" 
df$intervention.x <- NULL

# probably best not to include these (because we aren't certain if they are part of int or ctrl group)
df$intervention[df$sch==5 & df$class!="6E"] <- 1 # no student number information available for sch 5, the class and sex (from names) seem to match up well with data
df$intervention[df$sch==5 & df$class=="6E"] <- 0 # only student not in the list
# df$intervention[df$sch==14 & is.na(df$intervention)] <- 1 # these don't appear in the list (not even among the control), they only have one response (mostly T1)
# df <- df[!(df$sch==9 & df$class=="6A" & df$student_num==6),] # possible outlier in their T0 response? q2pos value is at minimum (i.e. 10)

df$intervention[df$sch==3] <- (df$"Intervention/Control.(1=int;.2=.control)"[df$sch==3]  - 2) * -1
df$"Intervention/Control.(1=int;.2=.control)" <- NULL

# df$intervention[is.na(df$intervention)] <- 1 # assume the rest is intervention group (although we don't know which ones are which group)
df <- df[!is.na(df$intervention),] # remove NA int rows

df$control <- ifelse(df$intervention, 0, 1)

# output dataframe ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary", setpath))
saveRDS(df, file = "qtn1819_primary_selective_long.rds")
# write_excel("qtn1819_primary_selective_long.xlsx", df[])

df_pre <- df[df$T1==0,]
df_pre$T1 <- NULL
df_post <- df[df$T1==1,]
df_post$T1 <- NULL

dfwide <- merge(df_pre, df_post, by.x = c("sch", "class", "student_num", 
                                         "dob", "sex",
                                        "control", "intervention",
                                        "grade"),
              by.y = c("sch", "class", "student_num", 
                        "dob", "sex", 
                       "control", "intervention",
                       "grade"),
              all.x=TRUE, all.y = TRUE, suffixes = c(".0", ".1"))

dfwide_both <- merge(df_pre, df_post, by.x = c("sch", "class", "student_num", 
                                         "dob", "sex",
                                        "control", "intervention",
                                        "grade"),
              by.y = c("sch", "class", "student_num", 
                        "dob", "sex", 
                       "control", "intervention",
                       "grade"),
              all.x=FALSE, all.y = FALSE, suffixes = c(".0", ".1"))

for (q in c('q1', 'q2neg', 'q2pos', 'q3', 'q4a', 'q4b', 'q5neg', 'q5pos')){
  eval_(sprintf("dfwide$%s <- (dfwide$%s - dfwide$%s)", q, paste0(q, ".1"),  paste0(q, ".0")))
}

dfwide <- dfwide %>% 
  # rowwise() %>% ## a slower method, pmax() is changed to max() with rowwise()
  mutate(age=pmax(age.0, age.1, na.rm=TRUE))

pre_subquestions <- paste0(questions, ".0")
post_subquestions <- paste0(questions, ".1")
subquestions <- c(pre_subquestions, post_subquestions)

other_variables <- names(dfwide)[!(names(dfwide) %in% subquestions)]

dfwide <- dfwide[, c(other_variables,
                     subquestions)] # names(df)[!(names(df) %in% questions)]

df$age.0 <- NULL
df$age.1 <- NULL

for (q in questions){
  eval_(sprintf("dfwide$%s <- (dfwide$%s - dfwide$%s)", q, paste0(q, ".1"),  paste0(q, ".0")))
}

# dfwide <- dfwide %>% mutate(dif = as.numeric(floor(as.Date(submitdate.1)-as.Date(submitdate.0))))
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary/qtn1819_primary_selective", setpath))
haven::write_sav(dfwide, "qtn1819_primary_selective_wide.sav")

dfwide$T0 <- ifelse(!is.na(dfwide$age.0), 1, NA)
dfwide$T1 <- ifelse(!is.na(dfwide$age.1), 1, NA)

dfwide %>% select(starts_with("AB")) %>% colnames(.) -> qA 
dfwide %>% select(starts_with("P2")) %>% colnames(.) -> qB 
dfwide %>% select(starts_with("E0")) %>% colnames(.) -> qC
dfwide %>% select(starts_with("P4")) %>% colnames(.) -> qD
dfwide %>% select(starts_with("C0")) %>% colnames(.) -> qE

questions_wide <- c(qA, qB, qC, qD, qE)

dfwide <- dfwide[, !(names(dfwide) %in% questions_wide)]

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_primary", setpath))
saveRDS(dfwide, file = "qtn1819_primary_selective_wide.rds")