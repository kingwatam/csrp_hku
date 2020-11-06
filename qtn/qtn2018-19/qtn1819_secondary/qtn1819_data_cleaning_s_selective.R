rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary/qtn1819_secondary_selective", setpath))
library(dplyr) # pipe

dfpre <- haven::read_sav("survey_883541_SPSS_pretest.sav")
dfpost <- haven::read_sav("survey_883542_SPSS_protest.sav")
dfpre$T1 <- 0
dfpost$T1 <- 1
df <- rbind(dfpre, dfpost)
rm(dfpre, dfpost)

temp = list.files(pattern="*.xlsx")
for (i in 1:length(temp)){
  assign(paste0(substring(temp[i], regexpr("dataentry_", temp[i])+10, regexpr("\\.xlsx", temp[i])-1), "_pre"), openxlsx::read.xlsx(temp[i], sheet = "Pre"))
  assign(paste0(substring(temp[i], regexpr("dataentry_", temp[i])+10, regexpr("\\.xlsx", temp[i])-1), "_post"), openxlsx::read.xlsx(temp[i], sheet = "Post"))
} 

# excel files cleaning ----
for (df_name in ls(pattern="Pre_student")){
  assign(substring(df_name, 13, nchar(df_name)), get(df_name))
}
rm(list=ls(pattern="Pre_student"))

for (df_name in ls(pattern="Pre\\+Post_student")){
  assign(paste0(substring(df_name, 18, nchar(df_name)), "2"), get(df_name))
}
rm(list=ls(pattern="Pre\\+Post_student"))

LWL_updated_post2 <- `LWL-updated_post2`
LWL_updated_pre2 <- `LWL-updated_pre2`
student_and_teacher_LSK_post <- `student and teacher_LSK_post`
student_and_teacher_LSK_pre <- `student and teacher_LSK_pre` 
rm(`LWL-updated_post2`, `LWL-updated_pre2`, `student and teacher_LSK_post`,  `student and teacher_LSK_pre`)

student_and_teacher_LSK_pre <- student_and_teacher_LSK_pre[2:nrow(student_and_teacher_LSK_pre),] # example row

for (df_name in ls()){
  if (is.data.frame(get(df_name))){
    eval_(sprintf("%s[, colSums(is.na(%s)) == nrow(%s)] <- NULL", df_name, df_name, df_name))# remove empty columns with all NAs
  }
}

# change dataframe names by creating new ones, remove original ones
assign("MKES_post", mkes_post2)
rm(mkes_post2)
assign("MKES_pre", mkes_pre2)
rm(mkes_pre2)
assign("KTLS_post", KTLS_post2)
rm(KTLS_post2)
assign("KTLS_pre", KTLS_pre2)
rm(KTLS_pre2)

# focus group answers only - KTLS_pre, LHK_post, LSK_post
# focus group answers in addition LWL_updated_post2, MKES_post
# focus_group <- plyr::rbind.fill(KTLS_pre, LHK_post, LSK_post)
rm(KTLS_pre, LHK_post, LSK_post)

# duplicated MKES_pre data = LHK_pre, LSK_myron_pre, LSK_pre, MKES_pre
rm(LHK_pre, LSK_myron_pre, LSK_pre)
# duplicated MKES_post data = MKES_post, KTLS_post
rm(KTLS_post)
# duplicated LSK_post data = LSK_post, LSK_myron_post
rm(LSK_myron_post)
# duplicated LWL_updated_pre2 = LWL_pre2
rm(LWL_pre2)
# duplicated LWL_updated_post2 = LWL_post2
rm(LWL_post2)
# LWL_pre is just an example
rm(LWL_pre)

MKES <- plyr::rbind.fill(MKES_pre, MKES_post)
rm(MKES_pre,MKES_post)
MKES <- MKES[!is.na(MKES$`Ass.P1-Q1.(0-4)`),]  # remove rows with NA answers

LWL_post <- LWL_post[!is.na(LWL_post$`Ass.P1-Q1.(0-4)`),]  # remove rows with NA answers
KTLS <- LWL_post
rm(LWL_post)
KTLS$`Pre-test(0).Post.Test.(1)` <- 1 # Assumed T1 due to sheet name of LWL_post
KTLS <- KTLS[!is.na(KTLS$`Ass.P1-Q1.(0-4)`),]  # remove rows with only focus group answers

LWL_updated_post2[,1] <- NULL # duplicated column with less information

LWL <- plyr::rbind.fill(LWL_updated_post2, LWL_updated_pre2)
rm(LWL_updated_post2, LWL_updated_pre2)
LWL <- LWL[!is.na(LWL$`Ass.P1-Q1.(0-4)`),]  # remove rows with NA answers

student_and_teacher_LSK_pre <- student_and_teacher_LSK_pre[,1:35] # discard SDQ 

LSK <- plyr::rbind.fill(student_and_teacher_LSK_pre, student_and_teacher_LSK_post)
LSK <- LSK[!is.na(LSK$`Ass.P1-Q1.(0-4)`),]  # remove rows with NA answers

rm(student_and_teacher_LSK_pre, student_and_teacher_LSK_post)

# identical(KTLS %>% select(starts_with("Ass")) %>% colnames(.),
#           MKES %>% select(starts_with("Ass")) %>% colnames(.))
# 
# identical(KTLS %>% select(starts_with("Ass")) %>% colnames(.),
#           LSK %>% select(starts_with("Ass")) %>% colnames(.))
# 
# identical(KTLS %>% select(starts_with("Ass")) %>% colnames(.),
#           LWL %>% select(starts_with("Ass")) %>% colnames(.))

df2 <- plyr::rbind.fill(LSK, KTLS, LWL, MKES) # df2 being data from excel files vs df being data from SPSS files
rm(LSK, KTLS, LWL, MKES)

df %>% select(starts_with("A01")) %>% colnames(.) -> qA
df %>% select(starts_with("B01")) %>% colnames(.) -> qB
df %>% select(starts_with("C01")) %>% colnames(.) -> qC
df %>% select(starts_with("D01")) %>% colnames(.) -> qD1
df %>% select(starts_with("D02")) %>% colnames(.) -> qD2
df %>% select(starts_with("E0")) %>% colnames(.) -> qE

qC1 <- qC[1:6]
qC2 <- qC[7:12]

questions_var <- c(qA, qB, qC, qD1, qD2, qE)

questions_var_old <- df2  %>% select(starts_with("Ass")) %>% colnames(.)

data.table::setnames(df2, old = questions_var_old, questions_var)

# identical(df %>% select(starts_with("Ass")) %>% colnames(.),
#           df2 %>% select(starts_with("Ass")) %>% colnames(.))

df2[df2=="99"] <- NA

df2 <- df2[,1:which(colnames(df2)=="part1-Q1")-1] # remove focus group answers

names(df2)[names(df2)=="Pre-test(0).Post.Test.(1)"] <- "T1"
names(df2)[names(df2)=="School.Name"] <- "sch"

# schools coding (S01=MKES, S02=KTLS, S03=LSK, S04=LWL, S05=CWSF, S06=LSC, S07=LHK)
df2 <- df2 %>% mutate(sch = plyr::mapvalues(sch,  c("KTLS","LSK","LWL","MKES"), c(2,3,4,1)))

names(df2)[names(df2)=="Int/Control.(1=int;.2=.control)"] <- "intervention"
df2 <- df2 %>% mutate(intervention = plyr::mapvalues(intervention,  c(1, 2), c("INV", "CTL")))

names(df2)[names(df2)=="Class"] <- "class"
df2$form <- as.numeric(substring(df2$class, 1,1 ))

names(df2)[names(df2)=="Class.No."] <- "student_num"
names(df2)[names(df2)=="Gender.(1=M;.2=F)"] <- "sex"
df2 <- df2 %>% mutate(sex= plyr::mapvalues(sex,  c(1,2), c(2,1)))

df2$dob <- as.Date(paste0(df2$'Date.of.birth.(day)', "/", df2$'Date.of.birth.(month)', "/", df2$'Date.of.birth.(year)'), format='%d/%m/%Y')

df2$submitdate <- as.Date(paste0(df2$'Questionnaire.date.(day)', "/", df2$'Questionnaire.date.(month)', "/", df2$'Questionnaire.date.(year)'), format='%d/%m/%Y')
df2$submitdate[df2$sch==1 & df2$class== "3B" & df2$student_num == 15 & df2$T1 == 0] <- as.Date("14/11/2018", format='%d/%m/%Y') # add submitdate based on submitdate of same school, class, and T0

df2$age <- as.numeric(floor((df2$submitdate-df2$dob)/365.2425))

df2[qC] <- df2[qC] + 1 # from (0:5) to (1:6)
df2[qD1] <- df2[qD1] + 1 # from (0:5) to (1:6)
df2[qD2] <- df2[qD2] + 1 # from (0:6) to (1:7)
df2[qE] <- df2[qE] + 1 # from (0:3) to (1:4)

# df cleaning ----
# schools coding (S01=MKES, S02=KTLS, S03=LSK, S04=LWL, S05=CWSF, S06=LSC, S07=LHK)
names(df)[names(df)=="Q05"] <- "sch"

df$sch[df$ipaddr=="116.92.228.90"] <- "S02" #KTLS
df$sch[df$ipaddr=="223.255.166.222"] <- "S05" #CWSF
df$sch[df$ipaddr=="210.3.138.94"] <- "S07" #LHK

df$sch <- as.integer(as.vector(substr(df$sch,2,3)))

names(df)[names(df)=="Q01"] <- "form"
df$form[df$id== 92 ] <- 2 # wrong form entry based on class, dob, sch, sex, student number, etc.
df$class <- paste0(df$form, df$Q01a)
names(df)[names(df)=="Q02"] <- "student_num"
names(df)[names(df)=="Q03"] <- "dob"
names(df)[names(df)=="Q04"] <- "sex" # 1=female, 2=male
names(df)[names(df)=="GROUPSEL"] <- "intervention"

# add int/ctrl information for missing values based on their other response in T0/T1.
df$intervention[df$intervention=="" & df$sch==2 & df$class=="2C" & df$student_num==8] <- "INV"
df$intervention[df$intervention=="" & df$sch==2 & df$class=="2C" & df$student_num==19] <- "INV"
df$intervention[df$intervention=="" & df$sch==5 & df$class=="3C" & df$student_num==2] <- "CTL"
df$intervention[df$intervention=="" & df$sch==5 & df$class=="3C" & df$student_num==5] <- "CTL"
df$intervention[df$intervention=="" & df$sch==7 & df$class=="2C" & df$student_num==18] <- "INV"

df$age <- as.numeric(floor((df$submitdate-df$dob)/365.2425))

df$year <- as.numeric(format(as.Date(df$submitdate),"%Y"))
df$month <- as.numeric(format(as.Date(df$submitdate),"%m"))
df$submitdate[df$sch==3 & df$class=="2E" & df$student_num==16 & df$T1==0] <- as.Date("27/11/2018", format='%d/%m/%Y')
df$dob[df$sch==5 & df$class=="3C" & df$student_num==18 & df$T1==1] <- as.Date("04/08/2004", format='%d/%m/%Y')

# Merging df and df2 ----
df <- select(df, c(sch, T1, intervention, class, form, student_num, sex, submitdate, dob, age, questions_var))
df2 <- select(df2, c(sch, T1, intervention, class, form, student_num, sex, submitdate, dob, age, questions_var))
df$online <- 1
df2$online <- 0
df <- rbind(df, df2)
rm(df2)

df$A01_2[df$A01_2 == 98] <- NA

df$intervention <- ifelse(df$intervention=="INV", 1, 0)
df$control <- ifelse(df$intervention, 0, 1)

df$sch <- as.numeric(df$sch)
df$T1 <- as.numeric(df$T1)

# Outcome Scoring ----
scoring <- function(df){
  qA_PD <- qA[c(3, 6, 9, 13, 15, 18, 21)] # personal distress, range(0-28) 
  qA_FS <- qA[c(2, 8, 12, 20)] # fantasy scale, range(0-16)
  qA_ES <- qA[c(1, 4, 5, 7, 10, 11, 14, 16, 17, 19, 22)] # empathy scale, range(0-44)
  
  # qA_PD2 <- qA[c(3, 6, 9, 13, 15)] # personal distress, range(0-20) 
  # qA_FS2 <- qA[c(2, 8, 12)] # fantasy scale, range(0-12)
  # qA_ES2 <- qA[c(1, 4, 5, 7, 10, 11, 14, 16)] # empathy scale, range(0-32)
  
  reverse_qA <- qA[c(8, 9, 10, 11, 14)] 
  # reverse scores (original labels no longer apply to these)
  df[reverse_qA] <- (df[reverse_qA]-4)*-1  # reverse (0,1,2,3,4) to (4,3,2,1,0)
  
  df <- df %>% 
    mutate(A_PD = rowSums(.[qA_PD], na.rm = FALSE),
           A_FS = rowSums(.[qA_FS], na.rm = FALSE),
           A_ES = rowSums(.[qA_ES], na.rm = FALSE))
  
  df$A_PD[is.na(df$A01_17)] <- NA # missing answers to questions 17-22 from part 1/A (A01_17 to A01_22)
  df$A_FS[is.na(df$A01_17)] <- NA
  df$A_ES[is.na(df$A01_17)] <- NA
  
  #              N   Mean       Median     SD        Min        Max    
  # A_PD         149 15.10738   15         4.760679  2          25        
  # A_FS         149 8.496644   8          2.677797  0          15        
  # A_ES         149 25.63087   26         4.520746  11         36   
  
  # df <- df %>% 
  #   mutate(A_PD2 = rowSums(.[qA_PD2], na.rm = FALSE),
  #          A_FS2 = rowSums(.[qA_FS2], na.rm = FALSE),
  #          A_ES2 = rowSums(.[qA_ES2], na.rm = FALSE))
  
  # eval(parse(text =
  #              sprintf("df$%s <- df$%s %s", 
  #                      c("A_PD", "A_FS", "A_ES"), 
  #                      c("A_PD2", "A_FS2", "A_ES2"),
  #                      c("/5*7", "/3*4", "/8*11")) # 5/7, 3/4, 8/11
  # ))
  
  # eval(parse(text =
  #              sprintf("df$%s[is.na(df$A01_17)] <- df$%s[is.na(df$A01_17)] %s",
  #                      c("A_PD", "A_FS", "A_ES"),
  #                      c("A_PD", "A_FS", "A_ES"),
  #                      c("/5*7", "/3*4", "/8*11")) # 5/7, 3/4, 8/11
  # ))
  #              N   Mean       Median     SD        Min        Max   
  # A_PD         198 15.56869   16         4.609946  2          28        
  # A_FS         198 8.69697    9          2.590316  0          15        
  # A_ES         198 25.6351    26         4.375577  11         36  
  
  reverse_qB <- qB[c(3, 4, 5, 7, 11, 17)]  # note that original values 1 = strongly agree, 4 = strongly disagree
  
  df[qB[!(qB %in% reverse_qB)]] <- lapply(df[qB[!(qB %in% reverse_qB)]], function(x) plyr::mapvalues(x, c(1,2,3,4), c(2,1,0,0))) # convert (1,2,3,4) to (2,1,0,0)
  df[reverse_qB] <- lapply(df[reverse_qB], function(x) plyr::mapvalues(x, c(1,2,3,4), c(0,0,1,2))) # convert (1,2,3,4) to (0,0,1,2)
  
  reverse_qC2 <- qC2[c(6)] # 12th question in qC which is the 6th question in qC2
  df[reverse_qC2] <- (df[reverse_qC2]-7)*-1 # reverse (1:6) to (6:1)
  
  reverse_qD1 <- qD1[c(3, 4)]
  df[reverse_qD1] <- (df[reverse_qD1]-7)*-1 # reverse (1:6) to (6:1)
  
  reverse_qE <- qE[c(1, 3, 4, 7, 8, 12)]
  df[reverse_qE] <- (df[reverse_qE]-5)*-1 # reverse (1:4) to (4:1)
  df[qE] <- df[qE]-1 # convert (1:4) to (0:3)
  
  df <- df %>% 
    mutate(B01 = rowSums(.[qB], na.rm = FALSE), # Empathy Quotient, range(0-44)
           C01 = rowSums(.[qC1], na.rm = FALSE), # Emotional Competence, range(6-36)
           C02 = rowSums(.[qC2], na.rm = FALSE), # Behavioural Competence, range(6-36)
           D01 = rowSums(.[qD1], na.rm = FALSE), # SLSS, range(7-42)
           D02 = rowSums(.[qD2], na.rm = FALSE), # BMSLSS, range(5-35)
           E01 = rowSums(.[qE], na.rm = FALSE)) # GHQ, range(0-36)
  
  return(subset(df, select = c(A_PD, A_FS, A_ES, B01, C01, C02, D01, D02, E01
  )))
}

df <- cbind(df, scoring(df))

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))
write_excel("qtn1819_secondary_selective_long.xlsx", df[])

df <- select(df, c(online, sch, T1, intervention, control,  class, form, student_num, sex, submitdate, dob, age,
                   A_PD, A_FS, A_ES,
                   B01, C01, C02, D01, D02, E01))

# saving long format ----
setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary", setpath))
df$online <- NULL
saveRDS(df, file = "qtn1819_secondary_selective_long.rds")
# write_excel("qtn1819_secondary_selective_long.xlsx", df[])

# matching students ----

df_pre <- df[df$T1==0,]
df_pre$T1 <- NULL
df_post <- df[df$T1==1,]
df_post$T1 <- NULL
# df_long<-rbind(df_pre,df_post) 
dfwide<-merge(df_pre, df_post, by.x = c("sch","class","student_num", "dob", "sex",
                                        "control", "intervention", "form"),
              by.y = c("sch","class","student_num", "dob", "sex",
                       "control", "intervention", "form"),
              all.x=TRUE, all.y = TRUE, suffixes = c(".0",".1"))

# dfwide_both<-merge(df_pre, df_post, by.x = c("sch","class","student_num", "dob", "sex",
#                                         "control", "intervention", "form"),
#               by.y = c("sch","class","student_num", "dob", "sex",
#                        "control", "intervention", "form"),
#               all.x=FALSE, all.y = FALSE, suffixes = c(".0",".1"))

for (q in c('A_PD', 'A_FS', 'A_ES', 'B01', 'C01', 'C02', 'D01', 'D02', 'E01')){
  eval_(sprintf("dfwide$%s <- (dfwide$%s - dfwide$%s)", q, paste0(q, ".1"),  paste0(q, ".0")))
}

dfwide <- dfwide %>% 
  # rowwise() %>% ## a slower method, pmax() is changed to max() with rowwise()
  mutate(age=pmax(age.0, age.1, na.rm=TRUE))

dfwide$T0 <- ifelse(!is.na(dfwide$age.0), 1, NA)
dfwide$T1 <- ifelse(!is.na(dfwide$age.1), 1, NA)

saveRDS(dfwide, file = "qtn1819_secondary_selective_wide.rds")
