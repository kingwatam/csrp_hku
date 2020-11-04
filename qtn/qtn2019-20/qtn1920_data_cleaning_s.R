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

# level 1 ----
setwd(sprintf("~%s/qtn/qtn2019-20/secondary", setpath))
dfpre <- haven::read_sav("survey_630000_SPSS.sav")
dfpost <- haven::read_sav("survey_630001_SPSS.sav")
# tepost <- haven::read_sav("survey_700000_SPSS.sav") # teacher self-efficacy post test

# save item names from dfpre for dfpre_HLC later
dfpre[20:115] %>% select(!starts_with("P1t2_") & !starts_with("P2t2x")) %>% colnames(.) -> items_names # exclude e-learning items

dfpre$T1 <- 0
dfpost$T1 <- 1
df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)
df$level <- 1

df <- convert2NA(df, c(""))

names(df)[names(df)=='Q01'] <- 'form'
names(df)[names(df)=='Q02'] <- 'student_num'
names(df)[names(df)=='Q03'] <- 'dob'
names(df)[names(df)=='Q04'] <- 'sex' # 1 = female, 2 = male

df$class <- ifelse(!is.na(df$Q01a), paste0(df$form, df$Q01a),
                   ifelse(!is.na(df$Q01b), paste0(df$form, df$Q01b),
                          paste0(df$form, df$Q01c)
                          ))

names(df)[names(df)=='Q05'] <- 'sch'
df$sch <- as.character(df$sch) # as.character() obtains values (s01, s02, etc) instead of labels (school names)
df$sch <- ifelse(df$T1==1,
                    car::recode(df$sch, "
                    'S10' =	'S01';
                    'S08' =	'S02';
                    'S09'	= 'S03';
                    'S13'	= 'S04';
                    'S07'	= 'S05';
                    'S06'	= 'S06';
                    'S11'	= 'S07';
                    'S02'	= 'S08';
                    'S14'	= 'S09';
                    'S01'	= 'S10';
                    'S04'	= 'S11';
                    'S05'	= 'S12';
                    'S12'	= 'S14'
                    ")
                 , df$sch) # Ho Lap College is paper-based

df$sch <- as.numeric(substr(df$sch, 2, 3))

# match ip addresses for those with missing schools in pre-test data
df$imputed_sch[is.na(df$sch)] <- 1 # not missing school
df$sch[is.na(df$sch) & df$ipaddr == "210.176.116.13"] <- 3
df$sch[is.na(df$sch) & df$ipaddr == "165.84.187.110"] <- 5
df$sch[is.na(df$sch) & df$ipaddr == "202.82.85.125"] <- 7
df$sch[is.na(df$sch) & df$ipaddr == "223.255.166.222"] <- 8
df$sch[is.na(df$sch) & df$ipaddr == "203.198.205.254"] <- 9
df$imputed_sch[is.na(df$imputed_sch)] <- 0 # imputed school based on IP address

df$intervention <- ifelse( (df$sch == 1 & df$form == 1) | # intervention group
                             (df$sch == 2 & df$form == 1) |
                             (df$sch == 3 & df$form == 3) |
                             (df$sch == 4 & df$form == 1) |
                             (df$sch == 5 & df$form == 2) |
                             (df$sch == 7 & df$form == 1) |
                             (df$sch == 8 & df$form == 2) |
                             (df$sch == 9 ) | # whole school doing the program (double check if all intervention group)
                             (df$sch == 10 & df$form == 1) |
                             (df$sch == 11 & df$form == 1) |
                             (df$sch == 12 & df$form == 1), 1,
                           ifelse( (df$sch == 5 & df$form == 1) | # control group
                                     (df$sch == 8 & df$form == 1) |
                                     (df$sch == 11 & df$form == 2), 0, NA))
df$control <- ifelse(df$intervention, 0, 1)

df$sch <- car::recode(df$sch, "
1 = 'YLCSS';
2 =  'LWLC';
3 = 'MHLSS';
4 = 'CSASS';
5 =  'TKP';
6 = 'LSK';
7 = 'LSC';
8 = 'CWSFMSS';
9 = 'DMS';
10 = 'LHKSS';
11 = 'MKES';
12 = 'KTLS';
14 = 'DGS'
")

# HLC level 1 pre-test
setwd(sprintf("~%s/qtn/qtn2019-20/secondary", setpath))
dfpre_HLC <- openxlsx::read.xlsx("Sec_Year2-pretest_final_king.xlsx")

dfpre_HLC <- dfpre_HLC[2:nrow(dfpre_HLC),]
dfpre_HLC[dfpre_HLC==999] <- NA
names(dfpre_HLC)[1] <- "sch"
names(dfpre_HLC)[2] <- "form"
names(dfpre_HLC)[3] <- "class"
dfpre_HLC$class <- paste0(dfpre_HLC$form, dfpre_HLC$class)
names(dfpre_HLC)[4] <- "student_num"
names(dfpre_HLC)[5] <- "dob"
dfpre_HLC$dob <- ifelse(nchar(dfpre_HLC$dob) < 8, paste0("0", dfpre_HLC$dob), dfpre_HLC$dob)
dfpre_HLC$dob <- as.Date(dfpre_HLC$dob, format = "%d%m%Y")
names(dfpre_HLC)[6] <- "sex"
dfpre_HLC$sex <- (as.numeric(dfpre_HLC$sex)-3)*-1 # from 1=M:2=F to 1=F;2=M
names(dfpre_HLC)[7] <- "submitdate"
dfpre_HLC$submitdate <- as.Date(dfpre_HLC$submitdate, format = "%d%m%Y")
dfpre_HLC[c(9:20, 29:84)]  <- lapply(dfpre_HLC[c(9:20, 29:84)], as.numeric)
dfpre_HLC[9:20] <- (dfpre_HLC[9:20]-2)*-1 # from 1:2 to 0:1
dfpre_HLC$T1 <- 0
dfpre_HLC$intervention <- 1
dfpre_HLC$control <- 0

names(dfpre_HLC)[9:84] <- items_names
df <- plyr::rbind.fill(df, dfpre_HLC)
rm(dfpre_HLC)

df$age <- as.numeric(floor((as.Date("2020-06-30")-as.Date(df$dob))/365.2425))

# P5xa_1 refers to a different question for post-test, create new variable P5xc_1
df$P5xc_1 <- ifelse(df$T1==1, df$P5xa_1, NA)
df$P5xa_1 <- ifelse(df$T1==0, df$P5xa_1, NA)

# the purpose of this function is to keep the original items unchanged
scoring_level1 <- function(df){
  df %>% select(starts_with("P1_")) %>% colnames(.) -> P1
  df %>% select(starts_with("P1t2_")) %>% colnames(.) -> P1e # elearning
  df %>% select(starts_with("P2x")) %>% colnames(.) -> P2
  df %>% select(starts_with("P2t2x")) %>% colnames(.) -> P2e # elearning
  df %>% select(starts_with("P3_")) %>% colnames(.) -> P3
  df %>% select(starts_with("P4_")) %>% colnames(.) -> P4
  df %>% select(starts_with("P5xa_")) %>% colnames(.) -> P5a
  df %>% select(starts_with("P5xb_")) %>% colnames(.) -> P5b
  df %>% select(starts_with("P5xc_")) %>% colnames(.) -> P5a2
  df %>% select(starts_with("P6_")) %>% colnames(.) -> P6
  df %>% select(starts_with("P7_")) %>% colnames(.) -> P7
  df %>% select(starts_with("P08x1")) %>% colnames(.) -> P8a
  df %>% select(starts_with("P08x2")) %>% colnames(.) -> P8b
  df %>% select(starts_with("P09x1")) %>% colnames(.) -> P9a
  df %>% select(starts_with("P09x2")) %>% colnames(.) -> P9b
  df %>% select(starts_with("P09x3")) %>% colnames(.) -> P9c
  
  reverse_P1 <- P1[c(2, 3, 7, 8, 9, 12)]
  reverse_P1e <- P1e[c(2, 3, 7, 8, 9)]
  df[reverse_P1] <- (df[reverse_P1]-1)*-1  # reverse (0,1) to (1,0)
  df[reverse_P1e] <- (df[reverse_P1e]-1)*-1  # reverse (0,1) to (1,0)
  
  df[P2[1]] <- ifelse(df[P2[1]] == 'D', 1, 0) # D is correct
  df[P2[2]] <- ifelse(df[P2[2]] == 'C', 1, 0)
  df[P2[3]] <- ifelse(df[P2[3]] == 'A', 1, 0)
  df[P2[4]] <- ifelse(df[P2[4]] == 'D', 1, 0)
  df[P2[5]] <- ifelse(df[P2[5]] == 'D', 1, 0)
  df[P2[6]] <- ifelse(df[P2[6]] == 'C', 1, 0)
  df[P2[7]] <- ifelse(df[P2[7]] == 'D', 1, 0)
  df[P2[8]] <- ifelse(df[P2[8]] == 'D', 1, 0)
  
  df[P2e[1]] <- ifelse(df[P2e[1]] == 'D', 1, 0)
  df[P2e[2]] <- ifelse(df[P2e[2]] == 'C', 1, 0)
  df[P2e[3]] <- ifelse(df[P2e[3]] == 'A', 1, 0)
  df[P2e[4]] <- ifelse(df[P2e[4]] == 'D', 1, 0)
  df[P2e[5]] <- ifelse(df[P2e[5]] == 'D', 1, 0)
  df[P2e[6]] <- ifelse(df[P2e[6]] == 'C', 1, 0)
  df[P2e[7]] <- ifelse(df[P2e[7]] == 'C', 1, 0)
  df[P2e[8]] <- ifelse(df[P2e[8]] == 'B', 1, 0)
  
  # GHQ
  df[P3] <- (df[P3]-1) # from (1:4) to (0:3)
  reverse_P3 <- P3[c(1, 3, 4, 7, 8, 12)]
  df[reverse_P3] <- (df[reverse_P3]-3)*-1  # reverse (0:3) to (3:0)
  
  # negative thinking
  reverse_P4 <- P4[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)]
  
  # SLSS
  reverse_P5a <- P5a[c(3, 4)]
  df[reverse_P5a] <- (df[reverse_P5a]-7)*-1 # reverse (1:6) to (6:1)
  
  # Empathy
  reverse_P6 <- P6[c(3)]
  df[reverse_P6] <- (df[reverse_P6]-4)*-1  # reverse (0:4) to (4:0)
  
  # Gratitude
  reverse_P7 <- P7[c(3, 6)]
  df[reverse_P7] <- (df[reverse_P7]-8)*-1  # reverse (1:7) to (7:1)
  
  # Compassion
  df[P8a] <- df[P8a]+1  # reverse (0:4) to (1:5)
  reverse_P8a <- P8a[c(3)]
  df[reverse_P8a] <- (df[reverse_P8a]-6)*-1  # reverse (1:5) to (5:1)
  
  # Self-compassion
  df[P8b] <- df[P8b]+1  # reverse (0:4) to (1:5)
  reverse_P8b <- P8b[c(3, 4, 5, 7, 9)]
  df[reverse_P8b] <- (df[reverse_P8b]-6)*-1  # reverse (1:5) to (5:1)
  
  # Prejudice
  df[P9a] <- lapply(df[P9a], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  df[P9c] <- lapply(df[P9c], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  P9c1 <- P9c[1:5]
  P9c2 <- P9c[6:7]
  
  P9a_b <- c(P9a, P9b)
  
  df$elearning <- ifelse(!is.na(df$P1_1), 0,
                         ifelse(!is.na(df$P1t2_1), 1, NA))
  
  df <- df %>%
    mutate(q1 = ifelse(!elearning, rowSums(.[P1], na.rm = FALSE), rowSums(.[P1e], na.rm = FALSE)), # Q1&2 are Mental Health Knowledge, na.rm = FALSE means any NA in an item results in NA for the scale
           q2 = ifelse(!elearning, rowSums(.[P2], na.rm = FALSE), rowSums(.[P2e], na.rm = FALSE)),
           q3 = rowSums(.[P3], na.rm = FALSE), # GHQ Psychological Distress range(0-36)
           q4neg = rowSums(.[reverse_P4], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
           q4pos = rowSums(.[P4[!(P4 %in% reverse_P4)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
           q5a = rowSums(.[P5a], na.rm = FALSE), # SLSS, range(7-42)
           q5a2 = rowSums(.[P5a2], na.rm = FALSE), # 1-item on satisfaction to replace SLSS in post-test, range(1-7)
           q5b = rowSums(.[P5b], na.rm = FALSE), # BMSLSS, range(5-35)
           q6 = rowSums(.[P6], na.rm = FALSE), # Empathy range(0-24)
           q7 = rowSums(.[P7], na.rm = FALSE), # Gratitude range(6-42)
           q8a = rowMeans(.[P8a], na.rm = FALSE), # partial compassion range(1-5), mean instead of sum
           q8b = rowMeans(.[P8b], na.rm = FALSE), # partial self-compassion range(1-5), mean instead of sum
           q9a_b= rowMeans(.[P9a_b], na.rm = FALSE), # help-seeking, mean instead of sum
           q9c = rowMeans(.[P9c], na.rm = FALSE) # partial PPMI range(1-3), mean instead of sum
    )
  
  df$q1_2 <- df$q1 + df$q2
  
  return(subset(df, select = c(q1, q2, q1_2, q3, q4neg, q4pos, q5a, q5a2, q5b, q6, q7, q8a, q8b, q9a_b, q9c
  )))
}

df <- cbind(df, scoring_level1(df))

df <- subset(df, select = c(sch, imputed_sch, form, class, student_num, age, sex,
                                  q1, q2, q1_2, q3, q4neg, q4pos, q5a, q5a2, q5b, q6, q7, q8a, q8b, q9a_b, q9c,
                                  control, submitdate, dob, T1, intervention, level))

dflevel1 <- df
rm(df)

# level 2 ----
setwd(sprintf("~%s/qtn/qtn2019-20/secondary", setpath))
dfpre <- haven::read_sav("survey_650000_SPSS.sav")
dfpost <- haven::read_sav("survey_630002_SPSS.sav")

dfpre$T1 <- 0
dfpost$T1 <- 1

names(dfpre)[names(dfpre)=='Q01'] <- 'form' 
names(dfpre)[names(dfpre)=='Q03'] <- 'student_num'
names(dfpre)[names(dfpre)=='Q05'] <- 'dob' 
names(dfpre)[names(dfpre)=='Q06'] <- 'sex' # 1 = female, 2 = male
names(dfpre)[names(dfpre)=='Q00'] <- 'sch' 

dfpre$sch <- as.numeric(substr(as.factor(dfpre$sch), 2, 3)) 
dfpre$imputed_sch <- NA
dfpre$imputed_sch[is.na(dfpre$sch)] <- 1 # not missing school
dfpre$sch[is.na(dfpre$sch) & dfpre$ipaddr == "61.238.98.99"] <- 1
dfpre$sch[is.na(dfpre$sch) & dfpre$ipaddr == "218.255.4.34"] <- 4
dfpre$sch[is.na(dfpre$sch) & dfpre$ipaddr == "221.126.251.2"] <- 4
dfpre$sch[is.na(dfpre$sch) & dfpre$ipaddr == "223.255.166.222"] <- 5
dfpre$imputed_sch[is.na(dfpre$imputed_sch)] <- 0 # imputed school based on IP address

dfpre$sch <- car::recode(as.factor(dfpre$sch), "
1 = 'MKES';
2 =  'KTLS';
3 = 'LSK';
4 = 'LWLC';
5 =  'CWSFMSS';
6 = 'LSC';
7 = 'LHKSS';
8 = 'TKP';
9 = 'MHLSS';
10 = 'YLCSS';
")

dfpre$class <- ifelse(!is.na(dfpre$Q02a), paste0(dfpre$form, dfpre$Q02a), 
                   ifelse(!is.na(dfpre$Q02b), paste0(dfpre$form, dfpre$Q02b), 
                          paste0(dfpre$form, dfpre$Q02c)
                   ))

# HLC level 2 pre-test
setwd(sprintf("~%s/qtn/qtn2019-20/secondary", setpath))
dfpre_HLC <- openxlsx::read.xlsx("Sec_pretest_HLC_F.4_final_king.xlsx")

dfpre_HLC <- dfpre_HLC[2:nrow(dfpre_HLC),]
dfpre_HLC[dfpre_HLC==999] <- NA
names(dfpre_HLC)[1] <- "sch"
names(dfpre_HLC)[2] <- "form"
names(dfpre_HLC)[3] <- "class"
dfpre_HLC$class <- paste0(dfpre_HLC$form, dfpre_HLC$class)
names(dfpre_HLC)[4] <- "student_num"
names(dfpre_HLC)[5] <- "dob"
dfpre_HLC$dob <- ifelse(nchar(dfpre_HLC$dob) < 8, paste0("0", dfpre_HLC$dob), dfpre_HLC$dob)
dfpre_HLC$dob <- as.Date(dfpre_HLC$dob, format = "%d%m%Y")
names(dfpre_HLC)[6] <- "sex"
dfpre_HLC$sex <- (as.numeric(dfpre_HLC$sex)-3)*-1 # from 1=M:2=F to 1=F;2=M
names(dfpre_HLC)[7] <- "submitdate"
dfpre_HLC$submitdate <- as.Date(dfpre_HLC$submitdate, format = "%d%m%Y")
dfpre_HLC[c(9:18, 23:102)]  <- lapply(dfpre_HLC[c(9:18, 23:102)], as.numeric)
dfpre_HLC[9:18] <- (dfpre_HLC[9:18]-2)*-1 # from 1:2 to 0:1
dfpre_HLC$T1 <- 0

names(dfpre_HLC)[9:102] <- names(dfpre)[22:115] 
dfpre <- plyr::rbind.fill(dfpre, dfpre_HLC)
rm(dfpre_HLC)

names(dfpost)[names(dfpost)=='Q01'] <- 'form' 
names(dfpost)[names(dfpost)=='Q02'] <- 'student_num'
names(dfpost)[names(dfpost)=='Q03'] <- 'dob' 
names(dfpost)[names(dfpost)=='Q04'] <- 'sex' # 1 = female, 2 = male
names(dfpost)[names(dfpost)=='Q05'] <- 'sch' 

dfpost$sch <- as.numeric(substr(as.factor(dfpost$sch), 2, 3)) 
dfpost$imputed_sch <- NA
dfpost$imputed_sch[is.na(dfpost$imputed_sch)] <- 0 
dfpost$sch <- car::recode(as.factor(dfpost$sch), "
1 = 'LHKSS';
2 =  'CWSFMSS';
3 = 'HLC';
4 = 'MKES';
5 =  'KTLS';
6 = 'LSK';
7 = 'TKP';
8 = 'LWLC';
9 = 'MHLSS';
10 = 'YLCSS';
11 = 'LSC';
12 = 'DGS';
13 = 'CSASS';
14 = 'DMS'
")

dfpost$class <- ifelse(!is.na(dfpost$Q01a), paste0(dfpost$form, dfpost$Q01a), 
                   ifelse(!is.na(dfpost$Q01b), paste0(dfpost$form, dfpost$Q01b), 
                          paste0(dfpost$form, dfpost$Q01c)
                   ))

names(dfpre)[99:115] <- names(dfpost)[91:107]

df <- plyr::rbind.fill(dfpre, dfpost)
rm(dfpre, dfpost)
df$level <- 2

df <- convert2NA(df, c(""))
df$age <- as.numeric(floor((as.Date("2020-06-30")-as.Date(df$dob))/365.2425))

df$intervention <- ifelse( (df$sch == "LHKSS" & df$form == 2) | # intervention group
                             (df$sch == "MKES" & df$form == 1) |
                             (df$sch == "LWLC" & df$form == 2) |
                             (df$sch == "LSC" & df$form == 2) |
                             (df$sch == "CWSFMSS" & df$form == 3), 1, NA) # no control group for level 2 this year
df$control <- ifelse(df$intervention, 0, 1)

scoring_level2 <- function(df){
  df %>% select(starts_with("Q01x01")) %>% colnames(.) -> P1
  df %>% select(starts_with("Q01x0") & ends_with(sprintf("0%s", 2:5))) %>% colnames(.)  -> P2
  df %>% select(starts_with("A01_")) %>% colnames(.) -> P3 # C-IRI
  df %>% select(starts_with("B01_")) %>% colnames(.) -> P4 # Empathy Quotient (EQ)
  df %>% select(starts_with("C01_")) %>% colnames(.) -> P5 # Emotional Competence & Behavioural Competence
  df %>% select(starts_with("D01_")) %>% colnames(.) -> P6a # SLSS (pre-test only)
  df %>% select(starts_with("P5xa_")) %>% colnames(.) -> P6a2 # one-item life satisfaction to replace SLSS
  df %>% select(starts_with("P5xb_")) %>% colnames(.) -> P6b # BMSLSS (pre-test D02_ already replaced with P5xb_, same as post-test variable names)
  df %>% select(starts_with("P3_")) %>% colnames(.) -> P7 # GHQ (pre-test E01_ already replaced with P3_, same as post-test variable names)
  df %>% select(starts_with("P08x1")) %>% colnames(.) -> P8a
  df %>% select(starts_with("P08x2")) %>% colnames(.) -> P8b
  df %>% select(starts_with("P09x1")) %>% colnames(.) -> P9a
  df %>% select(starts_with("P09x2")) %>% colnames(.) -> P9b
  df %>% select(starts_with("P09x3")) %>% colnames(.) -> P9c
  
  reverse_P1 <- P1[c(1, 3, 5, 6, 10)] 
  df[reverse_P1] <- (df[reverse_P1]-1)*-1  # reverse (0,1) to (1,0)
  
  df[P2[1]] <- ifelse(df[P2[1]] == 'C', 1, 0) # C is correct
  df[P2[2]] <- ifelse(df[P2[2]] == 'D', 1, 0) 
  df[P2[3]] <- ifelse(df[P2[3]] == 'B', 1, 0) 
  df[P2[4]] <- ifelse(df[P2[4]] == 'D', 1, 0) 
  
  # C-IRI empathy subscales
  P3_PD <- P3[c(3, 6, 9, 13, 15, 18, 21)] # personal distress, range(0-28) 
  P3_FS <- P3[c(2, 8, 12, 20)] # fantasy scale, range(0-16)
  P3_ES <- P3[c(1, 4, 5, 7, 10, 11, 14, 16, 17, 19, 22)] # empathy scale, range(0-44)
  
  reverse_P3 <- P3[c(8, 9, 10, 11, 14)] 
  df[reverse_P3] <- (df[reverse_P3]-4)*-1  # reverse (0:4) to (4:0)
  
  # Empathy Quotient (EQ)
  reverse_P4 <- P4[c(3, 4, 5, 7, 11, 17)]  # note that original values 1 = strongly agree, 4 = strongly disagree
  df[P4[!(P4 %in% reverse_P4)]] <- lapply(df[P4[!(P4 %in% reverse_P4)]], function(x) plyr::mapvalues(x, c(1,2,3,4), c(2,1,0,0))) # convert (1,2,3,4) to (2,1,0,0)
  df[reverse_P4] <- lapply(df[reverse_P4], function(x) plyr::mapvalues(x, c(1,2,3,4), c(0,0,1,2))) # convert (1,2,3,4) to (0,0,1,2)
  
  # Emotional Competence & Behavioural Competence
  P5a <- P5[c(1:6)] 
  P5b <- P5[c(7:12)] 
  reverse_P5b <- P5b[c(6)] # 12th question in P5 which is the 6th question in P5b
  df[reverse_P5b] <- (df[reverse_P5b]-7)*-1 # reverse (1:6) to (6:1)
  
  # SLSS
  reverse_P6a <- P6a[c(3, 4)]
  df[reverse_P6a] <- (df[reverse_P6a]-7)*-1 # reverse (1:6) to (6:1)
  
  # GHQ
  df[P7] <- (df[P7]-1) # from (1:4) to (0:3)
  reverse_P7 <- P7[c(1, 3, 4, 7, 8, 12)] 
  df[reverse_P7] <- (df[reverse_P7]-3)*-1  # reverse (0:3) to (3:0)
  
  # Compassion
  df[P8a] <- df[P8a]+1  # reverse (0:4) to (1:5)
  reverse_P8a <- P8a[c(3)] 
  df[reverse_P8a] <- (df[reverse_P8a]-6)*-1  # reverse (1:5) to (5:1)
  
  # Self-compassion
  df[P8b] <- df[P8b]+1  # reverse (0:4) to (1:5)
  reverse_P8b <- P8b[c(3, 4, 5, 7, 9)] 
  df[reverse_P8b] <- (df[reverse_P8b]-6)*-1  # reverse (1:5) to (5:1)
  
  # Prejudice
  df[P9a] <- lapply(df[P9a], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  df[P9c] <- lapply(df[P9c], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  P9c1 <- P9c[1:5]
  P9c2 <- P9c[6:7]
  
  P9a_b <- c(P9a, P9b)
  
  df <- df %>% 
    mutate(q1 = rowSums(.[P1], na.rm = FALSE), # Q1&2 are Mental Health Knowledge, na.rm = FALSE means any NA in an item results in NA for the scale
           q2 = rowSums(.[P2], na.rm = FALSE),
           q3 = rowSums(.[P3], na.rm = FALSE), # empathy C-IRI range(0-88)
           q3_PD = rowSums(.[P3_PD], na.rm = FALSE), # C-IRI personal distress, range(0-28) 
           q3_FS = rowSums(.[P3_FS], na.rm = FALSE), # C-IRI fantasy scale, range(0-16)
           q3_ES = rowSums(.[P3_ES], na.rm = FALSE), # C-IRI empathy scale, range(0-44)
           q4 = rowSums(.[P4], na.rm = FALSE), # Empathy Quotient (EQ), range(0-44)
           q5a = rowSums(.[P5a], na.rm = FALSE), # Emotional Competence, range(6-36)
           q5b = rowSums(.[P5b], na.rm = FALSE), # Behavioural Competence, range(6-36)
           q6a = rowSums(.[P6a], na.rm = FALSE), # SLSS, range(7-42)
           q6a2 = rowSums(.[P6a2], na.rm = FALSE), # 1-item on satisfaction to replace SLSS in post-test, range(1-7)
           q6b = rowSums(.[P6b], na.rm = FALSE), # BMSLSS, range(5-35)
           q7 = rowSums(.[P7], na.rm = FALSE), # GHQ Psychological Distress range(0-36)
           q8a = rowMeans(.[P8a], na.rm = FALSE), # partial compassion range(1-5), mean instead of sum
           q8b = rowMeans(.[P8b], na.rm = FALSE), # partial self-compassion range(1-5), mean instead of sum
           q9a_b= rowMeans(.[P9a_b], na.rm = FALSE), # help-seeking, mean instead of sum
           q9c = rowMeans(.[P9c], na.rm = FALSE) # partial PPMI range(1-2), mean instead of sum
    )
  
  df$q1_2 <- df$q1 + df$q2
  
  return(subset(df, select = c(q1, q2, q1_2, q3, q3_PD, q3_FS, q3_ES, q4, q5a, q5b, q6a, q6a2, q6b, q7, q8a, q8b, q9a_b, q9c
  )))
}

df <- cbind(df, scoring_level2(df))

df <- subset(df, select = c(sch, imputed_sch, form, class, student_num, age, sex, 
                            q1, q2, q1_2, q3, q3_PD, q3_FS, q3_ES, q4, q5a, q5b, q6a, q6a2, q6b, q7, q8a, q8b, q9a_b, q9c,
                            control, submitdate, dob, T1, intervention, level))

dflevel2 <- df
rm(df)

# saveRDS(df, file = "qtn1920_secondary_lvl1.rds")

# level 3 ----
setwd(sprintf("~%s/qtn/qtn2019-20/secondary", setpath))
dfpost <- haven::read_sav("survey_630003_SPSS.sav")

dfpost$T1 <- 1

df <- dfpost
rm(dfpost)

names(df)[names(df)=='Q01'] <- 'form' 
names(df)[names(df)=='Q02'] <- 'student_num'
names(df)[names(df)=='Q03'] <- 'dob' 
names(df)[names(df)=='Q04'] <- 'sex' # 1 = female, 2 = male
names(df)[names(df)=='Q05'] <- 'sch' 

df$class <- paste0(df$form, df$Q01a)
df$sch <- "LHKSS"
df$age <- as.numeric(floor((as.Date("2020-06-30")-as.Date(df$dob))/365.2425))
df$intervention <- 1
df$control <- 0

scoring_level3 <- function(df){
  df %>% select(starts_with("Q01x01")) %>% colnames(.) -> P1
  df %>% select(starts_with("Q01x0") & ends_with(sprintf("0%s", 2:5))) %>% colnames(.)  -> P2
  df %>% select(starts_with("A01_")) %>% colnames(.) -> P3 # C-IRI
  # df %>% select(starts_with("B01_")) %>% colnames(.) -> P4 # Empathy Quotient (EQ)
  df %>% select(starts_with("C01_")) %>% colnames(.) -> P5 # Emotional Competence & Behavioural Competence
  # df %>% select(starts_with("D01_")) %>% colnames(.) -> P6a # SLSS (pre-test only)
  df %>% select(starts_with("P5xa_")) %>% colnames(.) -> P6a2 # one-item life satisfaction to replace SLSS
  df %>% select(starts_with("P5xb_")) %>% colnames(.) -> P6b # BMSLSS 
  df %>% select(starts_with("P3_")) %>% colnames(.) -> P7 # GHQ 
  df %>% select(starts_with("P06x1")) %>% colnames(.) -> P8a
  df %>% select(starts_with("P06x2")) %>% colnames(.) -> P8b
  df %>% select(starts_with("P07x1")) %>% colnames(.) -> P9a
  df %>% select(starts_with("P07x2")) %>% colnames(.) -> P9b
  df %>% select(starts_with("P07x3")) %>% colnames(.) -> P9c
  
  reverse_P1 <- P1[c(1, 2, 4, 6, 7)] 
  df[reverse_P1] <- (df[reverse_P1]-1)*-1  # reverse (0,1) to (1,0)
  
  df[P2[1]] <- ifelse(df[P2[1]] == 'C', 1, 0) # C is the correct answer
  df[P2[2]] <- ifelse(df[P2[2]] == 'D', 1, 0) 
  df[P2[3]] <- ifelse(df[P2[3]] == 'C', 1, 0) 
  df[P2[4]] <- ifelse(df[P2[4]] == 'D', 1, 0) 
  
  # C-IRI empathy subscales
  P3_PD <- P3[c(3, 6, 9, 13, 15, 18, 21)] # personal distress, range(0-28) 
  P3_FS <- P3[c(2, 8, 12, 20)] # fantasy scale, range(0-16)
  P3_ES <- P3[c(1, 4, 5, 7, 10, 11, 14, 16, 17, 19, 22)] # empathy scale, range(0-44)
  
  reverse_P3 <- P3[c(8, 9, 10, 11, 14)] 
  df[reverse_P3] <- (df[reverse_P3]-4)*-1  # reverse (0:4) to (4:0)
  
  # # Empathy Quotient (EQ)
  # reverse_P4 <- P4[c(3, 4, 5, 7, 11, 17)]  # note that original values 1 = strongly agree, 4 = strongly disagree
  # df[P4[!(P4 %in% reverse_P4)]] <- lapply(df[P4[!(P4 %in% reverse_P4)]], function(x) plyr::mapvalues(x, c(1,2,3,4), c(2,1,0,0))) # convert (1,2,3,4) to (2,1,0,0)
  # df[reverse_P4] <- lapply(df[reverse_P4], function(x) plyr::mapvalues(x, c(1,2,3,4), c(0,0,1,2))) # convert (1,2,3,4) to (0,0,1,2)
  
  # Emotional Competence & Behavioural Competence
  P5a <- P5[c(1:6)] 
  P5b <- P5[c(7:12)] 
  reverse_P5b <- P5b[c(6)] # 12th question in P5 which is the 6th question in P5b
  df[reverse_P5b] <- (df[reverse_P5b]-7)*-1 # reverse (1:6) to (6:1)
  
  # # SLSS
  # reverse_P6a <- P6a[c(3, 4)]
  # df[reverse_P6a] <- (df[reverse_P6a]-7)*-1 # reverse (1:6) to (6:1)
  
  # GHQ
  df[P7] <- (df[P7]-1) # from (1:4) to (0:3)
  reverse_P7 <- P7[c(1, 3, 4, 7, 8, 12)] 
  df[reverse_P7] <- (df[reverse_P7]-3)*-1  # reverse (0:3) to (3:0)
  
  # Compassion
  df[P8a] <- df[P8a]+1  # reverse (0:4) to (1:5)
  reverse_P8a <- P8a[c(3)] 
  df[reverse_P8a] <- (df[reverse_P8a]-6)*-1  # reverse (1:5) to (5:1)
  
  # Self-compassion
  df[P8b] <- df[P8b]+1  # reverse (0:4) to (1:5)
  reverse_P8b <- P8b[c(3, 4, 5, 7, 9)] 
  df[reverse_P8b] <- (df[reverse_P8b]-6)*-1  # reverse (1:5) to (5:1)
  
  # Prejudice
  df[P9a] <- lapply(df[P9a], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  df[P9c] <- lapply(df[P9c], FUN = function(x) car::recode(x, "3 = 1.5"))  # recode (yes, no, not sure) to (yes, not sure, no)
  P9c1 <- P9c[1:5]
  P9c2 <- P9c[6:7]
  
  P9a_b <- c(P9a, P9b)
  
  df <- df %>% 
    mutate(q1 = rowSums(.[P1], na.rm = FALSE), # Q1&2 are Mental Health Knowledge, na.rm = FALSE means any NA in an item results in NA for the scale
           q2 = rowSums(.[P2], na.rm = FALSE),
           q3 = rowSums(.[P3], na.rm = FALSE), # empathy C-IRI range(0-88)
           q3_PD = rowSums(.[P3_PD], na.rm = FALSE), # C-IRI personal distress, range(0-28) 
           q3_FS = rowSums(.[P3_FS], na.rm = FALSE), # C-IRI fantasy scale, range(0-16)
           q3_ES = rowSums(.[P3_ES], na.rm = FALSE), # C-IRI empathy scale, range(0-44)
           # q4 = rowSums(.[P4], na.rm = FALSE), # Empathy Quotient (EQ), range(0-44)
           q5a = rowSums(.[P5a], na.rm = FALSE), # Emotional Competence, range(6-36)
           q5b = rowSums(.[P5b], na.rm = FALSE), # Behavioural Competence, range(6-36)
           # q6a = rowSums(.[P6a], na.rm = FALSE), # SLSS, range(7-42)
           q6a2 = rowSums(.[P6a2], na.rm = FALSE), # 1-item on satisfaction to replace SLSS in post-test, range(1-7)
           q6b = rowSums(.[P6b], na.rm = FALSE), # BMSLSS, range(5-35)
           q7 = rowSums(.[P7], na.rm = FALSE), # GHQ Psychological Distress range(0-36)
           q8a = rowMeans(.[P8a], na.rm = FALSE), # partial compassion range(1-5), mean instead of sum
           q8b = rowMeans(.[P8b], na.rm = FALSE), # partial self-compassion range(1-5), mean instead of sum
           q9a_b= rowMeans(.[P9a_b], na.rm = FALSE), # help-seeking, mean instead of sum
           q9c = rowMeans(.[P9c], na.rm = FALSE) # partial PPMI range(1-2), mean instead of sum
    )
  
  df$q1_2 <- df$q1 + df$q2
  
  return(subset(df, select = c(q1, q2, q1_2, q3, q3_PD, q3_FS, q3_ES, q5a, q5b, q6a2, q6b, q7, q8a, q8b, q9a_b, q9c
  )))
}

df <- cbind(df, scoring_level3(df))

dflevel3 <- df
rm(df)