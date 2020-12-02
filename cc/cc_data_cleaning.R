rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

setwd(sprintf("~%s/cc/cc_data", setpath))

library(openxlsx)
library(plyr) 
library(dplyr)
library(magrittr) # pipe 
library(tidyr)
library(car) # recode

# load data ----
# cc <- openxlsx::read.xlsx("data_02to17.xlsx" )
# saveRDS(cc, file = "data_02to17.rds")

# cc_anna <- openxlsx::read.xlsx("data_02to17_anna.xlsx" )
# saveRDS(cc_anna, file = "data_02to17_anna.rds")

cc <- readRDS("data_02to17.rds")
# ccanna <- readRDS("data_02to17_anna.rds")

df <- subset(cc, select = c(Year, DI_MA, case_no, 
                            arealive, live_address, areadie, die_address, die_area, oth_area,
                            cert_dd, cert_mm, cert_yy,
                            inc_dd, inc_mm, inc_yy,
                            icd_10, diemth_1, oth_m_1, diemth_2, oth_m_2, diemth_3, oth_m_3, 
                            gender, age,
                            birth_dd, birth_mm, birth_yy,
                            id_pass, type_id_pass, id_pass_CC,
                            nationality, oth_nationality,
                            marital, divorce, cross_border, sex_orientn,
                            ethnic, oth_ethni, 
                            bornarea, oth_born, year_hk,
                            edu, oth_edu, 
                            employ_status, occup, occup_group, # occup_group_student,
                            income, 
                            typ_hse, oth_hse,
                            public, 
                            psy, psy_num, psy_tt_ever, nowpsy1, 
                            sui_idea, psytreat,
                            Med, medprob1, 
                            FAM, FAM_1, OTH_FAM1, FAM_2, OTH_FAM2, FAM_3, OTH_FAM3, FAM_4, OTH_FAM4,
                            WORK,	WORK_1,	OTH_WORK1,	WORK_2,	OTH_WORK2,	WORK_3,	OTH_WORK3,	WORK_4,	OTH_WORK4,
                            FIN, FIN_1, OTH_FIN1, FIN_2, OTH_FIN2, FIN_3, OTH_FIN3, FIN_4, OTH_FIN4,
                            ABU,	ABU_1,	OTH_ABU1,	ABU_2,	OTH_ABU2
                            
))
rm(cc)

# remove leading & trailing empty spaces
for (i in names(df)) {
  if(class(df[, i]) %in% c("factor", "character")){
    df[, i] <- trimws(df[, i])
  }
}

df[df == "Missing"] <- NA
df[df == "missing"] <- NA
df[df == ""] <- NA
# df[df== " "] <- NA # blanks, this is reduntant after using trimws above
df[df == "NA"] <- NA # blanks
df[df == "N.A."] <- NA # blanks
df$id_pass_CC[df$id_pass_CC %in% c("98", "99", "Unknown")] <- NA

names(df)[names(df) == "Year"] <- "year"
df$inc_yy[df$inc_yy %in% c(217, 22017, 2917)] <- 2017

df$case_no[df$case_no == 210800235] <- 201800235
df$year2 <- substr(df$case_no, 1, 4)

df$case_no <- paste0(df$DI_MA, df$case_no)

names(df)[names(df) == "gender"] <- "sex"
names(df)[names(df) == "ethnic"] <- "ethnicity"

df$sex[df$sex==11] <- "Male" # https://web.archive.org/web/20170216084305/https://hk.on.cc/hk/bkn/cnt/news/20170216/bkn-20170216041803551-0216_00822_001.html

df$age <- as.integer(df$age)

df[, c('year',
       'cert_dd', 'cert_mm', 'cert_yy',
       'inc_dd', 'inc_mm', 'inc_yy',
       'age',
       'birth_dd', 'birth_mm', 'birth_yy')] <- apply(df[, c('year',
                                                            'cert_dd', 'cert_mm', 'cert_yy',
                                                            'inc_dd', 'inc_mm', 'inc_yy',
                                                            'age',
                                                            'birth_dd', 'birth_mm', 'birth_yy')], 2, function(x) as.numeric(as.character(x)))


# family issues ----
df$FAM <- ifelse(df$FAM %in% c('No', NA) &
                   (!(df$FAM_1 %in% c(NA, 'No Problem')) | !(df$FAM_2 %in% c(NA, 'No Problem')))
                 , 'Yes', df$FAM)

df$FAM <- ifelse(df$FAM=='Yes' & df$FAM_1=='No Problem' & !is.na(df$FAM_1), 'No', df$FAM) # FAM_1 as 'No Problem' should mean no family problems at all.


# work issues ----
df$WORK <- ifelse(df$WORK %in% c('No', NA) & 
                    (!(df$WORK_1 %in% c(NA, 'No Problem')) | !(df$WORK_2 %in% c(NA, 'No Problem', 'Other financial issues (pls specify)'))) # financial issues shouldn't belong here
                      , 'Yes', df$WORK)

df$WORK <- ifelse(df$WORK=='Yes' & df$WORK_1=='No Problem' & !is.na(df$WORK_1), 'No', df$WORK) # WORK_1 as 'No Problem' should mean no work problems at all.

# financial issues ----
df$FIN_1 <- ifelse(df$FIN=='71', 'Gambling', df$FIN_1) # it's probably Gambling when coded as 71

df$FIN_1 <- car::recode(df$FIN_1, "
c('90') = 'Debts';
c(' ') = NA;
'Nature unknown' = 'Unknown financial issue nature'
")

df$FIN <- ifelse(df$FIN=='Yes' & df$FIN_1=='No Problem' & !is.na(df$FIN_1), 'No', df$FIN) # FIN_1 as 'No Problem' should mean no financial problems at all

df$FIN <- car::recode(df$FIN, "
c('71') = 'Yes';
c('888', 'Missing', ' ') = NA;
")

df$FIN <- ifelse(df$FIN=='No' & !(df$FIN_1 %in% c(NA, 'No Problem')), 'Yes', df$FIN)

# abuses  ----
df$ABU <- ifelse(df$ABU %in% 'Yes' & df$ABU_1 %in% 'No Problem', 'No', df$ABU) 

# year of suicide death ----
df$year <- ifelse(df$year!=df$inc_yy & !is.na(df$inc_yy), df$inc_yy, df$year)
df$year <- ifelse(df$year!=df$cert_yy & !is.na(df$cert_yy) & df$cert_yy > 3, df$year2, df$year)
df$year2 <- NULL

df$year <- ifelse(df$cert_yy!= df$year &  !is.na(df$cert_yy) & df$cert_y > 3, df$inc_yy, df$year)

df$year <- ifelse(df$year<2002, 2002, df$year)
df$year <- ifelse(df$year>2017, 2017, df$year)

df$year <- ifelse(df$year>2017, 2017, df$year)

df$cert_yy <- df$year


df$birth_yy <- plyr::mapvalues(df$birth_yy, 
                               c(  26, 2008, 2010, 2014, 2015,  137, 11977, 1040, 1046, 1060), 
                               c(1981, 1936, 1974, 1958, 1960, 1937,  1977, 1940, 1946, 1960))

df$birth_yy <- ifelse(df$birth_yy==2017 & df$age==65 , 1951, df$birth_yy) 
df$birth_yy <- ifelse(df$birth_yy==2017 & df$age==62 , 1954, df$birth_yy)

# df$dob <-  as.Date(paste(df$birth_dd,
#                          df$birth_mm,
#                          df$birth_yy, sep = "/"), format = "%m/%d/%Y") # DoB
# 
# df$cert_date <-  as.Date(paste(df$cert_dd,
#                          df$cert_mm,
#                          df$cert_yy, sep = "/"), format = "%m/%d/%Y") # Date of certified death
# 
# df$age2 <- as.numeric(floor((df$cert_date-df$dob)/365.2425)) # 487 of them have a different calculated age than as reported, the absolute difference is more than 1 for 16 of them
# df$age <- ifelse(df$age!=df$age2, df$age2, df$age)

# duplicate cases/IDs ----
# "MA201708448" - two cases with same case number but different persons (confirmed with student suicides page on wikipedia)
df$case_no[df$case_no == "MA201708448" & df$age == 11] <- "MA201708448a"
df$case_no[df$case_no == "MA201708448" & df$age == 17] <- "MA201708448b"
df <- distinct(df, case_no, .keep_all = TRUE) # remove case number duplicates

df <- df[-which(df$id_pass_CC== "H4113006" & df$DI_MA=="MA"),] # remove duplicate ID with less info

# df$num_complete <- rowSums(!is.na(df)) # number of columns with non-NA values 

# nationality data cleaning ----
df$nationality[which(df$nationality=="Non-Chinese (please specify)")] <- "Non-Chinese"
df$ethnicity[which(df$ethnicity=="Non-Chinese (please specify)")] <- "Non-Chinese"

df$oth_nationality <- ifelse(df$oth_nationality %in% c(1, 87, 89, 9, 98), NA, df$oth_nationality) # missing

df$nationality <- ifelse(df$nationality %in% c(97), "Non-Chinese", df$nationality) # Non-Chinese based on oth_nationality

df$oth_nationality <- car::recode(df$oth_nationality, "
c('Amercian', 'America', 'American', 'United States of America') = 'American';
c('bangladeshi') = 'Bangladesh';
c('Belgian', 'BELGIUM') = 'Belgian';
c('british', 'British', 'BRITISH', 'English') = 'British';
c('British, Nepalese', 'Nepalese') = 'Nepalese';
c('Burundi', 'Nigerian') = 'African';
c('Filipino', 'FILIPINO', 'Filipinos', 'Fillipino') = 'Filipino';
c('Franch', 'French') = 'French';
c('Indian', 'INDIAN') = 'Indian';
c('Indonesia', 'indonesian', 'Indonesian', 'INDONESIAN') = 'Indonesian';
c('Janpanese', 'Janpanses', 'Japanese') = 'Japanese';
c('korean', 'Korean', 'South Korean') = 'Korean';
c('Myanmar') = 'Burmese';
c('New zealand', 'New Zealander') = 'New Zealander';
c('NORWEGIAN') = 'Norwegian';
c('Pakistani', 'Parkistan') = 'Pakistani';
c('Singaorean', 'Singapore', 'singaporean', 'Singaporean') = 'Singaporean';
c('Sri Landka') = 'Sri Landkan';
c('Thai', 'THAI') = 'Thai';
c('Unknown') = 'Other Non-Chinese';
c('Vietnam', 'Vietnamese') = 'Vietnamese';
")

# ethnicity data cleaning ----
df$id_prefix <- gsub("[^A-Za-z]","", substring(df$id_pass_CC,1, 4))

df$oth_ethni[which(df$oth_ethni==98)] <- NA # 98 means not applicable because they are coded as Chinese in the ethnicity variable, with a few exceptions

df$oth_ethni[which(df$oth_ethni==1 & df$case_no=="DI201000787")] <- NA # HKID prefix is "Z"
df$ethnicity[which(df$case_no=="DI201000787")] <- "Chinese" # HKID prefix is "Z"

df$ethnicity[which(df$case_no=="MA201209437")] <- "Chinese" # Her nationality is Chinese, born in Hong Kong, most likely Chinese

df$oth_ethni[which(df$oth_ethni==87 & df$ethnicity=="Chinese")] <- NA
df$oth_ethni[which(df$oth_ethni==99 & df$ethnicity=="Chinese")] <- NA

df$oth_ethni[which(df$oth_ethni==99 & df$oth_nationality=="Canadian")] <- "Canadian"
df$oth_ethni[which(df$oth_ethni=="Nan Hai" & df$oth_nationality=="Vietnamese")] <- "Vietnamese"

df$oth_ethni <- ifelse(is.na(df$ethnicity) & df$bornarea=="Other", df$oth_born, df$oth_ethni)  # assume ethnicity from where born
df$ethnicity[which(is.na(df$ethnicity) & df$bornarea=="Other")] <- "Non-Chinese" # most likely non-Chinese

df$ethnicity[which(is.na(df$ethnicity) & df$nationality=="Chinese" & nchar(df$id_prefix)==1)] <- "Chinese" # most likely Chinese from nationality unless id prefix has more than one letter

df$oth_ethni <- ifelse(is.na(df$ethnicity) & nchar(df$id_prefix)>1, df$oth_nationality, df$oth_ethni)  # assume ethnicity from nationality
df$ethnicity[which(is.na(df$ethnicity) & nchar(df$id_prefix)>1)] <- "Non-Chinese" # most likely Chinese according to id prefix 

df$ethnicity[which(df$ethnicity!="Chinese" & df$id_prefix=="W")] <- "Non-Chinese" # foreign (domestic) workers who weren't Chinese
# df$oth_ethni <- ifelse(df$ethnicity!="Chinese" & df$id_prefix=="W" & is.na(df$oth_ethni), df$oth_nationality, df$oth_ethni) # foreign (domestic) workers who weren't Chinese

df$ethnicity[which(df$nationality=="Non-Chinese" & nchar(df$id_prefix)>1)] <- "Non-Chinese" # recoded as Non-Chinese ethnicity according to id prefix & nationality
df$oth_ethni <- ifelse(is.na(df$oth_ethni) & df$nationality=="Non-Chinese" & nchar(df$id_prefix)>1, df$oth_nationality, df$oth_ethni) # recoded as Non-Chinese ethnicity according to id prefix & nationality
# df$oth_ethni <- ifelse(df$ethnicity=="Non-Chinese" & nchar(df$id_prefix)>1 & is.na(df$oth_ethni), df$oth_nationality, df$oth_ethni) # recoded as Non-Chinese ethnicity according to id prefix & ethnicity

df$ethnicity <- ifelse(df$ethnicity==97, "Non-Chinese", df$ethnicity) # ethnicity = 97 are non-Chiense
df$ethnicity <- ifelse(df$ethnicity %in% c(3, 4, 11, 98) & df$nationality=="Chinese", "Chinese", df$ethnicity) # most likely Chinese ethnicity (either born in HK/China/Macau)

df$ethnicity <- ifelse(is.na(df$ethnicity) & df$nationality=="Non-Chinese" & df$bornarea %in% c("Hong Kong", "Mainland China"), "Chinese", df$ethnicity) # missing ethnicity born in HK/China are assumed Chinese
df$ethnicity <- ifelse(is.na(df$ethnicity) & df$nationality=="Chinese", "Chinese", df$ethnicity) # missing ethnicity with Chinese nationality are assumed Chinese
df$oth_ethni <- ifelse(is.na(df$ethnicity) & df$nationality=="Non-Chinese" & is.na(df$bornarea), df$oth_nationality, df$oth_ethni) # missing ethnicity with non-Chinese nationality and no place of birth are assumed non-Chinese
df$ethnicity <- ifelse(is.na(df$ethnicity) & df$nationality=="Non-Chinese" & is.na(df$bornarea), "Non-Chinese", df$ethnicity) # missing ethnicity with non-Chinese nationality and no place of birth are assumed non-Chinese

df$oth_ethni[which(is.na(df$oth_ethni) & df$ethnicity=="Non-Chinese" & nchar(df$id_prefix)>1)] <- "Other Non-Chinese" # hkid with double prefix

df$ethnicity <- ifelse(is.na(df$ethnicity) & df$bornarea %in% c("Hong Kong", "Mainland China"), "Chinese", df$ethnicity) # missing ethnicity born in HK/China are assumed Chinese

df$oth_ethni[which(df$case_no=="DI20050264")] <- 'Filipino' # born in the Philippines
df$ethnicity[which(df$case_no=="DI20050264")] <- "Non-Chinese" # born in the Philippines

df$oth_ethni[which(df$case_no=="DI20060052")] <- 'Korean' # born in the Philippines
df$ethnicity[which(df$case_no=="DI20060052")] <- "Non-Chinese" # born in the Philippines

df$oth_ethni[which(is.na(df$oth_ethni) & df$ethnicity=="Non-Chinese")] <- 'Other Non-Chinese' # born in the Philippines

df$oth_ethni <- car::recode(df$oth_ethni, "
c('African', 'Burundi', 'Nigerian') = 'African';
c('America', 'American', 'USA') = 'American';
c('bangladeshi') = 'Bangladeshi';
c('Belgian', 'BELGIUM') = 'Belgian';
c('british', 'British', 'BRITISH', 'English', 'Britain') = 'British';
c('Cansasian', 'Caucasian', 'Caucasion') = 'Caucasian';
c('Filipino', 'FILIPINO', 'Filipinos', 'Fillipino', 'Philipino', 'British-Filipino', 'Philipine', 'Pilipino') = 'Filipino';
c('France', 'French') = 'French';
c('Indian', 'INDIAN') = 'Indian';
c('Indonesia', 'Indonesiam', 'indonesian', 'Indonesian', 'INDONESIAN') = 'Indonesian';
c('Janpanese', 'Janpanses', 'Japanese', 'Japanses') = 'Japanese';
c('korean', 'Korean', 'South Korean') = 'Korean';
c('Malayasian Chinese', 'Malaysian') = 'Malaysian';
'NORWEGIAN' = 'Norwegian';
c('Pakistani', 'Pakistsni', 'Parkistan') = 'Pakistani';
c('singaporean', 'Singaporean', 'Singaporian') = 'Singaporean';
c('Sinhalese') = 'Sri Landkan';
c('south east asian', 'South East Asian') = 'South East Asian';
c('Thai', 'THAI', 'thai') = 'Thai';
c('Nan Hai', 'Vietnamese', 'Vietnemese', 'Vietnam') = 'Vietnamese'
")


# place of birth data cleaning ----
df$oth_born <- ifelse(df$oth_born %in% c(9, 98, 99), NA, df$oth_born) # missing
df$oth_born <- ifelse(df$oth_born=='Guangzhou', NA, df$oth_born) # most Mainland Chinese don't provide specific place of birth
df$bornarea <- ifelse(df$oth_born %in% c('Italy', 'Japan', 'Phillipines', 'United States of America', 'Iloilo city', 'British') 
                      & df$bornarea %in% c('Hong Kong', 'Macau', 'Mainland China'), 'Other', df$bornarea) # Most likely mis-coded bornarea
df$bornarea <- ifelse(df$bornarea %in% c(5), 'Mainland China', df$bornarea) # most likely Mainlander based on Chinese ethnicity & id_prefix of 'P'

df$year_hk <- ifelse(df$year_hk=='Born in HK' & df$bornarea=='Other', NA, df$year_hk) # born in Thailand with id_prefix of "XE" 
df$ethnicity <- ifelse(df$case_no=='DI201000236', 'Non-Chinese', df$ethnicity)  # born in Thailand with id_prefix of "XE" 
df$oth_ethni <- ifelse(df$case_no=='DI201000236', 'Thai', df$oth_ethni)  # born in Thailand with id_prefix of "XE" 

df$bornarea <- ifelse(df$id_prefix %in% c("Z", "Y") & df$type_id_pass=='ID card' & df$bornarea!='Hong Kong', "Hong Kong", df$bornarea) # id_prefix of Z, Y, S, N are HK-born except already coded as other (HK-born with "N" prefix are way too young (born after 2019), HK-born with "S" prefix are very young and already coded properly)

df$bornarea <- ifelse(df$bornarea==98 & df$nationality=="Chinese", "Mainland China", df$bornarea) # most likely mainlander based on nationality and id_prefix

df$year_hk <- ifelse(df$year_hk=='Born in HK' 
                     & df$bornarea %in% c('Macau', 'Mainland China'), NA, df$year_hk) # they were probably not born in HK based on bornarea (and their id_prefix)

df$year_hk <- ifelse(df$bornarea %in% c('Hong Kong'), 'Born in HK', df$year_hk)
df$year_hk <- ifelse(df$year_hk %in% c(89, 998, 97), NA, df$year_hk)

df$bornHK <- ifelse(df$year_hk=='Born in HK', 1, 
                    ifelse(is.na(df$year_hk), NA, 0))

df$year_hk <- ifelse(df$year_hk %in% c('Born in HK'), NA, df$year_hk) # only keeping years in HK, born in HK coded as NA


df$oth_born <- car::recode(df$oth_born, "
c('Africa', 'Ethiopia') = 'Africa';
c('America', 'United States', 'United States of America', 'US', 'USA') = 'US';
c('Australia', 'AUSTRALIA', 'Australian') = 'Australia';
c('bangladeshi') = 'Bangladesh';
c('BARUM') = 'Norway';
c('Belgium', 'BELGIUM', 'Belgian') = 'Belgium';
c('britain', 'Britain', 'British', 'England', 'London', 'Scotland', 'UK', 'United Kingdom') = 'UK';
c('Bulgaria') = 'Bulgaria';
c('Burnese', 'Myanmar') = 'Myanmar';
c('Cahors', 'Draguignan', 'France') = 'France';
c('Cambodia') = 'Cambodia';
c('Canada') = 'Canada';
c('Iloilo city', 'Philiphines', 'Philipines', 'Philipino', 'Philippine', 'Philippines', 'PHILIPPINES', 
'Philippino', 'Phillipines', 'Phillippine', 'Pilippines', 'The philipines', 'The Philipines', 'the Philippines', 
'The philippines', 'The Philippines', 'Villapaz Naguilian Isabela') = 'Philippines';
c('indonesia', 'indonesia`', 'Indonesia', 'INDONESIA', 'indonesian', 'Indonesian') = 'Indonesia';
c('Korea', 'Korean', 'Republic of Korea', 'south korea', 'South Korea') = 'Korea';
c('Malasia', 'Malayasia', 'Malaysia') = 'Malaysia';
c('Pakistan', 'Parkistan') = 'Pakistan';
c('singapore', 'Singapore') = 'Singapore';
c('thailand', 'Thailand') = 'Thailand';
c('vietnam', 'Vietnam', 'Vietnanmese') = 'Vietnam'
")

# prepare oth_ethni for imputation
df$oth_ethni <- ifelse(df$oth_ethni=='Other Non-Chinese', NA, df$oth_ethni)

# ICD_10 & diemth_1 data cleaning ----
df$icd_10 <- ifelse(df$icd_10 %in% c('X\67'), 'X67.0', df$icd_10)
df$icd_10 <- ifelse(df$icd_10 %in% c('X\\67'), 'X\\67', df$icd_10)
df$icd_10 <- ifelse(df$icd_10 %in% c('99'), NA, df$icd_10)
df$icd_10 <- car::recode(df$icd_10, "
c('78') = 'X78';
")

df$diemth <- df$diemth_1
df$diemth <- ifelse(df$diemth %in% c(0, 49, 'Others', NA),  df$icd_10, df$diemth)
df$diemth <- ifelse(df$diemth %in% c('Others') & grepl('ump',df$oth_m_1, ignore.case = TRUE),  'Jumping', df$diemth) # jump from non-buildings (there's a mispelling "hump out bus")
df$diemth <- ifelse(grepl('rail',df$oth_m_1, ignore.case = TRUE),  'Railway', df$diemth) 
df$diemth <- ifelse(grepl('ferry',df$oth_m_1, ignore.case = TRUE),  df$icd_10, df$diemth) # Drowning
df$diemth <- ifelse(grepl('bus',df$oth_m_1, ignore.case = TRUE),  'Others', df$diemth)
df$diemth <- ifelse(grepl('stab',df$oth_m_1, ignore.case = TRUE),  'Cutting', df$diemth) 
df$diemth <- car::recode(df$diemth, "
c('Hanging', 'Suffocation') = 'Hanging/Suffocation';
c('Gassing (other than CO)', 'CO poisoning - charcoal', 'CO poisoning - exhaust gas inside car', 'CO poisoning - charcoal inside car') = 'Poisoning by carbon monoxide';
c('Poisoning (chemical other than medication & drugs)') = 'Poisoning by other chemicals'
")
df$diemth <- car::recode(df$diemth, "
c('X67.0', 'X67.5') = 'Poisoning by carbon monoxide';
c('X60.0', 'X61.8', 'X64.0') = 'Drug overdose';
c('X68.0') = 'Poisoning by other chemicals';
c('X70.0', 'X70.1', 'X70.8') = 'Hanging/Suffocation';
'X71.8' = 'Drowning';
'X76.8' = 'Burning';
c('X78.0', 'X78.1', 'X78.2', 'X78.8') = 'Cutting';
c('X80.0', 'X80.2', 'X80.4', 'X80.5', 'X80.8', 'X80.9') = 'Jumping';
c('X81.5', 'X81.4', 'X81.8') = 'Railway';
c('X83.0', 'X83.2', 'X83.4', 'X84.0', 'X84.8') = 'Others'
") # ICD-10 code is used to categorise diemth when coded as 'Others'
df$diemth <- ifelse(is.na(df$diemth), 'Others', df$diemth) # NA -> 'Others'

# employment status ----
df$occup_group <- ifelse(df$occup_group %in% c(87, 97, 9898), NA, df$occup_group) # retirees
df$income <- ifelse(df$income %in% c('No income'), 0, df$income) 

df$income3 <- df$income
df$income <- ifelse(df$income %in% c('Not fixed'), NA, df$income)
df$income <- as.numeric(df$income)
df$income <- ifelse(df$income > 0 & df$income <= 98, NA, df$income) 

df$income2 <- ifelse(df$income > 0, "Positive", 
                     ifelse(df$income == 0, "Zero income", NA))

df$income3 <-  ifelse(df$income3 == 'Not fixed', 'Not fixed', df$income2)

# df$less_than_4000 <- ifelse(df$income <= 4000 & !is.na(df$income), 'Yes', ifelse(is.na(df$income), NA, 'No'))

df$occup <- ifelse(df$occup %in% c(0, 1, 98, 99, 9888, 'Unknown', 'Employed'), NA, df$occup)
df$employ_status <- ifelse(df$employ_status %in% c(98, 997) & !is.na(df$occup), 'Other employed', df$employ_status) # 2 people employed with income
df$employ_status <- ifelse(df$employ_status %in% c(0, 98, 997), NA, df$employ_status)

df$employ_status7 <- car::recode(df$employ_status, "
c('Full-time employed', 'Self-employed', 'Part-time employed', 'Other employed') = 'Employed';
c('Unemployed') = 'Unemployed';
c('Underemployed') = 'Underemployed';
c('Retired') = 'Retired';
c('Homemaker') = 'Homemaker';
c('Full-time student') = 'Full-time student';
c('Others', 'Disability') = 'Others';
'Unknown' = 'Unknown'
")

df$employ_status7 <- ifelse(df$employ_status7 %in% c('Unknown'), NA, df$employ_status7)

df$employ_status7b <- ifelse(df$employ_status7 %in% c('Underemployed'), 'Unemployed', df$employ_status7)

## OCCUPATIONS ## ----

# check relationship between occup_group & occup
# occup_dat <- data.frame(matrix(ncol = 0,  nrow = 46))
# occup_dat$occup_group<- rep(1:46)
# for (i in (1:46)) { # get all the possible values of occup for each occup_group
#  occup_dat$occup[which(occup_dat$occup_group==i)]  <- paste0(unique(df$occup[which(df$occup_group==i & !is.na(df$occup))]), collapse=", ")
# }
# write_excel("occup_dat.xlsx", occup_dat)

# recoding/grouping top occupations
# most common occupations----
# convert all occupations to lowercase
df$occup <- tolower(df$occup)
# domestic helper
df$occup <- ifelse(grepl('mestic',
                         df$occup, ignore.case = TRUE) & 
                     !grepl('clerk',df$occup, ignore.case = TRUE) & 
                     !grepl('medicine', df$occup, ignore.case = TRUE),  
                   'Domestic helper', df$occup) 
# homemaker/housewife
df$occup <- ifelse(grepl('housewife|housekeeper',
                         df$occup, ignore.case = TRUE) |
                     df$employ_status=='Homemaker',  'Homemaker', df$occup) 
# student
df$occup <- ifelse(grepl('student|self-study',
                         df$occup, ignore.case = TRUE) |
                     df$employ_status=='Full-time student', 'Student', df$occup) 
df$occup <- ifelse(df$employ_status=='Self-employed' & is.na(df$occup), 'Self-employed', df$occup) 
# security guard
df$occup <- ifelse(grepl('security|guard|保安員|watchman', df$occup, ignore.case = TRUE) & 
                     !grepl('sales|trading|manager|supervisor|consultant|trader|waitress|life|guardian', df$occup, ignore.case = TRUE),
                   'Security guard', df$occup) 
# cleaner
df$occup <- ifelse(grepl('clean', df$occup, ignore.case = TRUE) &
                     !grepl('owner|director|cook|shareholder|dish|catering|proprieter|manager', df$occup, ignore.case = TRUE),
                   'Cleaning worker', df$occup) 
# teacher
df$occup <- ifelse(grepl('teach', df$occup, ignore.case = TRUE) &
                     !grepl('waitress', df$occup, ignore.case = TRUE),
                   'Teacher', df$occup) 
# tutor
df$occup <- ifelse(grepl('tutor', df$occup, ignore.case = TRUE) &
                     !grepl('wine', df$occup, ignore.case = TRUE),
                   'Tutor', df$occup) 
# driver
df$taxi <- ifelse(grepl('driv', df$occup, ignore.case = TRUE) & 
                    grepl('taxi', df$occup, ignore.case = TRUE),
                  1, 0)
df$occup <- ifelse(grepl('driv', df$occup, ignore.case = TRUE) & 
                     grepl('taxi|uber|coach|self-employ|private|chauffeur',
                         df$occup, ignore.case = TRUE),
                   'Taxi/private driver', df$occup) 
df$occup <- ifelse(grepl('driv|captain', df$occup, ignore.case = TRUE) & 
                     grepl('tram|PLB|bus|maxicab', df$occup, ignore.case = TRUE) & 
                     !grepl('taxi', df$occup, ignore.case = TRUE),
                   'Public transport driver', df$occup) 
df$occup <- ifelse(grepl('driv', df$occup, ignore.case = TRUE) & 
                     !grepl('public transport', df$occup, ignore.case = TRUE) & 
                     !grepl('taxi', df$occup, ignore.case = TRUE),
                   'Truck/other driver', df$occup) 

# salesperson
df$occup <- ifelse(grepl('sale|sales executive|retail|merchant|distribution agent|red wine|trader', df$occup, ignore.case = TRUE) & 
                     !grepl('manager|supervisor|owner|waitress|security|waiter|teacher|wholesale|teacher', df$occup, ignore.case = TRUE),
                   'Salesperson', df$occup)
# shop worker
df$occup <- ifelse(grepl('shop assistant|store worker|shop worker|supermarket|stall|dispensary|屈臣氏|bike shop', df$occup, ignore.case = TRUE) &
                     !grepl('workshop', df$occup, ignore.case = TRUE),
                   'Shopkeeper', df$occup)
# shopkeeper
df$occup <- ifelse(grepl('keeper', df$occup, ignore.case = TRUE) &
                     !grepl('ware|stock|house|horsekeeper', df$occup, ignore.case = TRUE),
                   'Shopkeeper', df$occup)
# baker
df$occup <- ifelse(grepl('bak', df$occup, ignore.case = TRUE),
                   'Baker', df$occup)
# hotel worker
df$occup <- ifelse(grepl('hotel|hostel', df$occup, ignore.case = TRUE) &
                     !grepl('cook|waiter|reception|manager', df$occup, ignore.case = TRUE),
                   'Hotel staff', df$occup)
# hawker
df$occup <- ifelse(grepl('hawker', df$occup, ignore.case = TRUE) &
                     !grepl('control', df$occup, ignore.case = TRUE),
                   'Hawker', df$occup)
# clerk
df$occup <- ifelse(grepl('clerk|administrative assistant|clerical|office assistant|assistant admin|white collar', df$occup, ignore.case = TRUE),
                   'Clerk', df$occup)
# secretary
df$occup <- ifelse(grepl('secretary|personal assistant', df$occup, ignore.case = TRUE),
                   'Secretary', df$occup)
# artisan/technician (maintenance, operators, mechanic, repair)
df$occup <- ifelse(grepl(paste0('garage|eletrical|serviceman|seviceman|instal|metal|fix|',
                               'plumb|renovat|welding|decorat|workman|artisan|technician|',
                               'electrician|electricity worker|technican|contractor|repair|technical officer trainee',
                               'mechanic|pipefitter|fitter|crane|maint|operator|switchboard|road-Surfacing'), df$occup, ignore.case = TRUE) &
                     !grepl('health|computer|waiter|massage', df$occup, ignore.case = TRUE),
                   'Technicians/Artisans', df$occup)

# professionals ----
# supervisor/foreman/assistant manager (lower management)
df$occup <- ifelse(grepl('supervisor|superviser|superisor|sepervisor|foreman|co-ordinator|corrdinator|executive officer|admin. dept|deputy financial controller|負責人|主任', df$occup, ignore.case = TRUE) | 
                     (grepl('assistant|assistent|assist', df$occup, ignore.case = TRUE) &
                     grepl('manager', df$occup, ignore.case = TRUE) & 
                     !grepl('assistant of', df$occup, ignore.case = TRUE)),
                   'Supervisor', df$occup)
# manager (middle management)
df$occup <- ifelse(grepl('financial controller|bank executive|manager|senior manager|assistant vice president|associate director|經理', df$occup, ignore.case = TRUE) &
                     !grepl('deputy financial controller|general manager|manager director|hotel manager|assistant of|insurance agent|manager of a band', df$occup, ignore.case = TRUE),
                   'Manager', df$occup)
df$occup <- ifelse(grepl('deputy general manager|vice general manager|assistant chairman', df$occup, ignore.case = TRUE),
                   'Manager', df$occup)
# top-level manager
df$occup <- ifelse(grepl('CEO|chairman|director|managing|general manager|vice president|manager director|hotel manager|chief engineer', df$occup, ignore.case = TRUE) |
                     (grepl('chief', df$occup, ignore.case = TRUE) &
                     grepl('officer', df$occup, ignore.case = TRUE) &
                     !grepl('assistant to', df$occup, ignore.case = TRUE)),
                   'Top-level manager', df$occup)
# business owner/person
df$occup <- ifelse(grepl('owner|proprietor|proprirtor|proprietress|boss|entrepreneur|in-charge|running|run a|東主|manufacturer|shareholder', df$occup, ignore.case = TRUE) |
                     (grepl('business|bussiness', df$occup, ignore.case = TRUE) & 
                        grepl('man|woman', df$occup, ignore.case = TRUE)) |
                     (grepl('Operated|operating', df$occup, ignore.case = TRUE) &
                        !grepl('husband', df$occup, ignore.case = TRUE)) |
                     (grepl('business', df$occup, ignore.case = TRUE) &
                        !grepl('analyst', df$occup, ignore.case = TRUE)),
                   'Business owner/person', df$occup)
# nurse
df$occup <- ifelse((grepl('nurse|nursing', df$occup, ignore.case = TRUE) &
                     !grepl('nursery|assistant', df$occup, ignore.case = TRUE)) |
                     grepl('assistant nursing officer', df$occup, ignore.case = TRUE),
                   'Nurse', df$occup)
# health care worker
df$occup <- ifelse(grepl('anaesthetist|dental nurse|nursing assistant|health|caretaker|carer|care taker|care worker|elderly|aged|rehab', df$occup, ignore.case = TRUE) &
                     !grepl('inspector|surv|management|shenzhen|mental health association', df$occup, ignore.case = TRUE) |
                     (grepl('therapy', df$occup, ignore.case = TRUE) &
                     grepl('assist', df$occup, ignore.case = TRUE)),
                   'Healthcare worker', df$occup)
# physio/occupational therapist
df$occup <- ifelse(grepl('therap', df$occup, ignore.case = TRUE) &
                     !grepl('assist', df$occup, ignore.case = TRUE),
                   'Physical/occupational therapist', df$occup)
# chinese medicine practitioner
df$occup <- ifelse(grepl('practitioner|herbalist', df$occup, ignore.case = TRUE),
                   'Chinese Medicine Practitioner', df$occup)
# dentist
df$occup <- ifelse(grepl('dental surgeon|dentist', df$occup, ignore.case = TRUE),
                   'Dentist', df$occup)
# pharmacist
df$occup <- ifelse(grepl('pharmac|dispenser', df$occup, ignore.case = TRUE),
                   'Pharmacist', df$occup)
# IT worker
df$occup <- ifelse(grepl('tech|operat', df$occup, ignore.case = TRUE) & 
                     grepl('comput', df$occup, ignore.case = TRUE) |
                     grepl('I.T. industry worker|IT officer|IT worker|IT tech|I.T. tech|I.T. officer|website operator|\\bI.T.\\b', df$occup, ignore.case = TRUE),
                   'IT worker', df$occup)
# computer programmer
df$occup <- ifelse(grepl('programmer', df$occup, ignore.case = TRUE),
                   'Computer programmer', df$occup)
# engineer
df$occup <- ifelse(grepl('engineer', df$occup, ignore.case = TRUE),
                   'Engineer', df$occup)
# banker
df$occup <- ifelse(grepl('bank|teller', df$occup, ignore.case = TRUE) &
                     !grepl('investment|analyst', df$occup, ignore.case = TRUE),
                   'Bank teller/banker', df$occup)
# analyst
df$occup <- ifelse(grepl('analyst|data|research', df$occup, ignore.case = TRUE) &
                     !grepl('professor|researcher', df$occup, ignore.case = TRUE),
                   'Analyst', df$occup)
# professor
df$occup <- ifelse(grepl('prof|researcher', df$occup, ignore.case = TRUE) &
                     !grepl('Chinese Medicine', df$occup, ignore.case = TRUE),
                   'Professor', df$occup)
# real estate agent
df$occup <- ifelse(grepl('property|estate|prosperity', df$occup, ignore.case = TRUE) &
                     !grepl('investor|management|officer', df$occup, ignore.case = TRUE),
                   'Real estate agent', df$occup)
# insurance agent
df$occup <- ifelse(grepl('insurance', df$occup, ignore.case = TRUE) &
                     !grepl('translator|manager', df$occup, ignore.case = TRUE),
                   'Insurance agent', df$occup)
# securities broker
df$occup <- ifelse(grepl('broker|dealer|agent|settlement', df$occup, ignore.case = TRUE) &
                     grepl('stock|share|securit|future|invest|financ', df$occup, ignore.case = TRUE) &
                     !grepl('guard|insurance', df$occup, ignore.case = TRUE),
                   'Securities broker', df$occup)
# investor
df$occup <- ifelse(grepl('investor|investment', df$occup, ignore.case = TRUE) &
                     !grepl('agent|banker|advisor', df$occup, ignore.case = TRUE),
                   'Investor', df$occup)
# marketer
df$occup <- ifelse(grepl('market|promoter|advertising', df$occup, ignore.case = TRUE) &
                     !grepl('wholesa', df$occup, ignore.case = TRUE),
                   'Marketer', df$occup)
# designer
df$occup <- ifelse(grepl('designer', df$occup, ignore.case = TRUE) &
                     !grepl('barber|manager', df$occup, ignore.case = TRUE),
                   'Designer', df$occup)
# merchandiser
df$occup <- ifelse(grepl('merchandiser', df$occup, ignore.case = TRUE),
                   'Merchandiser', df$occup)
# consultant
df$occup <- ifelse(grepl('consult|advisor|顧問', df$occup, ignore.case = TRUE) &
                     !grepl('site', df$occup, ignore.case = TRUE),
                   'Consultant', df$occup)
# reporter/journalist
df$occup <- ifelse(grepl('journalist|reporter', df$occup, ignore.case = TRUE),
                   'Journalist', df$occup)
# accountant
df$occup <- ifelse(grepl('account|audit', df$occup, ignore.case = TRUE) &
                     !grepl('executive|controller', df$occup, ignore.case = TRUE),
                   'Accountant', df$occup)
# editor/writer
df$occup <- ifelse(grepl('edit|writer|author', df$occup, ignore.case = TRUE),
                   'Editor/writer', df$occup)
# translator/interpreter
df$occup <- ifelse(grepl('translat|interpret', df$occup, ignore.case = TRUE) &
                     !grepl('insurance', df$occup, ignore.case = TRUE),
                   'Translator/interpreter', df$occup)
# human resources worker
df$occup <- ifelse(grepl('human resource', df$occup, ignore.case = TRUE),
                   'Human resources worker', df$occup)
# lawyer
df$occup <- ifelse(grepl('law|solicitor', df$occup, ignore.case = TRUE),
                   'Lawyer', df$occup)
# vet assistant
df$occup <- ifelse(grepl('vet|Reterinarian', df$occup, ignore.case = TRUE) &
                     grepl('assist', df$occup, ignore.case = TRUE),
                   'Veterinary assistant', df$occup)
# finance professional 
df$occup <- ifelse(df$occup %in% c('Bank teller/banker','Real estate agent', 
                                   'Insurance agent', 'Securities broker', 'Investor'),
                   'Finance professional', df$occup)

# skilled/semi-skilled occupations ----
# hairdresser
df$occup <- ifelse(grepl('barber|hair|salon', df$occup, ignore.case = TRUE) & 
                     !grepl('owner|chair|receptionist', df$occup, ignore.case = TRUE),
                   'Hairdresser', df$occup)
# cosmetologist
df$occup <- ifelse(grepl('cosmetologist|cosmetician|makeup|make-up|beautician', df$occup, ignore.case = TRUE) & 
                     !grepl('dog', df$occup, ignore.case = TRUE),
                   'Cosmetologist', df$occup)
# waiter/waitress
df$occup <- ifelse(grepl('waiter|waitress|witress', df$occup, ignore.case = TRUE) &
                     !grepl('sales|security|tutor|electrician', df$occup, ignore.case = TRUE),
                   'Waiter/Waitress', df$occup)
# contruction worker
df$occup <- ifelse(grepl('construct|constuction|cosntruction|scaffold|stone|CONSULTAION SITE|site-worker|cement', df$occup, ignore.case = TRUE) &
                     !grepl('surveyor|pipefitter|consultant|crane|safety', df$occup, ignore.case = TRUE),
                   'Construction worker', df$occup)
# safety officer
df$occup <- ifelse(grepl('safety officer', df$occup, ignore.case = TRUE),
                   'Safety officer', df$occup)
# surveyor
df$occup <- ifelse(grepl('survey', df$occup, ignore.case = TRUE),
                   'Surveyor', df$occup)
# factory worker
df$occup <- ifelse(grepl('factory|industry worker|industrial worker|fartory', df$occup, ignore.case = TRUE) &
                     !grepl('I.T.', df$occup, ignore.case = TRUE),
                   'Factory worker', df$occup)
# labourer
df$occup <- ifelse(grepl('labour', df$occup, ignore.case = TRUE) &
                     !grepl('safety|clerical|relation', df$occup, ignore.case = TRUE),
                   'Labourer', df$occup)
# chef/cooks
df$occup <- ifelse(grepl('chef|cook', df$occup, ignore.case = TRUE) &
                     !grepl('hawker', df$occup, ignore.case = TRUE),
                   'Cook/chef', df$occup)
# kitchen worker
df$occup <- ifelse(grepl('kitchen|dish', df$occup, ignore.case = TRUE) &
                     !grepl('ware', df$occup, ignore.case = TRUE),
                   'Kitchen worker', df$occup)
# restaurant worker
df$occup <- ifelse(grepl('rest|大快活|McDonald|fastfood|food stall|food shop|hospitality|canteen', df$occup, ignore.case = TRUE),
                   'Restaurant worker', df$occup)
# printing worker
df$occup <- ifelse(grepl('printing|newspaper', df$occup, ignore.case = TRUE),
                   'Printing worker', df$occup)
# airline worker
df$occup <- ifelse(grepl('flight|airline|aircraft|stewardess|ground crew', df$occup, ignore.case = TRUE) &
                     !grepl('pilot', df$occup, ignore.case = TRUE),
                   'Airline worker', df$occup)
# logistics worker
df$occup <- ifelse(grepl('logistic', df$occup, ignore.case = TRUE),
                   'Logistics worker', df$occup)
# tailor
df$occup <- ifelse(grepl('tailor', df$occup, ignore.case = TRUE) & 
                     !grepl('running', df$occup, ignore.case = TRUE),
                   'Tailor', df$occup)
# Sheltered workshop worker
df$occup <- ifelse(grepl('workshop', df$occup, ignore.case = TRUE),
                   'Sheltered workshop worker', df$occup)
# tour guide
df$occup <- ifelse(grepl('guide', df$occup, ignore.case = TRUE),
                   'Tour guide', df$occup)
# fitness trainer
df$occup <- ifelse(grepl('trainer|fitness', df$occup, ignore.case = TRUE),
                   'Fitness trainer', df$occup)
# masseuse
df$occup <- ifelse(grepl('masseuse', df$occup, ignore.case = TRUE),
                   'Masseuse', df$occup)
# loan shark
df$occup <- ifelse(grepl('loan', df$occup, ignore.case = TRUE),
                   'Loan shark worker', df$occup)
# funeral worker
df$occup <- ifelse(grepl('funeral|undertaker', df$occup, ignore.case = TRUE),
                   'Funeral worker', df$occup)
# estate/property worker
df$occup <- ifelse(grepl('estate|property management', df$occup, ignore.case = TRUE),
                   'Property management worker', df$occup)

# civil servants ----
# civil servant
df$occup <- ifelse(grepl('civil servant|department|housing authority|servant|housing officer|bureau', df$occup, ignore.case = TRUE) &
                     !grepl('head|inspector|meter-reader', df$occup, ignore.case = TRUE),
                   'Civil servant', df$occup)
# fireman
df$occup <- ifelse(grepl('fireman', df$occup, ignore.case = TRUE),
                   'Fireman', df$occup)
# correctional officer
df$occup <- ifelse(grepl('correctional|Assistant Officer I of CSD|Assistant officer \\(I\\)', df$occup, ignore.case = TRUE),
                   'Correctional officer', df$occup)
# customs officer
df$occup <- ifelse(grepl('custom|c&e|C and E|海關', df$occup, ignore.case = TRUE) &
                     !grepl('customer', df$occup, ignore.case = TRUE),
                   'Customs officer', df$occup)
# police officer
df$occup <- ifelse(grepl('police|superintendent of police|inspector|detective|sergeant|constable|警察', df$occup, ignore.case = TRUE) &
                     !grepl('health|food|c and e', df$occup, ignore.case = TRUE),
                   'Police officer', df$occup)
# ambulance officer
df$occup <- ifelse(grepl('ambul', df$occup, ignore.case = TRUE),
                   'Ambulance officer', df$occup)
# firefighter
df$occup <- ifelse(grepl('fireman', df$occup, ignore.case = TRUE),
                   'Firefighter', df$occup)
# lifeguard
df$occup <- ifelse(grepl('guard', df$occup, ignore.case = TRUE) &
                     grepl('life|safe', df$occup, ignore.case = TRUE),
                   'Lifeguard', df$occup)
# health inspector
df$occup <- ifelse(grepl('inspector|hawker', df$occup, ignore.case = TRUE) &
                     grepl('health|food|control', df$occup, ignore.case = TRUE),
                   'Health inspector', df$occup)
# health surveillance worker
df$occup <- ifelse(grepl('dept. of health|survenilance', df$occup, ignore.case = TRUE),
                   'Health surveillance worker', df$occup)

# other occupations ----
# delivery worker
df$occup <- ifelse(grepl('deliver|couri|messenger|DHL', df$occup, ignore.case = TRUE),
                   'Delivery worker', df$occup)
# postal worker
df$occup <- ifelse(grepl('post|分信員', df$occup, ignore.case = TRUE),
                   'Postal worker', df$occup)
# casual worker
df$occup <- ifelse(grepl('casual|causual', df$occup, ignore.case = TRUE),
                   'Casual worker', df$occup)
# receptionist
df$occup <- ifelse(grepl('receptionist', df$occup, ignore.case = TRUE),
                   'Receptionist', df$occup)
# transport/transportation worker
df$occup <- ifelse(grepl('transport', df$occup, ignore.case = TRUE) &
                     !grepl('decoration|public', df$occup, ignore.case = TRUE),
                   'Transport worker', df$occup)
# cashier
df$occup <- ifelse(grepl('cash|cshier', df$occup, ignore.case = TRUE),
                   'Cashier', df$occup)
# in jail
df$occup <- ifelse(grepl('jail', df$occup, ignore.case = TRUE),
                   'In jail', df$occup)
# in hospital
df$occup <- ifelse(grepl('hospital', df$occup, ignore.case = TRUE),
                   'In hospital', df$occup)
# sex worker
df$occup <- ifelse(grepl('sex|prostitute', df$occup, ignore.case = TRUE),
                   'Sex worker', df$occup)
# bartender
df$occup <- ifelse(grepl('bar', df$occup, ignore.case = TRUE) &
                     !grepl('manager|libarian|barge', df$occup, ignore.case = TRUE),
                   'Bartender', df$occup)
# warehouse/packing
df$occup <- ifelse(grepl('ware|packing|package|stock assistant|stock keeper|stocks|超級市場管貨員', df$occup, ignore.case = TRUE),
                   'Warehouse/packing worker', df$occup)
# porter
df$occup <- ifelse(grepl('porter', df$occup, ignore.case = TRUE) &
                     !grepl('supporter', df$occup, ignore.case = TRUE),
                   'Porter', df$occup)
# general worker
df$occup <- ifelse(grepl('general service|general worker', df$occup, ignore.case = TRUE) |
                     df$occup %in% c('worker', 'Worker'),
                   'General worker', df$occup)
# gardener
df$occup <- ifelse(grepl('gardener|garden worker', df$occup, ignore.case = TRUE),
                   'Gardener', df$occup)
# farmer
df$occup <- ifelse(grepl('farm', df$occup, ignore.case = TRUE),
                   'Farmer', df$occup)
# film crew
df$occup <- ifelse(grepl('production|extra', df$occup, ignore.case = TRUE),
                   'Film crew', df$occup)
# entertainer 
df$occup <- ifelse(grepl('singer|actress|entertainer', df$occup, ignore.case = TRUE),
                   'Entertainer', df$occup)
# nanny 
df$occup <- ifelse(grepl('nanny', df$occup, ignore.case = TRUE),
                   'Nanny', df$occup)
# public relations worker
df$occup <- ifelse(grepl('night club|PR officer', df$occup, ignore.case = TRUE),
                   'Public relations worker', df$occup)

## End of OCCUPATIONS ## ----
# mental health data cleaning ####
# nowpsy1 & psytreat
df$nowpsyprob <- df$nowpsy1
df$nowpsyprob <- ifelse(df$nowpsyprob %in% c(NA, 0, 89, 99),  NA, df$nowpsyprob)
df$nowpsyprob <- ifelse(df$nowpsyprob %in% c(NA) & df$psytreat %in% c('NA - no psychiatric problem'),  'NA - no psychiatric problem', df$nowpsyprob)
df$nowpsyprob <- ifelse(df$nowpsyprob %in% c(NA) & df$psytreat %in% c('Yes'),  'Received psychiatric treatment at the time of death', df$nowpsyprob)
df$nowpsyprob <- ifelse(df$nowpsyprob %in% c(NA), NA, 
                        ifelse(df$nowpsyprob %in% c('NA - no current psychiatric problem',
                                                    'NA - no psychiatric problem'), 'No', 'Yes'))

# psy, psy_tt_ever, psy_num &  psytreat
df$psyprob <- df$psy
df$psyprob <- ifelse(df$psyprob %in% c(NA) & df$psy_tt_ever %in% c('Yes', 'NA - no psychiatric problem'),  df$psy_tt_ever, df$psyprob)
df$psyprob <- ifelse(df$psyprob %in% c(NA) & df$psy_tt_ever %in% c(NA, 2, 11, 14, 96),  df$psy_num, df$psyprob)
df$psyprob <- ifelse(df$psyprob %in% c(NA) & df$psytreat %in% c('Yes'),  df$psytreat, df$psyprob)
df$psyprob <- car::recode(df$psyprob, "
c('No', 'NA - no psychiatric problem', 'None') = 'No';
c('Yes') = 'Yes';
")

# psytreat, psy_tt_ever
df$nowpsytreat <- df$psytreat
df$nowpsytreat <- ifelse(df$nowpsytreat %in% c(NA, 3, 4) & df$psy_tt_ever %in% c('Yes', 'No', 'NA - no psychiatric problem'),  df$psy_tt_ever, df$nowpsytreat)
df$nowpsytreat <- car::recode(df$nowpsytreat, "
c('NA - no psychiatric problem', 'None') = 'No psychiatric problem'
")

# medical condition data cleaning ----
df$medprob <- df$Med
df$medprob <- ifelse(df$medprob %in% c(NA, 4, 21, 98) ,  df$medprob1, df$medprob)
df$medprob <- ifelse(df$medprob %in% c(NA, 4, 21, 98) ,  df$medprob1, df$medprob)
df$medprob <- car::recode(df$medprob, "
c('NA - no physical illness', 'No', 'No medical problem') = 'No';
c('Had medical problem', 'Cancer, carcinoma, neoplasm', 'Angina', 0) = 'Yes'
")

# marital status & etc ----
df$cross_border2 <- ifelse(grepl('yes', df$cross_border, ignore.case = TRUE),
                   'Yes', df$cross_border)
df$cross_border2 <- car::recode(df$cross_border2, "
c('Yes') = '1';
c('No') = '0';
998 = NA
")

# education ----
df$edu <- ifelse(grepl('secondary|high|Night school|form 5', df$oth_edu, ignore.case = TRUE),
                   'Secondary', df$edu)
df$edu <- ifelse(grepl('nursing|IVE|business|finance|pre-uni|cadet', df$oth_edu, ignore.case = TRUE),
                 'Technical Institute, Commerce', df$edu)
df$edu <- ifelse(grepl('elementary', df$oth_edu, ignore.case = TRUE),
                 'Primary', df$edu)
df$edu <- ifelse(grepl('semi-illiterate', df$oth_edu, ignore.case = TRUE),
                 'Illiterate / no education', df$edu)

df$edu <- car::recode(df$edu, "
c('Missing', 35, 98) = NA;
c(12) = 'Matriculation(F.6-7)';
c(13, 'Form 4 - 5') = 'Form 4 - 6'
")

# type of housing
df$typ_hse <- ifelse(df$typ_hse %in% c(0, 29, 98), NA, df$typ_hse)


# areas & districts (arealive & areadie required, work in progress) ----
df$arealive <- ifelse(df$arealive=='Others/Outside HK' & grepl('lantau', df$live_address, ignore.case = TRUE), 'Islands',df$arealive)




# # imputation ----
# dfpart <- subset(df, select = c(# id_prefix,
#                                 year,
#                                 # icd_10,
#                                 sex, age,
#                                 # nationality, # oth_nationality,
#                                 marital,
#                                 ethnicity,
#                                 oth_ethni
#                                 # bornarea # oth_born,
#                                 # year_hk,
#                                 # bornHK
#                                 #edu, oth_edu,
#                                 # employ_status, occup, occup_group, occup_group_student,
#                                 #income, typ_hse, oth_hse
#                                 ))
# dfpart <- dfpart %>%
#   mutate(
#     # id_prefix = as.factor(id_prefix),
#     year = as.numeric(year),
#     # icd_10= as.factor(icd_10),
#     sex = as.factor(sex),
#     age = as.integer(age),
#     # nationality = as.factor(nationality),
#     marital = as.factor(marital),
#     ethnicity = as.factor(ethnicity),
#     oth_ethni = as.factor(oth_ethni)
#     # bornarea = as.factor(bornarea)
#     # year_hk = as.numeric(year_hk),
#     # bornHK = as.factor(bornHK)
#   )
# # set.seed(27076) # a 5-digit random number generated from random.org with a range (1:99999), set as seed
# set.seed(80131) # a 5-digit random number generated from random.org with a range (1:99999), set as seed
# 
# # imp <- mice::mice(dfpart, m = 1, method = c(# "",         # id_prefix
# #                                             "",         # year
# #                                             # "polyreg",  # icd_10
# #                                             "",         # sex
# #                                             "norm",     # age
# #                                             "polyreg",  # nationality
# #                                             "",         # marital
# #                                             "polyreg",  # ethnicity
# #                                             ""          # bornarea
# #                                             # "",         # year_hk
# #                                             # ""          # bornHK
# #                                             ))
# 
# imp <- mice::mice(dfpart, m = 1)
# dfimp <- mice::complete(imp)
# dfimp$ethnicity <- as.character(dfimp$ethnicity)
# dfimp$oth_ethni <- as.character(dfimp$oth_ethni)
# dfimp$oth_ethni <- ifelse(dfimp$ethnicity=='Chinese', NA, dfimp$oth_ethni)
# 
# dfpreimp <- df
# df <- cbind(df[, !(names(df) %in% names(dfimp))], dfimp)
# 
# imp10 <- mice::mice(dfpart, m = 10)
# # for (i in (1:10)) {
# #   eval_(sprintf('dfimp%s <- mice::complete(imp10, c(i))', i))
# # }
# dfimp10all <- mice::complete(imp10, action = "long")
# dfimp10all$ethnicity <- as.character(dfimp10all$ethnicity)
# dfimp10all$oth_ethni <- as.character(dfimp10all$oth_ethni)
# dfimp10all$oth_ethni <- ifelse(dfimp10all$ethnicity=='Chinese', NA, dfimp10all$oth_ethni)
# 
# dfpreimp <- df
# df <- dfimp10all

# combine ethnicity and place of birth with others ----
# df$nationality <- ifelse(df$nationality=="Non-Chinese", df$oth_nationality, df$nationality)
# df$bornarea <- ifelse(df$bornarea=="Other", df$oth_born, df$bornarea)
df$ethnicity3 <- df$ethnicity
df$ethnicity <- ifelse(df$ethnicity=="Non-Chinese", df$oth_ethni,
                       # ifelse(!is.na(df$oth_ethni), # this is the correct way but it would screw up suicide rates for "Others"
                       #        df$oth_ethni, 'Other Non-Chinese'), # "Non-Chinese" with missing oth_ethni without imputation should be regarded as missing
                       df$ethnicity)

df$ethnicity3b <- ifelse(df$ethnicity3=='Chinese', 
                         ifelse(df$ethnicity3=='Chinese' & is.na(df$bornarea), "Unknown Chinese", df$bornarea), 
                         df$ethnicity3)
df$ethnicity3b <- car::recode(df$ethnicity3b, "
c('Hong Kong') = 'Hong Kong Chinese';
c('Macau') = 'Macau Chinese';
c('Mainland China') = 'Mainland Chinese';
c('Other') = 'Overseas Chinese';
c('Unknown Chinese') = 'Chinese with unknown birthplace'
")

df$ethnicity3c <- df$ethnicity3b
df$ethnicity3c <- car::recode(df$ethnicity3c, "
c('Macau Chinese') = 'Mainland & Macau Chinese';
c('Mainland Chinese') = 'Mainland & Macau Chinese'
")

df$ethnicity11 <- car::recode(df$ethnicity, "
c('Chinese') = 'Chinese';
c('Filipino') = 'Filipino';
c('Indonesian') = 'Indonesian';
c('American', 'Australian', 'Belgian', 'Bulgarian', 'Canadian', 'Caucasian', 'British', 
'French', 'German', 'Italian', 'New Zealander', 'Norwegian', 'Portuguese', 'Russian', 'Spanish') = 'White';
c('Indian') = 'Indian';
c('Nepalese') = 'Nepalese';
c('Pakistani') = 'Pakistani';
c('Thai') = 'Thai';
c('Janpanese') = 'Japanese';
c('Bangladeshi', 'Burmese', 'East Asian', 'Korean', 'Malaysian', 'Mongolian', 'Singaporean', 
'Sri Landkan', 'South East Asian', 'Vietnamese') = 'Other Asian';
c('African', 'Colombian', 'Mexican', 'Other Non-Chinese') = 'Others'
")

df$ethnicity12 <- car::recode(df$ethnicity, "
c('Chinese') = 'Chinese';
c('Filipino') = 'Filipino';
c('Indonesian') = 'Indonesian';
c('American', 'Australian', 'Belgian', 'Bulgarian', 'Canadian', 'Caucasian', 'British', 
'French', 'German', 'Italian', 'New Zealander', 'Norwegian', 'Portuguese', 'Russian', 'Spanish') = 'White';
c('Indian') = 'Indian';
c('Nepalese') = 'Nepalese';
c('Pakistani') = 'Pakistani';
c('Thai') = 'Thai';
c('Janpanese') = 'Japanese';
c('Korean') = 'Korean';
c('Bangladeshi', 'Burmese', 'East Asian', 'Malaysian', 'Mongolian', 'Singaporean', 
'Sri Landkan', 'South East Asian', 'Vietnamese') = 'Other Asian';
c('African', 'Colombian', 'Mexican', 'Other Non-Chinese') = 'Others'
")

df$ethnicity12b <- ifelse(df$ethnicity12=='Chinese', 
                          df$ethnicity3b, 
                          df$ethnicity12)

# age groups ----
# labs_age18 <- c(paste(seq(0, 80, by = 5), seq(0 + 5 - 1, 85 - 1, by = 5),
#                 sep = "-"), paste(85, "+", sep = ""))
# df$age18 <- cut(as.integer(df$age), breaks = c(seq(0, 85, by = 5), Inf), labels = labs_age18, right = FALSE)
labs_age7 <- c("0-14", paste(seq(15, 55, by = 10), seq(25 - 1, 65 - 1, by = 10),
                             sep = "-"), paste(65, "+", sep = ""))
df$age7 <- cut(as.integer(df$age), breaks = c(0, seq(15, 65, by = 10), Inf), labels = labs_age7, right = FALSE)
labs_age11 <- c("0-14", paste(seq(15, 55, by = 5), seq(20 - 1, 60 - 1, by = 5),
                              sep = "-"), paste(60, "+", sep = ""))
df$age11 <- cut(as.integer(df$age), breaks = c(0, seq(15, 60, by = 5), Inf), labels = labs_age11, right = FALSE)

labs_age3 <- c("0-24", "25-64", "65+")
df$age3 <- cut(as.integer(df$age), breaks = c(0, 25, 65, Inf), labels = labs_age3, right = FALSE)
labs_age3b <- c("0-24", "25-54", "55+")
df$age3b <- cut(as.integer(df$age), breaks = c(0, 25, 55, Inf), labels = labs_age3b, right = FALSE)
labs_age2 <- c("0-59", "60+")
df$age2 <- cut(as.integer(df$age), breaks = c(0, 60, Inf), labels = labs_age2, right = FALSE)

# place of brith regroup ----
df$bornarea3 <- car::recode(df$bornarea, "
c('Hong Kong') = 'Hong Kong';
c('Macau') = 'China & Macau';
c('Mainland China') = 'China & Macau';
c('Other') = 'Other'
")

readr::write_csv(df, "data_02to17_extract.csv", na = "")

setwd(sprintf("~%s/cc", setpath))

saveRDS(df, file = "cc_02to17.rds")
