rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(magrittr) # pipe 
library(tidyr)
library(car) # recode
# library(conflicted) # show conflicting functions with the same name from different packages
# detach("package:conflicted", unload=TRUE)

setwd(sprintf("~%s/poverty/case_control", setpath))

# # grouping <- xlsx::read.xlsx("Case Control Interivew_基本資料_original_king.xlsx", sheetName = "mismatch", stringsAsFactors = FALSE)
# demog <- xlsx::read.xlsx("Case Control Interivew_基本資料_original_king.xlsx", sheetName = 'updated', stringsAsFactors = FALSE)
# 
# df <- read.csv("CSRPcase_final-cl_20200326.csv", stringsAsFactors = FALSE)
# 
# # grouping$Ref.No.[which(grouping$Ref.No. == 'P166-a')] <- 'P166' # P166-a and P166 should be the same
# demog$X1..受訪者編號[which(demog$X1..受訪者編號 == 'P166-a')] <- 'P166' # P166-a and P166 should be the same
# 
# # once the two datasets are sorted by ID, matching them only requires a cbind
# df <- df %>% arrange(參加者編號)
# demog <- demog %>% arrange(X1..受訪者編號)
# # df <- cbind(df, demog, grouping[15])
# df <- cbind(df, demog[1:25])
# write_excel('poverty_case_control_merged.xlsx', merged= df)
# saveRDS(df, file = "poverty_case_control_merged.rds")

df <- readRDS('poverty_case_control_merged.rds')

# data cleaning ----
# rename variables
names(df)[names(df) == '參加者編號'] <- 'id'
names(df)[names(df) == 'X2..性別'] <- 'sex'
# names(df)[names(df) == 'X3a..出生年月'] <- 'dob'
names(df)[names(df) == 'X3b..出生年月.年齡.'] <- 'age'
names(df)[names(df) == 'X4..出生地點'] <- 'PoB'
names(df)[names(df) == 'X4..a..來港年份'] <- 'YoA' # year of arrival
names(df)[names(df) == 'X5..是否香港永久居民'] <- 'perm' # permanent resident
names(df)[names(df) == 'X6..婚姻狀況'] <- 'marital'
names(df)[names(df) == 'X7..已經完成.畢業.的最高學歷'] <- 'educ'
names(df)[names(df) == 'X8..經濟活動身份'] <- 'employ_status'
names(df)[names(df) == 'X9..現在的職業.或對上一份工作.'] <- 'last_job'
names(df)[names(df) == 'X10..你的居住地點.'] <- 'district'
names(df)[names(df) == 'X11..你家共有多少位同住家庭成員.包括自己.不包括留宿傭工..'] <- 'household_size'
names(df)[names(df) == 'X12..成員包括'] <- 'members'
names(df)[names(df) == 'X12..a..你有幾個兒子.'] <- 'sons'
names(df)[names(df) == 'X12..b..你有幾個女兒.'] <- 'daughters'
names(df)[names(df) == 'X13..你現在居住的屋宇單位類型.'] <- 'hse_type'
names(df)[names(df) == 'X14..你現在住所的居住權是'] <- 'hse_tenure'
# names(df)[names(df) == 'X15..a..津貼及補助以外.在過去十二個月.你的家庭平均..每月..總收入大概有多少..1'] <- 'income'
names(df)[names(df) == 'X16..在過去十二個月.你家庭有否接受下列津貼及補助.'] <- 'subsidy'

df[df==999] <- NA
df[df=='/'] <- NA
df[df=='／'] <- NA
df[df==' '] <- NA
df[df=='missing'] <- NA

df$perm <- ifelse(df$perm=='是', 1, 0)
df$PoB <- car::recode(df$PoB, "
c('香港') = 'Hong Kong';
c('中國内地', '澳門', 'Indonesia') = 'Outside Hong Kong'
")
df$PoB <- factor(df$PoB, levels = c('Hong Kong', 'Outside Hong Kong'))

df$YoA <- car::recode(df$YoA, "
c('missing', '安 fai') = NA;
c('1958/02') = 1958;
c('2004-2005') = 2004.5
")
df$marital <- car::recode(df$marital, "
c('喪偶') = 'Widowed';
c('已婚', '同居') = 'Married/Cohabitating';
c('從未結婚') = 'Never married';
c('分居', '離婚') = 'Separated/Divorced'
")
df$marital <- factor(df$marital, levels = c('Never married', 'Married/Cohabitating', 'Separated/Divorced', 'Widowed'))

df$educ <- car::recode(df$educ, "
c('初中（中一至中三）') = 'S1-S3';
c('大學（本科）') = 'Bachelor degree';
c('大專（非學位課程/副學士課程）') = 'Associate degree';
c('小學', '未上學') = 'Primary or below';
c('碩士') = 'Master degree';
c('職業培訓/毅進計劃') = 'Vocational training';
c('高中（中四及中五）', '預科（中六及中七）') = 'S4-S7';
")
df$educ <- factor(df$educ, levels = c('S4-S7', 'Primary or below', 'S1-S3', 'Vocational training', 
                                      'Associate degree', 'Bachelor degree', 'Master degree'))

df$employ_status <- car::recode(df$employ_status, "
'僱主/僱員/自僱主人' = 'Employed';
'失業人口' = 'Unemployed';
'非從事經濟活動身份（例如學生，退休人士，料理家務者）' = 'Others'
")
df$employ_status <- factor(df$employ_status, levels = c('Employed', 'Unemployed', 'Others'))

df$district <- car::recode(df$district, "
c('Causeway Bay', 'Causway bay', 'CWB', '大坑', '灣仔', '灣仔區', '跑馬地', '銅鑼灣') = 'Wan Chai';
c('Central', '西環') = 'Central and Western';
c('Chai Wan', 'Eastern District Shau Kei Wan', '北角', '天后', '太古', '小西灣', '東區', '柴灣', '筲箕湾', 
'筲箕灣', '西灣河', '香港島東區', '鰂魚涌') = 'Eastern';

c('Ching Ho Estate Sheung Shui', 'Fanling', 'New Territory (Fanling)',
'sheung shui', '上水', '北區', '北區，Fanling', '北區，粉嶺', '北區粉嶺', '粉嶺', '粉嶺新界北區') = 'North';

c('Ho Man Tin', '九龍啟德德朗德瓏樓3017', '九龍城', '九龍城 德朗邨', '九龍城啟晴邨樂晴樓2103室', '土瓜灣') = 'Kowloon City';
c('Jordan', '九龍大角咀', '佐敦', '大角咀', '旺角', '油麻地', '西九龍') = 'Yau Tsim Mong';
c('Kowloon Bay', 'Kwun Tong', 'Lam Tin', 'Lam Tin, Kowloon', 'Sau Mau Ping', 'SMP', '九龍灣', 
'九龍觀塘', '油塘', '牛頭角', '秀茂坪', '秀茂坪寳達村', '藍田', '藍田康柏苑祥柏閣', '觀塘', 
'觀塘區', '觀塘安達邨', '觀塘翠屏北村翠柏樓1501', '觀塘順天邨') = 'Kwun Tong';

c('Lai Chi Kok', 'Sham Shui Po', 'Shamshui Po', 'Shum Shui Po', '九龍 深水埗', '九龍深水埗', 
'深水區', '深水埗', '深水埗 大南街198號4樓', '深水捗南山邨南豐樓410室', '石硤尾', '荔枝角', '長沙灣') = 'Sham Shui Po';

c('shatin wai', '大圍', '沙田', '石古龍村18G') = 'Sha Tin';
c('TKO', '將軍澳') = 'Sai Kung';
c('Tuen Mun', '屯門') = 'Tuen Mun';
c('Tung Chung', '東涌') = 'Islands';
c('Wah Guai Village', '南區（鴨脷洲）', '香港仔') = 'Southern';
c('九龍天馬苑', '九龍慈雲山', '慈雲山', '新蒲崗', '牛池灣', '黃大仙', '黃大仙，竹園北葉園樓') = 'Wong Tai Sin';
c('元朗', '天水圍') = 'Yuen Long';
c('大埔', '太和') = 'Tai Po';
c('荃灣') = 'Tsuen Wan';
c('荔景', '葵涌', '葵青', '青衣') = 'Kwai Tsing'
")


df$region <- car::recode(df$district, "
c('HK', 'Wan Chai', 'Central and Western', 'Eastern', 'Southern') = 'Hong Kong Island';
c('Kowloon', '九龍', 'Kowloon City', 'Yau Tsim Mong', 'Kwun Tong', 'Sham Shui Po', 'Wong Tai Sin') = 'Kowloon';
c('新界', 'North', 'Sha Tin', 'Sai Kung', 'Tuen Mun', 'Islands', 'Yuen Long', 'Tai Po', 'Tsuen Wan', 'Kwai Tsing') = 'New Territories'
")
df$region <- factor(df$region, levels = c('Hong Kong Island', 'Kowloon', 'New Territories'))

df$district <- car::recode(df$district, "
c('HK', 'Kowloon', '九龍', '新界', NA) = NA
")
df$district <- factor(df$district, levels = c('Central and Western', 'Eastern', 'Southern', 'Wan Chai',
                                              'Sham Shui Po', 'Kowloon City', 'Kwun Tong', 'Wong Tai Sin', 'Yau Tsim Mong',
                                              'Kwai Tsing', 'Tsuen Wan', 'Tuen Mun', 'Yuen Long', 'Sai Kung', 'Sha Tin', 'Tai Po', 'North', 'Islands'))

# hse_type variable still requires some cleaning
df$hse_type <- car::recode(df$hse_type, "
c('公營租住房屋', '大坑西新邨') = 'Public rental housing';
c('劏房', '板間房', '光房', '寮屋') = 'Cubicles/subdivided flats/shacks';
c('新型村屋、別墅、平房、簡單石屋、傳統村屋', '村屋', '宿舍') = 'Others';
c('私人住宅單位（例：唐樓）', '私人舊大廈') = 'Private residential flats';
c('資助出售單位（不可在公開市場買賣）（例：居屋）', '資助出售單位（可在公開市場買賣）（例：可租可買的公屋）') = 'Subsidized sale flats'
")
df$hse_type <- factor(df$hse_type, levels = c('Public rental housing', 'Cubicles/subdivided flats/shacks', 'Subsidized sale flats', 'Private residential flats', 'Others'))

df$hse_tenure <- car::recode(df$hse_tenure, "
c('全租戶', '免交租金/由公司或僱主提供') = 'Sole tenant';
c('合租戶') = 'Co-tenant';
c('自置物業') = 'Owner-occupier'
")
df$hse_tenure <- factor(df$hse_tenure, levels = c('Sole tenant', 'Co-tenant', 'Owner-occupier'))

df$subsidies <- df$subsidy
df$cssa <- ifelse(grepl('綜合社會保障援助',df$subsidies, ignore.case = TRUE),  1, 0) 
df$ssa <- ifelse(grepl('公共福利金|生果金',df$subsidies, ignore.case = TRUE),  1, 0) 
df$educsub <- ifelse(grepl('教育補助|學費|業費減免',df$subsidies, ignore.case = TRUE),  1, 0)
df$ccf <- ifelse(grepl('關愛基金',df$subsidies, ignore.case = TRUE),  1, 0) 
df$wfa <- ifelse(grepl('低津|LIFA|LIF|LFI|LFS|就業補助',df$subsidies, ignore.case = TRUE),  1, 0) 
df$medfeesub <- ifelse(grepl('醫療補助',df$subsidies, ignore.case = TRUE),  1, 0) 
df$foodassist <- ifelse(grepl('短暫食物援助',df$subsidies, ignore.case = TRUE),  1, 0) 

df$subsidy <- ifelse(df$subsidy %in% '我/家庭沒有接受任何津貼或補助', 0, 1)

df$dob <- as.Date(as.numeric(df$dob), origin = "1899-12-30")
df[c('sons', 'daughters')] <-  sapply(df[c('sons', 'daughters')], as.numeric)
df$income <- df$calculated_income

df$poor <- ifelse(df$Groups=='case', 1, 0)
df$sex <- car::recode(df$sex, "
c('男') = 'Male';
c('女') = 'Female'
")
df$sex <- factor(df$sex, levels = c('Male', 'Female'))
df$sons[which(df$sons %in% c(NA))] <- 0
df$daughters[which(df$daughters %in% c(NA))] <- 0
df$children <- df$sons+df$daughters

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
labs_age2b <- c("0-64", "65+")
df$age2b <- cut(as.integer(df$age), breaks = c(0, 65, Inf), labels = labs_age2b, right = FALSE)

labs_age5 <- c("18-29", "30-39", "40-49", "50-59", "60+")
df$age_ <- cut(as.integer(df$age), breaks = c(0, 30, 40, 50, 60, Inf), labels = labs_age5, right = FALSE)

labs_age7 <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
df$age__ <- cut(as.integer(df$age), breaks = c(0, 30, 40, 50, 60, 70, 80, Inf), labels = labs_age7, right = FALSE)

# item cleaning ----
# recode NA to zeros for purely interaction terms (nested variables of P2.2.Q1)
for (nested in c(df %>% dplyr::select(starts_with("P2.2.Q2")) %>% colnames(),
                 df %>% dplyr::select(starts_with("P2.2.Q4")) %>% colnames())){
  df[, nested] <- ifelse(df[, nested] %in% c(NA), 0, df[, nested])
}
# negate P2.2.Q1 & P2.2.Q3
df$P2.2.Q1_ne <- (df$P2.2.Q1-1)*-1
df$P2.2.Q3_ne <- (df$P2.2.Q3-1)*-1


# values over 10 represent in between values (e.g. '12' means between 2 & 3)
df$P1.Q5.a <- car::recode(df$P1.Q5.a, "
12 = 2.5;
13 = 3.5;
14 = 4.5;
15 = 5.5;
16 = 6.5
")
df$P1.Q5.b <- car::recode(df$P1.Q5.b, "
12 = 2.5;
13 = 3.5;
14 = 4.5;
15 = 5.5;
16 = 6.5
")

df$P1.Q5.c <- car::recode(df$P1.Q5.c, "
c('>10') = 10;
c('<10') = 5;
c('<20') = 10;
c('100+') = 100;
c('100-200') = 150;
c('1-2') = 1.5;
c('20-30') = 25;
c('2-3') = 2.5;
c('250up') = 250;
c('3 - 4', '3-4') = 3.5;
c('30-40') = 35;
c('4-5') = 4.5;
c('5-6') = 5.5;
c('5-8') = 7.5;
c('6-7') = 6.5;
c('8-10') = 9
")
df$P1.Q5.c <- as.numeric(df$P1.Q5.c)

df$P1.Q5.c.i <- car::recode(df$P1.Q5.c.i, "
c('1-2') = 1.5;
c('2-3') = 2.5;
c('3-4') = 3.5;
c('5-6') = 5.5
")
df$P1.Q5.c.i <- as.numeric(df$P1.Q5.c.i)

df$P1.Q5.c.iii <- car::recode(df$P1.Q5.c.iii, "
12 = 2.5;
13 = 3.5;
14 = 4.5;
15 = 5.5;
16 = 6.5;
17 = 7.5;
18 = 8.5;
111 = 7;
")

df$P1.Q5.c.iiv <- car::recode(df$P1.Q5.c.iiv, "
11 = 1.5;
12 = 2.5;
13 = 3.5;
14 = 4.5;
")

df$P2.1.SA.Q1c <- car::recode(df$P2.1.SA.Q1c, "
'>1' = 1;
'>5' = 5;
'>50' = 50;
'1-2' = 1.5;
'2-3'= 2.5
")
df$P2.1.SA.Q1c <- as.numeric(df$P2.1.SA.Q1c)

df$P2.1.SB.Q1c <- car::recode(df$P2.1.SB.Q1c, "
'>5' = 5;
'2-3'= 2.5
")
df$P2.1.SB.Q1c <- as.numeric(df$P2.1.SB.Q1c)

df$P2.1.SB.Q2c <- car::recode(df$P2.1.SB.Q2c, "
'<5' = 2.5;
'10-20' = 15;
'2-4' = 3;
'3-4' = 3.5;
")
df$P2.1.SB.Q2c <- as.numeric(df$P2.1.SB.Q2c)

df$P2.1.SB.Q4c <- car::recode(df$P2.1.SB.Q4c, "
'>10' = 10;
'1-2' = 1.5;
'2-3' = 2.5
")
df$P2.1.SB.Q4c <- as.numeric(df$P2.1.SB.Q4c)

df$P2.1.SB.Q5c <- car::recode(df$P2.1.SB.Q5c, "
'<3' = 2
")
df$P2.1.SB.Q5c <- as.numeric(df$P2.1.SB.Q5c)

df$P2.1.SC.Q1.2c <- car::recode(df$P2.1.SC.Q1.2c, "
'>10' = 10;
'2-3' = 2.5;
'5-6' = 5.5
")
df$P2.1.SC.Q1.2c <- as.numeric(df$P2.1.SC.Q1.2c)

df$P2.1.SC.Q1.3c <- car::recode(df$P2.1.SC.Q1.3c, "
'>20' = 20;
'>5' = 5;
c('3-4', '3-4 wk') = 3.5;
'5-6' = 5.5
")
df$P2.1.SC.Q1.3c <- as.numeric(df$P2.1.SC.Q1.3c)

df$P2.1.SC.Q2.1c <- car::recode(df$P2.1.SC.Q2.1c, "
'<5' = 2.5
")
df$P2.1.SC.Q2.1c <- as.numeric(df$P2.1.SC.Q2.1c)

df$P2.1.SC.Q2.2c <- car::recode(df$P2.1.SC.Q2.2c, "
'1-2' = 1.5;
'6-7' = 6.5
")
df$P2.1.SC.Q2.2c <- as.numeric(df$P2.1.SC.Q2.2c)

df$P2.1.SC.Q3.c <- car::recode(df$P2.1.SC.Q3.c, "
'>1' = 1;
'>10' = 10;
'1-2' = 1.5;
'2-3' = 2.5
")
df$P2.1.SC.Q3.c <- as.numeric(df$P2.1.SC.Q3.c)

df$P2.1.SC.Q4.c <- car::recode(df$P2.1.SC.Q4.c, "
'1-2' = 1.5
")
df$P2.1.SC.Q4.c <- as.numeric(df$P2.1.SC.Q4.c)

# negate P2.1.SA.Q2.A.1, P2.1.SA.Q2.B.1, P2.1.SA.Q2.C.1
df$P2.1.SA.Q2.A.1_ne <- (df$P2.1.SA.Q2.A.1-1)*-1
df$P2.1.SA.Q2.B.1_ne <- (df$P2.1.SA.Q2.B.1-1)*-1
df$P2.1.SA.Q2.C.1_ne <- (df$P2.1.SA.Q2.C.1-1)*-1

# outcome scoring ----
df %>% dplyr::select(starts_with('P1.Q2.')) %>% colnames(.) -> qCHQ
df %>% dplyr::select(starts_with('P1.Q4.')) %>% colnames(.) -> qSWLS
df %>% dplyr::select(starts_with('P3.')) %>% colnames(.) -> qAHS

# CHQ scoring guide http://ir.lib.pccu.edu.tw/bitstream/987654321/26577/2/102PCCU0328006-001.pdf
# note that q11 and q12 are already reverse coded in the raw data
# change values (1,2,3,4) to (0,0,1,1)
df[qCHQ] <- sapply(df[qCHQ], plyr::mapvalues, from = c(1, 2, 3, 4), to = c(0, 0, 1, 1))
df$CHQ <- rowSums(df[qCHQ], na.rm = FALSE) # CHQ range(0-12) lower is better 
df[qCHQ] <- NULL
df$CHQ_3 <- ifelse(df$CHQ>3, 1, 0)

df$SWLS <- rowSums(df[qSWLS], na.rm = FALSE) # Satisfaction with Life Scale (SWLS) range(5-35)
df[qSWLS] <- NULL

qAHS_agency <- qAHS[c(2, 9, 10, 12)]
qAHS_pathway <- qAHS[c(1, 4, 6, 8)]
df$AHS <- rowSums(df[c(qAHS_agency, qAHS_pathway)], na.rm = FALSE) # Adult Hope Scale (AHS) range(8-64)
df$AHS_agency <- rowSums(df[qAHS_agency], na.rm = FALSE)
df$AHS_pathway <- rowSums(df[qAHS_pathway], na.rm = FALSE)
df[qAHS] <- NULL

# one-item Subjective Happiness & self-rated poverty
names(df)[names(df)=='P2.2.Q5']  <- 'happy'
names(df)[names(df)=='P1.Q1']  <- 'as_poor'
names(df)[names(df)=='P1.Q5.a']  <- 'social_status'
names(df)[names(df)=='P1.Q5.b']  <- 'social_status_childhood'
names(df)[names(df)=='P1.Q5.c']  <- 'n_friends'
names(df)[names(df)=='P1.Q5.c.i']  <- 'n_neighbour_friends'
df$n_neighbour_friends <- ifelse(df$n_neighbour_friends > df$n_friends, df$n_friends, df$n_neighbour_friends) # number of friends as neighbours should not exceed the total number of friends
df$prop_neightbour_friends <- df$n_neighbour_friends/df$n_friends
names(df)[names(df)=='P1.Q5.c.ii']  <- 'same_social_status'
names(df)[names(df)=='P1.Q5.c.iii']  <- 'highest_social_status'
names(df)[names(df)=='P1.Q5.c.iiv']  <- 'lowest_social_status'

df %>% dplyr::select(starts_with('P1.Q3.')) %>% colnames(.) -> qsocial_support
df$social_support <- rowSums(df[qsocial_support], na.rm = FALSE) # 3-item social support (unknown scale)

df %>% dplyr::select(starts_with('P4.')) %>% colnames(.) -> qtransport
df$transport <- rowSums(df[qtransport], na.rm = FALSE) # 10-item transport affordability (unknown scale)

# sumdf(df[which((1:ncol(df)) %in% (2:189) | names(df) %in% c('PoB', 'YoA', 'perm', 'marital',
#                                                             'educ', 'employ_status', 'last_job',
#                                                             'district', 'household_size', 'members',
#                                                             'sons', 'daughters', 'hse_type', 'hse_tenure', 'subsidy', 'income', 'children'))])

var_names <- c(names(df)[2:160],c('PoB', 'YoA', 'perm', 'marital', 
                                  'educ', 'employ_status', 'last_job', 
                                  'district', 'region', 'household_size', 'members', 
                                  'sons', 'daughters', 'hse_type', 'hse_tenure', 'subsidy', 'income',
                                  'CHQ', 'SWLS', 'AHS', 'AHS_agency', 'AHS_pathway', 'social_support', 'transport'))

# combining service need & use variables ----
# df$rec_10 <- ifelse( (df$P2.1.SA.Q1a %in% 1 & df$P2.1.SA.Q1b %in% 0) |
#                        (df$P2.1.SA.Q2a %in% 1 & df$P2.1.SA.Q2b %in% 0) |
#                        (df$P2.1.SA.Q3a %in% 1 & df$P2.1.SA.Q3b %in% 0), 1, 0)
# 
# df$rec_11 <- ifelse( (df$P2.1.SA.Q1a %in% 1 & df$P2.1.SA.Q1b %in% 1) |
#                        (df$P2.1.SA.Q2a %in% 1 & df$P2.1.SA.Q2b %in% 1) |
#                        (df$P2.1.SA.Q3a %in% 1 & df$P2.1.SA.Q3b %in% 1), 1, 0)
# 
# df$rec_1100 <- ifelse( (df$P2.1.SA.Q1a %in% 1 & df$P2.1.SA.Q1b %in% 1) |
#                          (df$P2.1.SA.Q1a %in% 0 & df$P2.1.SA.Q1b %in% 0) , 1, 0)
# 


service_var_names <- names(df)[which(names(df)=='P2.1.SA.Q1a'):which(names(df)=='P2.2.Q3')]
service_var_names <- c(service_var_names[grep('a', service_var_names)],
                       service_var_names[grep('b', service_var_names)],
                       service_var_names[grep('c', service_var_names)]
) # re-order variables, first service need, second service use, third service use frequency
service_freq_names <- service_var_names[grep('c', service_var_names)]

# # create 4-level categorical variables from service needs & use
# for (service_need in service_var_names[grep('a', service_var_names)]){
#   for (service_use in service_var_names[grep('b', service_var_names)]){
#     if (substr(service_need, 1, nchar(service_use)-1) == substr(service_use, 1, nchar(service_use)-1)){
#       df[substr(service_need, 1, nchar(service_need)-1)] <-
#         ifelse( df[[service_need]] %in% 1 & df[[service_use]] %in% 0 , 'Needed but not used',
#                 ifelse( df[[service_need]] %in% 1 & df[[service_use]] %in% 1 , 'Needed and used',
#                         ifelse( df[[service_need]] %in% 0 & df[[service_use]] %in% 0, 'Not needed and not used',
#                                 ifelse( df[[service_need]] %in% 0 & df[[service_use]] %in% 1 , 'Not needed but used', NA)
#                         )
#                 )
#         )
#     }
#   }
# }

# # create numeric variables from service needs & use to be added up for aggregation
# for (service_need in service_var_names[grep('a', service_var_names)]){
#   for (service_use in service_var_names[grep('b', service_var_names)]){
#     if (substr(service_need, 1, nchar(service_use)-1) == substr(service_use, 1, nchar(service_use)-1)){
#       df[substr(service_need, 1, nchar(service_need)-1)] <-
#         ifelse( df[[service_need]] %in% 1 & df[[service_use]] %in% 0 , 1, # Needed but not used
#                 ifelse( df[[service_need]] %in% 1 & df[[service_use]] %in% 1 , 0, # Needed and used
#                         ifelse( df[[service_need]] %in% 0 & df[[service_use]] %in% 0,  0, # Not needed and not used
#                                 ifelse( df[[service_need]] %in% 0 & df[[service_use]] %in% 1 , 0, NA) # Not needed but used
#                         )
#                 )
#         )
#     }
#   }
# }

# create binary variables from service needs & use
for (service_need in service_var_names[grep('a', service_var_names)]){
  for (service_use in service_var_names[grep('b', service_var_names)]){
    if (substr(service_need, 1, nchar(service_use)-1) == substr(service_use, 1, nchar(service_use)-1)){
      df[substr(service_need, 1, nchar(service_need)-1)] <-
        ifelse( df[[service_need]] %in% 1 & df[[service_use]] %in% 0 , 1,
                ifelse( df[[service_need]] %in% NA | df[[service_use]] %in% NA , NA, 0)
        )
    }
  }
}

rec_var_names <- names(df)[which(names(df)=='P2.1.SA.Q1'):which(names(df)=='P2.1.SA.Q3')]
soc_var_names <- names(df)[which(names(df)=='P2.1.SB.Q1'):which(names(df)=='P2.1.SB.Q8')]
med_var_names <- names(df)[which(names(df)=='P2.1.SC.Q1.1'):which(names(df)=='P2.1.SC.Q4.')]
med1_var_names <- names(df)[which(names(df)=='P2.1.SC.Q1.1'):which(names(df)=='P2.1.SC.Q1.4')]
med2_var_names <- names(df)[which(names(df)=='P2.1.SC.Q2.1'):which(names(df)=='P2.1.SC.Q2.5')]
med3_var_names <- names(df)[which(names(df)=='P2.1.SC.Q3.'):which(names(df)=='P2.1.SC.Q4.')]

df <- df %>%
  mutate(rec_services = rowSums(.[rec_var_names], na.rm = FALSE), # na.rm = FALSE means NAs are coded as NAs
         soc_services = rowSums(.[soc_var_names], na.rm = FALSE),
         med_services = rowSums(.[med_var_names], na.rm = FALSE),
         med1_services = rowSums(.[med1_var_names], na.rm = FALSE),
         med2_services = rowSums(.[med2_var_names], na.rm = FALSE),
         med3_services = rowSums(.[med3_var_names], na.rm = FALSE),
         services = rowSums(.[service_var_names], na.rm = FALSE)
  )

df$P2.1.SC.Q1.1c <- df$P2.1.SC.QP1.1c
df$P2.1.SC.QP1.1c <- NULL
service_freq_names[which(service_freq_names=='P2.1.SC.QP1.1c')] <- 'P2.1.SC.Q1.1c'
rec_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SA.Q1c'):which(service_freq_names=='P2.1.SA.Q3c')]
soc_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SB.Q1c'):which(service_freq_names=='P2.1.SB.Q8c')]
med_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SC.Q1.1c'):which(service_freq_names=='P2.1.SC.Q4.c')]
med1_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SC.Q1.1c'):which(service_freq_names=='P2.1.SC.Q1.4c')]
med2_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SC.Q2.1c'):which(service_freq_names=='P2.1.SC.Q2.5c')]
med3_var_names_freq <- service_freq_names[which(service_freq_names=='P2.1.SC.Q3.c'):which(service_freq_names=='P2.1.SC.Q4.c')]

df <- df %>%
  mutate(rec_services_freq = rowSums(.[rec_var_names_freq], na.rm = FALSE), # na.rm = FALSE means NAs are coded as NAs
         soc_services_freq = rowSums(.[soc_var_names_freq], na.rm = FALSE),
         med_services_freq = rowSums(.[med_var_names_freq], na.rm = FALSE),
         med1_services_freq = rowSums(.[med1_var_names_freq], na.rm = FALSE),
         med2_services_freq = rowSums(.[med2_var_names_freq], na.rm = FALSE),
         med3_services_freq = rowSums(.[med3_var_names_freq], na.rm = FALSE),
         services_freq = rowSums(.[service_freq_names], na.rm = FALSE)
  )

service_var_names <- c(names(df)[213:234]) # drop service need & use & frequency and only keep categorical versions of service need & use


# list of variables ----
var_names_descr <- c('sex',
                     'age',
                     'PoB',
                     'YoA',
                     'perm',
                     'educ',
                     'marital',
                     'employ_status',
                     
                     'household_size',
                     'children',
                     'sons', 
                     'daughters', 
                     'subsidy', 
                     'cssa',
                     'ssa',
                     'educsub',
                     'ccf',
                     'wfa',
                     'medfeesub',
                     'foodassist',
                     
                     'income', 
                     'poor',
                     'hse_type',
                     'hse_tenure',
                     'region',
                     'district',
                     
                     'happy', 
                     'as_poor',
                     'social_status',
                     
                     'social_status_childhood',
                     'n_friends',
                     'n_neighbour_friends',
                     'prop_neightbour_friends',
                     'same_social_status',
                     'highest_social_status',
                     'lowest_social_status',
                     
                     'CHQ', 
                     'CHQ_3', 
                     
                     'P2.2.Q1', 
                     'P2.2.Q3',
                     
                     'SWLS',
                     'AHS',
                     'AHS_agency',
                     'AHS_pathway',
                     'social_support',
                     'transport',
                     
                     # service_var_names,
                     # service_freq_names,
                     
                     
                     'services_freq',
                     'P2.1.SA.Q1c',
                     'P2.1.SA.Q2c',
                     'soc_services_freq',
                     'med_services_freq',
                     'med1_services_freq',
                     'med2_services_freq',
                     'P2.1.SC.Q3.c',
                     
                     
                     'services',
                     'P2.1.SA.Q1',
                     'P2.1.SA.Q2',
                     'soc_services',
                     'med_services',
                     'med1_services',
                     'med2_services',
                     'P2.1.SC.Q3.'
)

categorical_var <- c('sex',
                     'age_',
                     'PoB',
                     'educ',
                     'marital',
                     'employ_status',
                     
                     'hse_type',
                     'hse_tenure',
                     'region',
                     'district'
                     
                     # service_var_names
)

binary_var <- c('perm',
                'subsidy',
                
                'cssa',
                'ssa',
                'educsub',
                'ccf',
                'wfa',
                'medfeesub',
                'foodassist',
                
                'poor',
                'as_poor',
                'CHQ_3'
                # service_var_names[grep('a|b|P2.2.Q', service_var_names)]
)

ordinal_var <- c('happy', 
                 'social_status',
                 'social_status_childhood',
                 'same_social_status',
                 'highest_social_status',
                 'lowest_social_status',
                 'CHQ'
)

# descriptive statistics ----
# # get descriptive statistics and unequal variance t-test p-values
# table <- data.frame(matrix(ncol = 8,  nrow = 0))
# for (var in names(df)){
#   colnames(table)  <-  c("Variable", "N", "Mean",  "SD", "N", "Mean",  "SD",  "P-value")
#   table[which(names(df)==var), 1] <- var
#   table[which(names(df)==var), 2] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==1)]",var)))[2]
#   table[which(names(df)==var), 3] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==1)]",var)))[3]
#   table[which(names(df)==var), 4] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==1)]",var)))[5]
#   
#   table[which(names(df)==var), 5] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==0)]",var)))[2]
#   table[which(names(df)==var), 6] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==0)]",var)))[3]
#   table[which(names(df)==var), 7] <-  summ(eval_(sprintf("df$'%s'[which(df$poor==0)]",var)))[5]
#   
#   table[which(names(df)==var), 8] <- iferror(eval_(sprintf("t.test(%s ~ poor, data = df, var.equal = FALSE)$p.value", var))
#                                              , NA
#   )
# }

stats_table <- function(df, do_test = FALSE, test_by_var = NULL){ # test_by_var must be binary variables only (values = 1 & 0)
  table <- data.frame(matrix(ncol = 11,  nrow = 0))
  row_count <- 1
  colnames(table)  <-  c("Variable", "Category", "N", "Mean/Proportion",  "SD", "N", "Mean/Proportion",  "SD",  "P-value", "***", "Range")
  for (var in var_names_descr){
    if (!(var %in% categorical_var)){
      table[row_count, 1] <- var
      
      try({
        table[row_count, 3] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==1)]", var, test_by_var)))[2]
        table[row_count, 4] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==1)]", var, test_by_var)))[3]
        table[row_count, 5] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==1)]", var, test_by_var)))[5]
        
        table[row_count, 6] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==0)]", var, test_by_var)))[2]
        table[row_count, 7] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==0)]", var, test_by_var)))[3]
        table[row_count, 8] <-  summ(eval_(sprintf("df$'%s'[which(df$%s==0)]", var, test_by_var)))[5]
      })
      
      min_value <- summ(eval_(sprintf("df$'%s'", var)))[[6]]
      max_value <- summ(eval_(sprintf("df$'%s'", var)))[[7]]
      table[row_count, 11] <-  paste0(min_value, '-', max_value)
      
      if (do_test){
        if (var %in% binary_var){
          table[row_count, 9] <- iferror(eval_(sprintf("fisher.test(df$'%s', df$%s, simulate.p.value = TRUE, B= 10000)$p.value", var, test_by_var))
                                         , NA)
        } else if (var %in% ordinal_var){
          table[row_count, 9] <- iferror(eval_(sprintf("wilcox.test(%s ~ %s, data = df, correct=FALSE)$p.value", var, test_by_var))
                                         , NA)
        } else {
          table[row_count, 9] <- iferror(eval_(sprintf("t.test(%s ~ %s, data = df, var.equal = FALSE)$p.value", var, test_by_var))
                                         , NA)
        }
        table[row_count, 10] <- starred_p(table[row_count, 9], 3)
      }
      row_count <- row_count + 1
      
    } else {
      levels_values <- unique(eval_(sprintf("df$'%s'",var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
      num_levels <- length(levels_values)
      table[row_count, 1] <- var
      
      if (do_test){
        table[row_count, 9] <- iferror(eval_(sprintf("fisher.test(df$'%s', df$%s, simulate.p.value = TRUE, B= 10000)$p.value", var, test_by_var))
                                       , NA)
      }
      
      row_count_0 <- row_count # keep row count the same for starred_p()
      for (level in levels_values){
        if (is.na(level)){
          next()
        }
        table[row_count, 2] <- level
        try({
          n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$%s == 1)]", var, var, test_by_var))) # double %% to escape the usual %
          table[row_count, 3] <- n_level
          table[row_count, 4] <- n_level/length(eval_(sprintf("df$'%s'[which(df$%s == 1 & !is.na(df$'%s'))]", var, test_by_var, var)))
          
          n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$%s == 0)]", var, var, test_by_var)))
          table[row_count, 6] <- n_level
          table[row_count, 7] <- n_level/length(eval_(sprintf("df$'%s'[which(df$%s == 0 & !is.na(df$'%s'))]", var, test_by_var, var)))
          table[row_count, 10] <- iferror(starred_p(table[row_count, 9], 3), NA)
        })
        row_count <- row_count + 1
      }
    }
  }
  
  table[is.na(table)] <- ""
  table <- format(table, scientific = FALSE)
  return(table)
}

df$all <- 1
table <- stats_table(df, do_test = FALSE, test_by_var = 'all')

# estimate odds ratio from logistic (logit-binomial) regression ----
for (var in var_names){
  try({
    fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "logit"))', var))
    if ((is.numeric(eval_(sprintf("df$'%s'",var))))){ 
      beta <- exp(summary(fit)$coef[var, 1])
      lowerCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.025) * summary(fit)$coef[var, 2])
      upperCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.975) * summary(fit)$coef[var, 2])
      p_value <- summary(fit)$coef[var, 4]
      if (p_value < 0.05){
        print(paste(var, round_format(beta, 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(p_value, 5), sep=', '))
      }
    } else { # if factor (non-numeric), then ...
      levels_values <- unique(eval_(sprintf("df$'%s'",var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that first level is baseline/reference group
      num_levels <- length(levels_values)
      count <- 1
      for (i in levels_values[2:num_levels]){ # get beta & p_value for each level of the factor
        beta <- exp(summary(fit)$coef[paste0(var, i), 1])
        lowerCI <- exp(summary(fit)$coef[paste0(var, i), 1] + qnorm(0.025) * summary(fit)$coef[paste0(var, i), 2])
        upperCI <- exp(summary(fit)$coef[paste0(var, i), 1] + qnorm(0.975) * summary(fit)$coef[paste0(var, i), 2])
        p_value <- summary(fit)$coef[paste0(var, i), 4]
        if (p_value < 0.05){
          print(paste(paste0(var, i), round_format(beta, 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(p_value, 9), sep=', '))
        }        
        count <- count+1
      }
    }
  }, silent = TRUE)
}

# fit <- glm(poor~sex+age+household_size+children,data=df,family=binomial(link = "log"),  start = c(log(mean(df$poor)), rep(0, 5-1))) # log-binomial requires starting values to converge, 5 is the number of explanatory variables including the intercept 
# for (var in c('sexmale', 'age', 'household_size', 'children')){
#   print(paste(var,  round_format(exp(summary(fit)$coef[var, 1]), 2),round_format(summary(fit)$coef[var, 4], 3), sep=', '))
# }

fit <- glm(poor~sex+age__+household_size+children+P2.2.Q2.8.1,data=df,family=binomial(link = "logit"))
for (var in c('sexmale', 'age__30-39', 'age__40-49', 'age__50-59', 'age__60+', 'household_size', 'children')){
  print(paste(var,  round_format(exp(summary(fit)$coef[var, 1]), 2),round_format(summary(fit)$coef[var, 4], 3), sep=', '))
}

fit <- glm(poor~sex+age_+household_size+children,data=df,family=binomial(link = "logit"))
for (var in c('sexmale', 'age_30-39', 'age_40-49', 'age_50-59', 'age_60-69', 'age_70-79', 'age_80+', 'household_size', 'children')){
  lowerCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.025) * summary(fit)$coef[var, 2])
  upperCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.975) * summary(fit)$coef[var, 2])
  print(paste(var,  round_format(exp(summary(fit)$coef[var, 1]), 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(summary(fit)$coef[var, 4], 9), sep=', '))
}

fit <- glm(poor~sex+age_+household_size+children+PoB,data=df,family=binomial(link = "log"),  start = c(log(mean(df$poor)), rep(0, 11+(length(unique(df$PoB))-2-1))))  # log-binomial requires starting values to converge, 10 is the number of explanatory variables including the intercept and dummies
for (var in c('sexmale', 'age_30-39', 'age_40-49', 'age_50-59', 'age_60-69', 'age_70-79', 'age_80+', 'household_size', 'children')){
  lowerCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.025) * summary(fit)$coef[var, 2])
  upperCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.975) * summary(fit)$coef[var, 2])
  print(paste(var,  round_format(exp(summary(fit)$coef[var, 1]), 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(summary(fit)$coef[var, 4], 9), sep=', '))
}


# estimate relative risk ratio from log-binomial regression ----
for (var in var_names_descr){
  try({
    if ((is.numeric(eval_(sprintf("df$'%s'",var))))){ 
      fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "log"),  start = c(log(mean(df$poor)), rep(0, 11-1)))', var))
      beta <- exp(summary(fit)$coef[var, 1])
      lowerCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.025) * summary(fit)$coef[var, 2])
      upperCI <- exp(summary(fit)$coef[var, 1] + qnorm(0.975) * summary(fit)$coef[var, 2])
      p_value <- summary(fit)$coef[var, 4]
      if (p_value < 0.05){
        print(paste(var, round_format(beta, 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(p_value, 5), sep=', '))
      }
    } else { # if factor (non-numeric), then ...
      levels_values <- unique(eval_(sprintf("df$'%s'",var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
      num_levels <- length(levels_values)
      count <- 1
      iferror(
        fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "log"),  start = c(log(mean(df$poor)), rep(0, %s-1)))', var, (11+(num_levels-2))))
        ,
        fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "log"),  start = c(log(mean(df$poor)), rep(0, %s-1)))', var, (11+(num_levels-3))))
      )
      for (i in levels_values[2:num_levels]){ # get beta & p_value for each level of the factor
        beta <- exp(summary(fit)$coef[paste0(var, i), 1])
        lowerCI <- exp(summary(fit)$coef[paste0(var, i), 1] + qnorm(0.025) * summary(fit)$coef[paste0(var, i), 2])
        upperCI <- exp(summary(fit)$coef[paste0(var, i), 1] + qnorm(0.975) * summary(fit)$coef[paste0(var, i), 2])
        p_value <- summary(fit)$coef[paste0(var, i), 4]
        if (p_value < 0.05){
          print(paste(paste0(var, i), round_format(beta, 5), round_format(lowerCI, 5), round_format(upperCI, 5), round_format(p_value, 9), sep=', '))
        }
        count <- count+1
      }
    }
  }, silent = TRUE)
}

# estimate relative risk ratio from modified Poisson regression ----
df$id2 <- 1:nrow(df)
for (var in var_names){
  try({
    fit <- eval_(sprintf("
    geepack::geeglm(formula = poor~sex+age_+household_size+children+%s,
    data    = na.omit(df[, c('id2', 'poor', 'sex', 'age_', 'household_size', 'children', '%s')]),
    family  = poisson(link = 'log'),
    id      = id2, 
    corstr  = 'exchangeable')
                         ", var, var))
    
    # parameters::model_parameters(glm(formula,family="poisson",data=df), robust = TRUE, vcov_type = 'HC0') would be essentially identical results as using geeglm to get robust SEs
    # note that the default HC3 vcov_type is slightly different, HC0 produces the same results as geeglm with id (per observation)
    
    beta <- exp(summary(fit)$coef[var, 1])
    p_value <- summary(fit)$coef[var, 4]
  }, silent = TRUE)
  if (p_value < 0.05){
    print(paste(var, round_format(beta, 2), round_format(p_value, 3), sep=', '))
  }
}
df$id2 <- NULL

df$id2 <- 1:nrow(df)
fit <- geepack::geeglm(formula = poor~sex+age_+household_size+children+P2.1.SC.Q1.3c,
                       data    = na.omit(df[, c('id2', 'poor', 'sex', 'age_', 'household_size', 'children', 'P2.1.SC.Q1.3c')]),
                       family  = poisson(link = "log"),
                       id      = id2, # individual observations treated as a repeated measure
                       corstr  = "exchangeable")
for (var in c('sexmale', 'age_30-39', 'age_40-49', 'age_50-59', 
              'age_60-69', 'age_70-79', 'age_80+', 'household_size', 'children', 'P2.1.SC.Q1.3c')){
  print(paste(var,  round_format(exp(summary(fit)$coef[var, 1]), 2),round_format(summary(fit)$coef[var, 4], 3), sep=', '))
}
df$id2 <- NULL

fit <- glm(poor~sex+age_+household_size+children+P2.2.Q1_ne+
             P2.2.Q1_ne:P2.2.Q2.2+
             P2.2.Q1_ne:P2.2.Q2.3+
             P2.2.Q1_ne:P2.2.Q2.4+
             P2.2.Q1_ne:P2.2.Q2.5+
             P2.2.Q1_ne:P2.2.Q2.6+
             P2.2.Q1_ne:P2.2.Q2.7+
             P2.2.Q1_ne:P2.2.Q2.8
           ,data=df,family=binomial(link = "logit"))
summary(fit)

fit <- glm(poor~sex+age_+household_size+children+P2.2.Q1_ne+
             P2.2.Q1_ne:P2.2.Q2.6
           ,data=df,family=binomial(link = "logit"))
summary(fit)


# log-binomial regression results in table form ----
var_names_reg <- var_names_descr
var_names_reg[which(var_names_reg %in% 'age')]  <- 'age_'
if (max(df$income) >= 200){ # recale income by 1000 if it hasn't been scaled already
  df$income <- df$income/1000
} 

table <- data.frame(matrix(ncol = 7,  nrow = 0))
row_count <- 1
colnames(table)  <-  c("Variable", "Category", "RR", "Lower CI", "Upper CI",  "p-value", "significance")
for (var in var_names_reg){
  if (!(var %in% categorical_var)){
    table[row_count, 1] <- var
    
    iferror(
      fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "log"),  
                         start = c(log(mean(df$poor,  na.rm=TRUE)), rep(0, nrow(summary(glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "logit")))$coef)-1)))', var, var))
      , {row_count <- row_count + 1 ; next()})
    beta <- iferror(summary(fit)$coef[var, 1], NA)
    rr <- iferror(exp(beta), NA)
    se <-  iferror(summary(fit)$coef[var, 2], NA)
    lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
    upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
    p_value <- iferror(summary(fit)$coef[var, 4], NA)
    
    table[row_count, 3] <-  round_format(rr, 5)
    table[row_count, 4] <-  round_format(lowerCI, 5)
    table[row_count, 5] <-  round_format(upperCI, 5)
    table[row_count, 6] <-  round_format(p_value, 5)
    table[row_count, 7] <-  starred_p(p_value, 3)
    
    row_count <- row_count + 1
  } else {
    levels_values <- unique(eval_(sprintf("df$'%s'",var)))
    levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
    num_levels <- length(levels_values)
    table[row_count, 1] <- var
    
    iferror(
      fit <- eval_(sprintf('glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "log"),  
                         start = c(log(mean(df$poor,  na.rm=TRUE)), rep(0, nrow(summary(glm(poor~sex+age_+household_size+children+%s,data=df,family=binomial(link = "logit")))$coef)-1)))', var, var))
      , {row_count <- row_count + 1 ; next()})
    for (level in levels_values){
      if (is.na(level)){
        next()
      }
      
      table[row_count, 2] <- level
      
      if (level %in% levels_values[1]){
        row_count <- row_count + 1
        next()
      }
      
      beta <- iferror(summary(fit)$coef[paste0(var, level), 1], NA)
      rr <- iferror(exp(beta), NA)
      se <-  iferror(summary(fit)$coef[paste0(var, level), 2], NA)
      lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
      upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
      p_value <- iferror(summary(fit)$coef[paste0(var, level), 4], NA)
      
      table[row_count, 3] <-  round_format(rr, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  round_format(p_value, 5)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
      
      # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==1)]", var, var))) # double %% to escape the usual %
      # table[row_count, 3] <- n_level
      # table[row_count, 4] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==1 & !is.na(df$'%s'))]", var, var)))
      # 
      # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==0)]", var, var)))
      # table[row_count, 6] <- n_level
      # table[row_count, 7] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==0 & !is.na(df$'%s'))]", var, var)))
      # table[row_count, 10] <- starred_p(table[row_count_0, 9], 3)
      # row_count <- row_count + 1
    }
  }
}

table[is.na(table)] <- ""
table <- format(table, scientific = FALSE)

# different Ys in OLS regression ----
dep_vars <- c('rec_services',
              'soc_services',
              'med_services',
              'med1_services',
              'med2_services',
              'med3_services',
              'services',
              'rec_services_freq',
              'soc_services_freq',
              'med_services_freq',
              'med1_services_freq',
              'med2_services_freq',
              'med3_services_freq',
              'services_freq')
df$overseas_born <- ifelse(!(df$PoB %in% 'Hong Kong'), 1, 0)
df$YoA[is.na(df$YoA) & df$overseas_born == 0] <- 0
for (dep_var in dep_vars){
  
  var_names_reg <- var_names_descr
  var_names_reg <-var_names_reg[-(which(var_names_reg %in% c(dep_var)))]
  var_names_reg[which(var_names_reg %in% 'age')]  <- 'age_'
  if (max(df$income) >= 200){ # recale income by 1000 if it hasn't been scaled already
    df$income <- df$income/1000
  } 
  
  table <- data.frame(matrix(ncol = 8,  nrow = 0))
  row_count <- 1
  colnames(table)  <-  c("Variable", "Category", "Beta", "SE", "Lower CI", "Upper CI",  "p-value", "significance")
  for (var in var_names_reg){
    if (var == 'YoA'){
      iferror(
        fit <- eval_(sprintf('lm(%s~sex+age_+household_size+children+poor+as_poor+overseas_born+overseas_born:YoA,data=df)', dep_var, var))
        , {row_count <- row_count + 1 ; next()})
      var <- 'overseas_born:YoA'
    } else{
      iferror(
        fit <- eval_(sprintf('lm(%s~sex+age_+household_size+children+poor+as_poor+%s,data=df)', dep_var, var))
        , {row_count <- row_count + 1 ; next()})        
    }
    table[row_count, 1] <- var
    
    if (!(var %in% categorical_var)){
      
      beta <- iferror(summary(fit)$coef[var, 1], NA)
      se <-  iferror(summary(fit)$coef[var, 2], NA)
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
      p_value <- iferror(summary(fit)$coef[var, 4], NA)
      
      table[row_count, 3] <-  round_format(beta, 5)
      table[row_count, 4] <-  round_format(se, 5)
      table[row_count, 5] <-  round_format(lowerCI, 5)
      table[row_count, 6] <-  round_format(upperCI, 5)
      table[row_count, 7] <-  round_format(p_value, 5)
      table[row_count, 8] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    } else {
      levels_values <- unique(eval_(sprintf("df$'%s'", var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
      num_levels <- length(levels_values)
      
      for (level in levels_values){
        if (is.na(level)){
          next()
        }
        
        table[row_count, 2] <- level
        
        if (level %in% levels_values[1]){
          row_count <- row_count + 1
          next()
        }
        
        beta <- iferror(summary(fit)$coef[paste0(var, level), 1], NA)
        se <- iferror(summary(fit)$coef[paste0(var, level), 2], NA)
        lowerCI <- iferror(beta + qnorm(0.025) * se, NA)
        upperCI <- iferror(beta + qnorm(0.975) * se, NA)
        p_value <- iferror(summary(fit)$coef[paste0(var, level), 4], NA)
        
        table[row_count, 3] <-  round_format(beta, 5)
        table[row_count, 4] <-  round_format(se, 5)
        table[row_count, 5] <-  round_format(lowerCI, 5)
        table[row_count, 6] <-  round_format(upperCI, 5)
        table[row_count, 7] <-  round_format(p_value, 5)
        table[row_count, 8] <-  starred_p(p_value, 3)
        
        row_count <- row_count + 1
        
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==1)]", var, var))) # double %% to escape the usual %
        # table[row_count, 3] <- n_level
        # table[row_count, 4] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==1 & !is.na(df$'%s'))]", var, var)))
        # 
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==0)]", var, var)))
        # table[row_count, 6] <- n_level
        # table[row_count, 7] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==0 & !is.na(df$'%s'))]", var, var)))
        # table[row_count, 10] <- starred_p(table[row_count_0, 9], 3)
        # row_count <- row_count + 1
      }
    }
  }
  table <- format(table, scientific = FALSE)
  table[is.na(table)] <- ""
  table[3:7] <- mutate_all(table[3:7], function(x) as.numeric(as.character(x)))
  table[table=='NA'] <- ""
  assign(dep_var, table)
  rm(table)
}

# service item-level regression ----
dep_vars <- c('rec_services',
              'soc_services',
              'med_services',
              'med1_services',
              'med2_services',
              'med3_services',
              'services')
table <- data.frame(matrix(ncol = 2,  nrow = 0))
row_count <- 1

for (dep_var in dep_vars){
  colnames(table)  <-  c("Variable", "R squared")
  table[row_count, 1] <- dep_var
  
  vars <- switch(which(dep_vars==dep_var), rec_var_names, soc_var_names, med_var_names, med1_var_names, med2_var_names, med3_var_names, service_var_names)
  fit <-  eval_(sprintf('lm(%s~-1 + %s, data = df)', dep_var, capture.output(cat(vars, sep = '+'))
  ))
  table[row_count, 2] <- summary(fit)$r.sq
  
  row_count <- row_count + 1
  for (var in vars){
    table[row_count, 1] <- var
    fit <-  eval_(sprintf('lm(%s~-1 + %s, data = df)', dep_var, var)
    )
    table[row_count, 2] <- summary(fit)$r.sq
    
    row_count <- row_count + 1
  }
}


# service results using Poisson regression with non-robust SEs ----
dep_vars <- c('P2.1.SA.Q1',
              'P2.1.SA.Q2',
              'soc_services',
              'med_services',
              'med1_services',
              'med2_services',
              'P2.1.SC.Q3.',
              'services',
              'P2.1.SA.Q1c',
              'P2.1.SA.Q2c',
              'soc_services_freq',
              'med_services_freq',
              'med1_services_freq',
              'med2_services_freq',
              'P2.1.SC.Q3.c',
              'services_freq')

df$overseas_born <- ifelse(!(df$PoB %in% 'Hong Kong'), 1, 0)
df$YoA[is.na(df$YoA) & df$overseas_born == 0] <- 0
for (dep_var in dep_vars){
  print(dep_var)
  var_names_reg <- var_names_descr
  if (dep_var %in% var_names_reg){
    var_names_reg <-var_names_reg[-(which(var_names_reg %in% c(dep_var)))]
  }
  var_names_reg[which(var_names_reg %in% 'age')]  <- 'age_'
  if (max(df$income) >= 200){ # recale income by 1000 if it hasn't been scaled already
    df$income <- df$income/1000
  } 
  
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  colnames(table)  <-  c("Variable", "Category", "RR", "Lower CI", "Upper CI",  "p-value", "significance")
  for (var in var_names_reg){
    if (var == 'YoA'){
      iferror(
        fit <- eval_(sprintf('glm(%s~sex+age_+household_size+children+poor+as_poor+overseas_born+overseas_born:YoA,data=df, family=poisson(log))', dep_var, var))
        , {row_count <- row_count + 1 ; next()})
      var <- 'overseas_born:YoA'
    } else{
      iferror(
        fit <- eval_(sprintf('glm(%s~sex+age_+household_size+children+poor+as_poor+%s,data=df, family=poisson(log))', dep_var, var))
        , {row_count <- row_count + 1 ; next()})        
    }
    table[row_count, 1] <- var
    
    if (!(var %in% categorical_var)){
      
      
      beta <- iferror(summary(fit)$coef[var, 1], NA)
      rr <- iferror(exp(beta), NA)
      se <-  iferror(summary(fit)$coef[var, 2], NA)
      
      
      lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
      upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
      p_value <- iferror(summary(fit)$coef[var, 4], NA)
      table[row_count, 3] <-  round_format(rr, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  round_format(p_value, 5)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    } else {
      levels_values <- unique(eval_(sprintf("df$'%s'", var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
      num_levels <- length(levels_values)
      
      for (level in levels_values){
        if (is.na(level)){
          next()
        }
        
        table[row_count, 2] <- level
        
        if (level %in% levels_values[1]){
          row_count <- row_count + 1
          next()
        }
        
        beta <- iferror(summary(fit)$coef[paste0(var, level), 1], NA)
        rr <- iferror(exp(beta), NA)
        se <-  iferror(summary(fit)$coef[paste0(var, level), 2], NA)
        
        lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
        upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
        p_value <- iferror(summary(fit)$coef[paste0(var, level), 4], NA)
        
        table[row_count, 3] <-  round_format(rr, 5)
        table[row_count, 4] <-  round_format(lowerCI, 5)
        table[row_count, 5] <-  round_format(upperCI, 5)
        table[row_count, 6] <-  round_format(p_value, 5)
        table[row_count, 7] <-  starred_p(p_value, 3)
        
        row_count <- row_count + 1
        
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==1)]", var, var))) # double %% to escape the usual %
        # table[row_count, 3] <- n_level
        # table[row_count, 4] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==1 & !is.na(df$'%s'))]", var, var)))
        # 
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==0)]", var, var)))
        # table[row_count, 6] <- n_level
        # table[row_count, 7] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==0 & !is.na(df$'%s'))]", var, var)))
        # table[row_count, 10] <- starred_p(table[row_count_0, 9], 3)
        # row_count <- row_count + 1
      }
    }
  }
  table <- format(table, scientific = FALSE)
  table[is.na(table)] <- ""
  table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  table[table=='NA'] <- ""
  assign(dep_var, table)
  rm(table)
}

write_excel("poisson_results.xlsx", 
            P2.1.SA.Q1, P2.1.SA.Q1c, P2.1.SA.Q2, P2.1.SA.Q2c, 
            soc_services, soc_services_freq,
            med_services, med_services_freq, 
            med1_services, med1_services_freq,
            med2_services, med2_services_freq,
            P2.1.SC.Q3., P2.1.SC.Q3.c,
            services, services_freq
)


# service results using Poisson regression with robust SEs ----
dep_vars <- c('P2.1.SA.Q1',
              'P2.1.SA.Q2',
              'soc_services',
              'med_services',
              'med1_services',
              'med2_services',
              'P2.1.SC.Q3.',
              'services',
              'P2.1.SA.Q1c',
              'P2.1.SA.Q2c',
              'soc_services_freq',
              'med_services_freq',
              'med1_services_freq',
              'med2_services_freq',
              'P2.1.SC.Q3.c',
              'services_freq')

df$overseas_born <- ifelse(!(df$PoB %in% 'Hong Kong'), 1, 0)
df$YoA[is.na(df$YoA) & df$overseas_born == 0] <- 0
for (dep_var in dep_vars){
  print(dep_var)
  var_names_reg <- var_names_descr
  if (dep_var %in% var_names_reg){
    var_names_reg <-var_names_reg[-(which(var_names_reg %in% c(dep_var)))]
  }
  var_names_reg[which(var_names_reg %in% 'age')]  <- 'age_'
  if (max(df$income) >= 200){ # recale income by 1000 if it hasn't been scaled already
    df$income <- df$income/1000
  } 
  
  table <- data.frame(matrix(ncol = 7,  nrow = 0))
  row_count <- 1
  colnames(table)  <-  c("Variable", "Category", "RR", "Lower CI", "Upper CI",  "p-value", "significance")
  for (var in var_names_reg){
    if (var == 'YoA'){
      iferror(
        fit <- eval_(sprintf('glm(%s~sex+age_+household_size+children+poor+as_poor+overseas_born+overseas_born:YoA,data=df, family=poisson(log))', dep_var, var))
        , {row_count <- row_count + 1 ; next()})
      var <- 'overseas_born:YoA'
    } else{
      iferror(
        fit <- eval_(sprintf('glm(%s~sex+age_+household_size+children+poor+as_poor+%s,data=df, family=poisson(log))', dep_var, var))
        , {row_count <- row_count + 1 ; next()})        
    }
    table[row_count, 1] <- var
    
    if (!(var %in% categorical_var)){
      
      
      beta <- iferror(summary(fit)$coef[var, 1], NA)
      rr <- iferror(exp(beta), NA)
      se <-  iferror(if_na(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC3"))[var,2],
                           lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC1"))[var,2]), NA) # for some reason, default HC3 doesn't compute for med_services_freq ~ hse_type & district
      
      
      lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
      upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
      p_value <- iferror(if_na(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC3"))[var,4],
                               lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC1"))[var,4]), NA)
      table[row_count, 3] <-  round_format(rr, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  round_format(p_value, 5)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    } else {
      levels_values <- unique(eval_(sprintf("df$'%s'", var)))
      levels_values <- levels_values[order(levels_values)] # order the levels so that baseline/reference group is the first level
      num_levels <- length(levels_values)
      
      for (level in levels_values){
        if (is.na(level)){
          next()
        }
        
        table[row_count, 2] <- level
        
        if (level %in% levels_values[1]){
          row_count <- row_count + 1
          next()
        }
        
        beta <- iferror(summary(fit)$coef[paste0(var, level), 1], NA)
        rr <- iferror(exp(beta), NA)
        se <-  iferror(if_na(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC3"))[paste0(var, level),2],
                             lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC1"))[paste0(var, level),2]), NA)
        
        lowerCI <-  iferror(exp(beta + qnorm(0.025) * se), NA)
        upperCI <- iferror(exp(beta + qnorm(0.975) * se), NA)
        p_value <- iferror(if_na(lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC3"))[paste0(var, level),4],
                                 lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit, type="HC1"))[paste0(var, level),4]), NA)
        
        table[row_count, 3] <-  round_format(rr, 5)
        table[row_count, 4] <-  round_format(lowerCI, 5)
        table[row_count, 5] <-  round_format(upperCI, 5)
        table[row_count, 6] <-  round_format(p_value, 5)
        table[row_count, 7] <-  starred_p(p_value, 3)
        
        row_count <- row_count + 1
        
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==1)]", var, var))) # double %% to escape the usual %
        # table[row_count, 3] <- n_level
        # table[row_count, 4] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==1 & !is.na(df$'%s'))]", var, var)))
        # 
        # n_level <- length(eval_(sprintf("df$'%s'[which(df$'%s' %%in%% level & df$poor==0)]", var, var)))
        # table[row_count, 6] <- n_level
        # table[row_count, 7] <- n_level/length(eval_(sprintf("df$'%s'[which(df$poor==0 & !is.na(df$'%s'))]", var, var)))
        # table[row_count, 10] <- starred_p(table[row_count_0, 9], 3)
        # row_count <- row_count + 1
      }
    }
  }
  table <- format(table, scientific = FALSE)
  table[is.na(table)] <- ""
  table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  table[table=='NA'] <- ""
  assign(dep_var, table)
  rm(table)
}

write_excel("robust_poisson_results.xlsx", 
            P2.1.SA.Q1, P2.1.SA.Q1c, P2.1.SA.Q2, P2.1.SA.Q2c, 
            soc_services, soc_services_freq,
            med_services, med_services_freq, 
            med1_services, med1_services_freq,
            med2_services, med2_services_freq,
            P2.1.SC.Q3., P2.1.SC.Q3.c,
            services, services_freq
)
