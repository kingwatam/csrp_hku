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
library(magrittr) # pipes

library(sqldf)
# census16 <- read.csv("P:\\Poverty_shared\\Census data\\2016 Census data\\5%sample dataset\\2016census_5%dataset.csv",
                     # header = TRUE, stringsAsFactors = FALSE)
# census16 <- openxlsx::read.xlsx("P:\\Poverty_shared\\Census data\\2016 Census data\\5%sample dataset\\2016census_5%dataset.xlsx")

setwd(sprintf("~%s/poverty/monograph/data", setpath))
# saveRDS(census16, 'census16.RDS')

census16 <- readRDS('census16.RDS')

# data cleaning ----
# sourced & adapted from Jacky's R file (C:\Users\kingtam\Documents\MEGAsync\Work\RA HKU\CSRP\poverty\monograph\archive\census16_trend analysis.R)

#exclude non-domestic household and its members
# table(census16$HHTYPE, exclude=NULL)
domeshh16 <- census16[census16$HHTYPE %in% 1,]

#separate household and individual level info
# table(domeshh16$PPN, exclude=NULL)
# sum(table(domeshh16$PPN[!(domeshh16$PPN %in% 0)], exclude=NULL))
##hh
hh16 <- domeshh16[domeshh16$PPN %in% 0,]
hh16$hhNO <- hh16$QRNO*10 + hh16$HHN
hh16 <- hh16[,c(ncol(hh16),4:29)]
##pp
pp16 <- domeshh16[!(domeshh16$PPN %in% 0),]
pp16$ppNO <- pp16$QRNO*100 + pp16$HHN*10 + pp16$PPN
pp16$hhNO <- pp16$QRNO*10 + pp16$HHN
pp16 <- pp16[, c(1:3, 30:79, ncol(pp16)-1, ncol(pp16))]
rm(domeshh16)


#re-calculate household size and household income (exclude FDH)
pp16_nofdh <- pp16[!(pp16$RELAT %in% 14),]
# pp16_nofdh <- pp16 # switch to include FDHs
rm(pp16)
pp16_nofdh$counter <- 1
pp16_nofdh$emearn1 <- pp16_nofdh$MEARN
pp16_nofdh$emearn1 <- car::recode(pp16_nofdh$emearn1, "999999=0")
pp16_nofdh$emearn2 <- pp16_nofdh$OTH_EARN
pp16_nofdh$emearn2 <- car::recode(pp16_nofdh$emearn2, "999999=0")
pp16_nofdh$ppinc <- pp16_nofdh$emearn1 + pp16_nofdh$emearn2 + pp16_nofdh$OCASH
hh16_new_size_inc <- sqldf("select hhNO, sum(counter) as hhsize, sum(ppinc) as hhinc 
                            from pp16_nofdh
                            group by hhNO")
hh16_nofdh <- merge(hh16, hh16_new_size_inc, by="hhNO", all.y=T)
rm(hh16, hh16_new_size_inc)

#calculate median household income and hence the poverty line by household size (1,2,3,4,5,6+)
hh16_nofdh$hhsize6 <- car::recode(hh16_nofdh$hhsize, "6:hi=6")
medn_hhinc <- aggregate(hh16_nofdh$hhinc[!(hh16_nofdh$hhinc %in% 999999)], 
                        list(hh16_nofdh$hhsize6[!(hh16_nofdh$hhinc %in% 999999)]), median)
names(medn_hhinc)[1] <- "hhsize6"
# medn_hhinc #show median
# median(hh16_nofdh$hhinc[!(hh16_nofdh$hhinc %in% 999999)])

#sample size
##overall households (by household size)

##poor households (by household size)
###sample-based poverty line and number of poor households
medn_hhinc$poverty_lines <- medn_hhinc$x/2
hh16 <- merge(hh16_nofdh, medn_hhinc, by="hhsize6", all.x=T)
rm(medn_hhinc)
hh16 <- hh16[order(hh16$hhNO),c(2:(ncol(hh16)-1),1,ncol(hh16))]
hh16$poorhh[hh16$hhinc <  hh16$poverty_lines] <- 1
hh16$poorhh[hh16$hhinc >= hh16$poverty_lines] <- 0

###poverty line with ref to HK 2016 poverty report and number of poor households
# hh16 <- hh16_nofdh
# hh16$govn_pvtyln[hh16$hhsize6 %in% 1] <- 4000
# hh16$govn_pvtyln[hh16$hhsize6 %in% 2] <- 9000
# hh16$govn_pvtyln[hh16$hhsize6 %in% 3] <- 15000
# hh16$govn_pvtyln[hh16$hhsize6 %in% 4] <- 18500
# hh16$govn_pvtyln[hh16$hhsize6 %in% 5] <- 19000
# hh16$govn_pvtyln[hh16$hhsize6 %in% 6] <- 20000
# hh16$poorhh[hh16$hhinc <  hh16$govn_pvtyln] <- 1
# hh16$poorhh[hh16$hhinc >= hh16$govn_pvtyln] <- 0

rm(hh16_nofdh)

##overall per capita household income
hh16$percap_hhinc[!(hh16$hhinc %in% 999999)] <- 
  hh16$hhinc[!(hh16$hhinc %in% 999999)] / hh16$hhsize[!(hh16$hhinc %in% 999999)]
##overall individuals (by household size)
hhpvty_ind <- hh16[,c("hhNO","hhsize","hhsize6","poorhh", "percap_hhinc")]
pp16 <- merge(pp16_nofdh, hhpvty_ind, by="hhNO", all.x=T)
rm(pp16_nofdh, hhpvty_ind)
##poor individuals
# table(pp16$poorhh, pp16$hhsize6, exclude=NULL)

##per capita household income of aged 18+ by age group and poverty status
pp16$agegp_18up <- car::recode(pp16$AGE, "0:17=NA; 18:29=1; 30:39=2; 40:49=3; 50:64=4; 65:hi=5")
# median(pp16$percap_hhinc, na.rm=T)
# aggregate(pp16$percap_hhinc, list(pp16$agegp_18up), median, na.rm=T)
# aggregate(pp16$percap_hhinc, list(pp16$poorhh), median, na.rm=T)
# aggregate(pp16$percap_hhinc, list(pp16$agegp_18up, pp16$poorhh), median, na.rm=T)

#identify variables of interest: groupings
##household size
##age
pp16$agegp <- car::recode(pp16$AGE, "0:14=1; 15:24=2; 25:34=3; 35:44=4; 45:54=5; 55:64=6; 65:hi=7")
##sex
##type of housing
hh16$houtype4 <- car::recode(hh16$QRTYP, "1:2=1; 3:4=2; 5:8=3; 9:12=4")
pp16 <- merge(pp16, hh16[,c("hhNO","houtype4")], by="hhNO", all.x=T)
##marital status
pp16$marr4 <- car::recode(pp16$MARIT, "2=1; 1=2; 3=3; 4:5=4")
##employment status
pp16$econ3 <- car::recode(pp16$ACTIV, "11:16=1; 21:25=2; 31:38=3")
##education (attended)
pp16$edu3 <- car::recode(pp16$EDUCN, "1:16=1; 20:38=2; 41:96=3")
##industry
pp16$indus7 <- car::recode(pp16$INDUST, "100:330=1; 410:430=2; 450:470=3; 550:560=3;
                                                   490:530=4; 580:630=4; 640:750=5; 371=6; 770:990=6")
pp16$indus7[pp16$indus7 >= 10] <- 7
pp16$indus7[pp16$INDUST %in% c(1,9)] <- 99
##occupation
pp16$occ9 <- car::recode(pp16$OCCUP, "1:10=99; 110:140=1; 210:260=2; 310:360=3; 410:450=4;
                                                510:550=5; 710:750=6; 810:830=7; 910:950=8; 610=9")
##households of selected characteristics
###identify characteristics to be captured and merge to hh

pp16$elder65 <- (pp16$AGE >= 65)*1
pp16$employ <- 2 - pp16$WHETWK
pp16$unemyouth1829 <- (pp16$econ3 %in% 2 & pp16$AGE %in% 18:29)*1
pp16$child18 <- (pp16$AGE %in% 0:17)*1
pp16$newimmig <- (pp16$BORNPL %in% 11:12 & pp16$NAT %in% 1 & pp16$DUR_HK %in% 1:7)*1
pp16_char <- sqldf("select hhNO,
                           sum(elder65) as eld65_no,
                           sum(employ) as work_no,
                           sum(unemyouth1829) as umy1829_no,
                           sum(child18) as chi18_no,
                           sum(newimmig) as newim_no
                    from pp16
                    group by hhNO")
hh16 <- merge(hh16, pp16_char, by="hhNO", all.x=T)
rm(pp16_char) 
###elderly households (all members age>=65)
hh16$elderhh <- (hh16$eld65_no == hh16$hhsize)*1
###working households (at least 1 employed member)
hh16$workhh <- (!(hh16$work_no %in% 0))*1
###households with children (at least 1 member age<18)
hh16$childhh <- (!(hh16$chi18_no %in% 0))*1
###households with unemployed youth (at least 1 unemployed member age=18-29)
hh16$umythhh <- (!(hh16$umy1829_no %in% 0))*1
###households with new immigrant (at least 1 member from mainland less than 7 years of residence)
hh16$newimhh <- (!(hh16$newim_no %in% 0))*1

###merge hh info to pp
pp16 <- merge(pp16, hh16[,c("hhNO","elderhh","workhh","childhh","umythhh","newimhh", "QRTYP","DEG_SH")],
                    by="hhNO", all.x=T)

# poverty rates & poverty numbers by various demographic groups ----
pp16$student <- ifelse(pp16$ACTIV == 32, 1, 0)
pp16$studying <- (pp16$SCH_ATT %in% 1:3)*1
pp16$NEET <- ifelse(((pp16$econ3 %in% 2 & pp16$studying == 0) | (pp16$econ3 %in% 3 & pp16$student == 0 & pp16$studying == 0))
                         & pp16$AGE %in% 15:24, 1, 
                         ifelse(pp16$AGE %in% 15:24, 0, NA)
                         )

##households with long parent (this won't give us single parents in pp16, because there are many people living with single parents)
pp16$married <- (pp16$MARIT %in% 2)*1
pp16$singlePPN <- ifelse(pp16$married %in% 0, pp16$PPN, NA)
pp16$singlePPN_male <- ifelse(!is.na(pp16$singlePPN) & pp16$SEX == 1, pp16$PPN, NA)
pp16$singlePPN_female <- ifelse(!is.na(pp16$singlePPN) & pp16$SEX == 2, pp16$PPN, NA)

# match children (incl. adults) by their PSN (Parent’s serial no.) with their single parents' PPN (singlePPN)
# SPH is single-parent household
pp16 <- 
  pp16 %>% 
  group_by(hhNO) %>%
  mutate(SPHchild = ifelse(PSN %in% singlePPN & AGE < 18, 1, 0), # whether child of single-parent households
         SPHchildPSN = ifelse(SPHchild, PSN, NA),
         SPHparent = ifelse(PPN %in% SPHchildPSN, 1, 0), # whether parent of single-parent households
         SmumHchild =  ifelse(PSN %in% singlePPN_female & AGE < 18, 1, 0),
         SdadHchild =  ifelse(PSN %in% singlePPN_male & AGE < 18, 1, 0)
         )
# # check correctness of SPHparent
# testdf <- pp16[117485:117595, 
#                c(1:4, 11, 87:91)
#                ]
pp16$SPHdad <- ifelse(pp16$SPHparent & pp16$SEX == 1, 1, 0) # single fathers
pp16$SPHmum <- ifelse(pp16$SPHparent & pp16$SEX == 2, 1, 0) # single mothers
# pp16$SPH <-  ifelse(pp16$SPHparent | pp16$SPHchild, 1, 0) # single-parent households
# pp16$SmumH <-  ifelse(pp16$SPHmum | pp16$SmumHchild, 1, 0) # single-mother households
# pp16$SdadH <-  ifelse(pp16$SPHdad | pp16$SdadHchild, 1, 0) # single-father households
# include other relatives in the single-parent households
pp16 <- 
  pp16 %>% 
  group_by(hhNO) %>%
  mutate(SPH_hhNO = ifelse(SPHparent, hhNO, NA),
         SPHdad_hhNO = ifelse(SPHparent & SEX == 1, hhNO, NA),
         SPHmum_hhNO = ifelse(SPHparent & SEX == 2, hhNO, NA),
         SPH = ifelse(hhNO %in% SPH_hhNO, 1, 0),
         SdadH = ifelse(hhNO %in% SPHdad_hhNO, 1, 0),
         SmumH = ifelse(hhNO %in% SPHmum_hhNO, 1, 0),
  )
pp16$ethnicminority <- !(pp16$ETHNIC %in% 1)*1
pp16$FDH <- (pp16$RELAT %in% 14)*1 # must to include FDH above code (line 48)

pp16 <- 
  pp16 %>% 
  group_by(hhNO) %>%
  mutate(FDHH_hhNO = ifelse(FDH, hhNO, NA),
         FDHH = ifelse(hhNO %in% FDHH_hhNO, 1, 0),
         )