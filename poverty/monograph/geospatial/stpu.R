rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(magrittr) # pipes

setwd(sprintf("~%s/poverty/monograph/geospatial/data", setpath))

# df <- readxl::read_xls("kndeath2016.xls")
# saveRDS(df, file = "kndeath2016.rds")

df <- readRDS("kndeath2016.rds")
names(df)[names(df)=='Place of residence'] <- "TPU"
TPU_to_STPU <- function(var){
  car::recode(var, "
  c('XXX', 84, 91) = NA;
  123:124 = 121;
  147 = 146;
  158 = 156;
  165 = 164;
  176 = 175;
  182 = 181;
  184 = 183;
  c(192, 194) = 190;
  c(195, 198) = 193;
  215:216 = 213;
  256 = 251;
  269 = 255;
  289 = 288;
  296 = 293;
  321 = 310;
  c(324, 329) = 320;
  331:334 = 331; c(336, 340) = 331;
  411:416 = 411; 427 = 411;
  422 = 421;
  428 = 423;
  431:434 = 431;
  546 = 543;
  c(621, 632) = 610;
  c(622, 641) = 620;
  633 = 633;
  651:653 = 651;
  c(712, 721, 728) = 711;
  727 = 722;
  c(733, 754) = 731;
  c(751, 753) = 732;
  741:744 = 741;
  761:762 = 756;
  811:815 = 811;
  829 = 824;
  828 = 826;
  834 = 832;
  911:913 = 911;
  933 = 931;
  934 = 932;
  941:943 = 941;
  951 = 950;
  961:963 = 961;
  971:974 = 971
  ")
} # Small TPU (STPU) is TPU where some TPUs are merged with adjacent TPUs
df$STPU <- TPU_to_STPU(df$TPU)
# poverty rates ----
setwd(sprintf("~%s/poverty/monograph/geospatial", setpath))
poverty1996 <- openxlsx::read.xlsx('Census_Proportion_Poverty_STPU_1996.2001.2006.2011.xlsx', sheet = '1996')
poverty2001 <- openxlsx::read.xlsx('Census_Proportion_Poverty_STPU_1996.2001.2006.2011.xlsx', sheet = '2001')
poverty2006 <- openxlsx::read.xlsx('Census_Proportion_Poverty_STPU_1996.2001.2006.2011.xlsx', sheet = '2006')
poverty2011 <- openxlsx::read.xlsx('Census_Proportion_Poverty_STPU_1996.2001.2006.2011.xlsx', sheet = '2011')

poverty1996$STPU <- TPU_to_STPU(poverty1996$STPU)
poverty2001$STPU <- TPU_to_STPU(poverty2001$STPU)
poverty2006$STPU <- TPU_to_STPU(poverty2006$STPU)
poverty2011$STPU <- TPU_to_STPU(poverty2011$STPU)

poverty <- data.frame(STPU = unique(c(poverty1996$STPU, poverty2001$STPU, poverty2006$STPU, poverty2011$STPU)))
poverty$STPU <- poverty[order(poverty$STPU),]
poverty1996 <- as.data.frame(aggregate(cbind(all, poor) ~ STPU, poverty1996 , sum))
poverty2001 <- as.data.frame(aggregate(cbind(all, poor) ~ STPU, poverty2001 , sum))
poverty2006 <- as.data.frame(aggregate(cbind(all, poor) ~ STPU, poverty2006 , sum))
poverty2011 <- as.data.frame(aggregate(cbind(all, poor) ~ STPU, poverty2011 , sum))

poverty1996$rate1996 <- poverty1996$poor/poverty1996$all
poverty2001$rate2001 <- poverty2001$poor/poverty2001$all
poverty2006$rate2006 <- poverty2006$poor/poverty2006$all
poverty2011$rate2011 <- poverty2011$poor/poverty2011$all

poverty1996$all1996 <- poverty1996$all
poverty2001$all2001 <- poverty2001$all
poverty2006$all2006 <- poverty2006$all
poverty2011$all2011 <- poverty2011$all

poverty <- merge(poverty, poverty1996[, c("STPU", "rate1996", "all1996")], by=c("STPU"), all.x = TRUE)
poverty <- merge(poverty, poverty2001[, c("STPU", "rate2001", "all2001")], by=c("STPU"), all.x = TRUE)
poverty <- merge(poverty, poverty2006[, c("STPU", "rate2006", "all2006")], by=c("STPU"), all.x = TRUE)
poverty <- merge(poverty, poverty2011[, c("STPU", "rate2011", "all2011")], by=c("STPU"), all.x = TRUE)

poverty$dif2011_1996 <- poverty$rate2011 - poverty$rate1996
poverty$dif2011_2006 <- poverty$rate2011 - poverty$rate2006
poverty$dif2011_2001 <- poverty$rate2011 - poverty$rate2001

write.csv(poverty, file = 'poverty_STPU_1996to2011.csv')

# calculate standardized mortality rates----
setwd(sprintf("~%s/poverty/monograph/geospatial/data", setpath))
pop <- openxlsx::read.xlsx('population_sex_age_STPU_2016.xlsx', sheet = 'T1.1b_stpug')
who_pop <- openxlsx::read.xlsx('population_sex_age_STPU_2016.xlsx', sheet = 'WHO')
names(who_pop)[names(who_pop)=='age'] <- 'age16'

# convert age in days & months to year
df$Age <- ifelse(df$Age == 'XXX', NA, df$Age)
df$Age <- as.numeric(df$Age)
df$age <- ifelse(df$`Age definition` == "Y", df$Age,
                 ifelse(df$`Age definition` == "M", df$Age/12,
                        ifelse(df$`Age definition` == "D", df$Age/365, NA)
                 ))
df$age16 <- recode_age(df$age, second_group = 5, interval = 5, last_group = 75)

df$Sex <- car::recode(df$Sex, "
'F' = 'Female';
'M' = 'Male';
'U' = NA
")
# get mortality counts by STPU, sex, and age groups
mortality <- as.data.frame(table(df$STPU, df$Sex, df$age16, dnn = list('STPU', 'sex', 'age16')), responseName = "freq")

pop <- reshape(pop, 
               direction = "long",
               varying = list(names(pop)[3:18]),
               v.names = "pop", 
               timevar = "age16",
               times = as.vector(names(pop)[3:18]))
pop <- pop[, c('STPU', 'sex', 'age16', 'pop')] # drop STPUG variable in pop

# get population by STPU, sex, and age groups
mortality <- merge(mortality, pop[which(pop$STPU %in% unique(mortality$STPU)),], by = c('STPU', 'sex', 'age16'), all.x = TRUE) # remove STPUs from pop that are not in mortality to avoid warnings
# calculate crude mortality rate
mortality$rate <- mortality$freq / mortality$pop * 100000
mortality <- merge(mortality, who_pop, by = c('age16'), all.x = TRUE) # get WHO standard 
# calculate standardized mortality rate (SMR) by sex & STPU
mortality$ratebypop <- mortality$rate* mortality$WHO_pop
SMR <- with(mortality, rowsum(ratebypop/99999, paste(sex, STPU), na.rm = TRUE)) %>% as.data.frame()
SMR$sex_STPU <- rownames(SMR)
SMR$sex <- before_char(SMR$sex_STPU, " ")
SMR$STPU <- after_char(SMR$sex_STPU, " ")
names(SMR)[names(SMR)=='V1'] <- 'SMR'
SMR <- within(SMR, rm(sex_STPU))
# calculate standardized premature mortality rate (SPMR) by sex & STPU
mortality$pre75ratebypop <- ifelse(mortality$age16 %in% '75+', 0, mortality$ratebypop)
SPMR <- with(mortality, rowsum(pre75ratebypop/99999, paste(sex, STPU), na.rm = TRUE)) %>% as.data.frame()
SPMR$sex_STPU <- rownames(SPMR)
SPMR$sex <- before_char(SPMR$sex_STPU, " ")
SPMR$STPU <- after_char(SPMR$sex_STPU, " ")
names(SPMR)[names(SPMR)=='V1'] <- 'SPMR'
SPMR <- within(SPMR, rm(sex_STPU))

# calculate SMR by STPU (male & female combined)
mortality_all <- mortality %>% group_by(age16, STPU) %>% summarise_at(c('freq', 'pop'), sum, na.rm = TRUE) # combine sex-specific freq & pop for each age16 & STPU group
mortality_all$rate <- mortality_all$freq / mortality_all$pop * 100000
mortality_all <- merge(mortality_all, who_pop, by = c('age16'), all.x = TRUE) # get WHO standard 
mortality_all$ratebypop <- mortality_all$rate* mortality_all$WHO_pop
SMR_all <- with(mortality_all, rowsum(ratebypop/99999, paste(STPU), na.rm = TRUE)) %>% as.data.frame()
SMR_all$STPU <- rownames(SMR_all)
names(SMR_all)[names(SMR_all)=='V1'] <- 'SMR'
# calculate SPMR by STPU (male & female combined)
mortality_all$pre75ratebypop <- ifelse(mortality_all$age16 %in% '75+', 0, mortality_all$ratebypop)
SPMR_all <- with(mortality_all, rowsum(pre75ratebypop/99999, paste(STPU), na.rm = TRUE)) %>% as.data.frame()
SPMR_all$STPU <- rownames(SPMR_all)
names(SPMR_all)[names(SPMR_all)=='V1'] <- 'SPMR'

setwd(sprintf("~%s/poverty/monograph/geospatial", setpath))
# write.csv(SMR[SMR$sex=='Male',], file = 'SMR_male.csv')
# write.csv(SMR[SMR$sex=='Female',], file = 'SMR_female.csv')
write.csv(SMR_all, file = 'SMR_all.csv')

# write.csv(SPMR[SPMR$sex=='Male',], file = 'SPMR_male.csv')
# write.csv(SPMR[SPMR$sex=='Female',], file = 'SPMR_female.csv')
write.csv(SPMR_all, file = 'SPMR_all.csv')


# GIS charting ----
library(sf)
library(ggmap)
library(ggplot2)
setwd(sprintf("~%s/poverty/monograph/mortality/", setpath))
s.sf <- st_read("stpu_2016/STPU_2016.shp")
plot(st_geometry(s.sf), col = sf.colors(n = 214, categorical = TRUE), border = 'grey',
     axes = FALSE)


register_google(key = as.character(read.csv("~/MEGAsync/Work/HKU/google_api.txt")[1, 2]) )
# provide your own Google Maps Platform (Google Clouds) API key in the following format
# "","x"
# "1","api-key"

hk_map <- get_map(location = c(lon = 114.1694, lat = 22.3193), 
                  maptype = "roadmap",
                  source = "google",
                  zoom = 10)

plot(hk_map)
plot(st_geometry(s.sf), col = sf.colors(n = 214, categorical = TRUE), border = 'grey',
     axes = FALSE,
     add = TRUE)
