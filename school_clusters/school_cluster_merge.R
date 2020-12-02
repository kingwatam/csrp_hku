rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

setwd(sprintf("~%s/school_clusters", setpath))

library(jsonlite)
library(openxlsx)
library(magrittr) # pipe 

sch <- fromJSON("secondary_school_info.json", flatten=TRUE)
df <- openxlsx::read.xlsx("school_clustering.xlsx" )

for (i in (1:nrow(sch))){
  sch$cn_name[i] <- sch$school[i][[1]][1]
}

sch$cn_name <- gsub("（", "(", sch$cn_name, fixed = TRUE) # replace Chinese brackets with English brackets
sch$cn_name <- gsub("）", ")", sch$cn_name, fixed = TRUE) 

temp<- data.frame(rep(0,nrow(df)))
temp$cn_nameN <- sch$cn_name
temp$num_staff <- sch$staff_background.全校教師總人數

# similar to vlookup function in excel
df <- merge(df, unique(temp)[, c("cn_name", "num_staff")], by=c("cn_name"), all.x = TRUE)

# check matching
temp <- temp[order(temp$cn_name),]
temp$cn_name2 <- df$cn_name
temp$num_staff2 <- df$num_staff
# df$cn_name <- ifelse(grepl('月華', df$cn_name, ignore.case = TRUE), '月華', df$cn_name)

# PCA & k-means
df[,(1:15)]

write_excel("school_clustering_num_staff.xlsx", df)
