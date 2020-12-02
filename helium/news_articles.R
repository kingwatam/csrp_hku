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

setwd(sprintf("~%s/helium/news", setpath))

df <- as.data.frame(readxl::read_excel("helium_suicide_news_v1_king.xlsx",
                          sheet = "Coding", guess_max = 3127)) # guess_max is required, otherwise a row of data is imported incorrectly due to data type differences

names(df)[names(df)=='gas/tool_1'] <- 'gas_1'
names(df)[1] <- 'news_id'

df$suicide_news[which(df$news_id == 498)]  <- 1 # manually code this as suicide news as the keyword filter wasn't able to pick this up (the news article didn't mention suicide-related terms)

# # wrong groups of variables (variable of the same name should be in the same group)
# for (i in 1:6){
#   dynamically create variable names for 1st to 6th suicide of each new article
#   variables_list <- rlist::list.append(variables_list,
#                                        sprintf(sprintf("%s",
#                                                        c('Sui_case_status_%s',
#                                                          'case_nature_%s',
#                                                          'gas_%s',
#                                                          'Sui_yr_%s',
#                                                          'Sui_mth_%s',
#                                                          'Sui_dd_%s',
#                                                          'Gender_%s',
#                                                          'Age_%s',
#                                                          'inc_dist_%s',
#                                                          'inc_est_%s',
#                                                          'inc_blk_%s',
#                                                          'sui_note_%s',
#                                                          'sui_content_%s',
#                                                          'r_case_nature_%s.1',
#                                                          'r_case_nature_%s.2',
#                                                          'r_case_nature_%s.3')), i)
#   )
# }

variables_list <- list()

for (j in c('Sui_case_status_%s',
            'case_nature_%s',
            'gas_%s',
            'Sui_yr_%s',
            'Sui_mth_%s',
            'Sui_dd_%s',
            'Gender_%s',
            'Age_%s',
            'inc_dist_%s',
            'inc_est_%s',
            'inc_blk_%s',
            'sui_note_%s',
            'sui_content_%s',
            'r_case_nature_%s.1',
            'r_case_nature_%s.2',
            'r_case_nature_%s.3')){
  variables_list <- rlist::list.append(variables_list, sprintf(j, 1:6))
}

dflong <- reshape(df, # df[which(df$Sui_case >=1),],
                  direction="long", 
                  varying=variables_list, 
                  v.names=c('Sui_case_status', # this vector corresponds to the grouping in varying (variables_list)
                            'case_nature',
                            'gas_tool',
                            'Sui_yr',
                            'Sui_mth',
                            'Sui_dd',
                            'Gender',
                            'Age',
                            'inc_dist',
                            'inc_est',
                            'inc_blk',
                            'sui_note',
                            'sui_content',
                            'r_case_nature_1',
                            'r_case_nature_2',
                            'r_case_nature_3'), 
                  timevar= "nth_person", idvar="document_id") 

# write_excel('potential_helium_news_2007-2019.xlsx', dflong[which(!is.na(dflong$Sui_yr)),])

dflong <- dflong[which((dflong$Sui_case == 1 & !is.na(dflong$Sui_yr)) | (dflong$Sui_case == 0 & dflong$nth_person == 1)),]  # only keep non-empty rows for suicide news, and keep the first row for non-suicide news
write_excel('potential_helium_news_2007-2019.xlsx', dflong) 
