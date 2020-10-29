rm(list=ls())
graphics.off()
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

setwd(sprintf("~%s/qtn/qtn2018-19/qtn1819_secondary/qtn1819_secondary_selective_sdq", setpath))
library(dplyr) # pipe

temp = list.files(pattern="*.xlsx")
for (i in 1:length(temp)){
  assign(paste0(substring(temp[i], regexpr("dataentry_", temp[i])+10, regexpr("\\.xlsx", temp[i])-1)), openxlsx::read.xlsx(temp[i], sheet = 1))
}

LHK_LSK <- `Post_teacher SDQ_LHK + LSK - Copy`
LHK_LSK <- LHK_LSK[2:nrow(LHK_LSK),]
LHK_LSK[25,1] <- 1 # missing T0/T1 

MKES_pre <- `Pre_teacher SDQ_MKES`
MKES_pre <- MKES_pre[2:nrow(MKES_pre),]

LWL <- `Post_teacher SDQ_LWL`
LWL <- LWL[,-c(3)] # remove consent column

LSK_post <- `Post_teacher SDQ_LSK`
LSK_post <- LSK_post[2:nrow(LSK_post),]

rm(`Pre_Post_teacher SDQ_LWL`, `Post_teacher SDQ_LSK - Copy`, `teacher SDQ_LHK`, `Pre_teacher SDQ_LHK`) # duplicate files
rm(`Post_teacher SDQ_LHK + LSK - Copy`, `Pre_teacher SDQ_MKES`, `Post_teacher SDQ_LWL`, `Post_teacher SDQ_LSK`)

var_names <- c('Pre-test(0).Post.Test.(1)', 'School.Name', 'Int/Control.(1=int;.2=.control)', 'Name.of.student', 
               'Class', 'NO.', 'Gender.(1=M;.2=F)', 'Date.of.birth.(year)', 'Date.of.birth.(month)', 'Date.of.birth.(day)', 
               'Questionnaire.date.(year)', 'Questionnaire.date.(month)', 'Questionnaire.date.(day)', 'Ass2-P1-01', 'Ass2-P1-02', 
               'Ass2-P1-03', 'Ass2-P1-04', 'Ass2-P1-05', 'Ass2-P1-06', 'Ass2-P1-07', 'Ass2-P1-08', 'Ass2-P1-09', 'Ass2-P1-10', 'Ass2-P1-11', 
               'Ass2-P1-12', 'Ass2-P1-13', 'Ass2-P1-14', 'Ass2-P1-15', 'Ass2-P1-16', 'Ass2-P1-17', 'Ass2-P1-18', 'Ass2-P1-19', 'Ass2-P1-20', 
               'Ass2-P1-21', 'Ass2-P1-22', 'Ass2-P1-23', 'Ass2-P1-24', 'Ass2-P1-25', 'Ass2-P1-26', 'Ass2-P1-27', 'Ass2-P1-28', 'Ass2-P1-29', 
               'Ass2-P1-29b', 'Ass2-P1-30', '關注:')

for (dat in list(LHK_LSK, MKES_pre, LWL, LSK_post)){
  setnames(dat, old = (1:45), new = var_names)
  print(names(dat))
  rm(dat)
}


# variable names are inconsistent!!!
df <- plyr::rbind.fill(LHK_LSK, MKES_pre, LWL, LSK_post)
rm(LHK_LSK, MKES_pre, LWL, LSK_post)

