rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)

setwd(sprintf("~%s/qtn/qtn2019-20/primary/archive", setpath))
df <- readxl::read_xls("S10-2019-20.xls")
col_names <- data.frame( index = 15:131, name = names(df)[15:131])
names(df)[15:131] <- 15:131

names(df)[10] <- 'class' # having trouble evaluating (= or %in%) UTF-8 encoded Chinese characters
names(df)[11] <- 'class_number'
names(df)[12] <- 'DoB'
names(df)[13] <- 'sex'

q1a <- 15:27
q1b <- 28:29
q2 <- 31:34 # subjective happiness
q3 <- 35:54 # PANAS
q4 <- 55:76 # SKUS Strength knowledge & use
q5 <- 77:96 # CATS-P/N
q6 <- 97:102 # C-IRI
q7 <- 103:112 # RSES
q8 <- 113:118 # GQ-6
q9a <- 119:120
q9b <- 121:125
q9c1 <- 126
q9c2 <- 127
q9d <- 128:131

reverse_q1a <- q1a[c(2,5,6,8,9,11)]
df[reverse_q1a] <- (df[reverse_q1a]-1)*-1 # reverse (1:0) to (0:1)

df[q1b[1]] <- ifelse(df[q1b[1]] == 2, 1, 0) # B is correct
df[q1b[2]] <- ifelse(df[q1b[2]] == 3, 1, 0) # c is correct

reverse_q2 <- q2[c(4)] 
df[reverse_q2] <- (df[reverse_q2]-8)*-1 # reverse (1:7) to (7:1)

reverse_q3 <- q3[c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)] # Negative Affect questions

q4a <- q4[1:8]
q4b <- q4[9:22]

reverse_q4a <- q4a[c(2)]
df[reverse_q4a] <- (df[reverse_q4a]-8)*-1 # reverse (1:7) to (7:1)

reverse_q5 <- q5[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)] # Negative thinking

reverse_q6 <- q6[c(3)]
df[reverse_q6] <- (df[reverse_q6]-4)*-1 # reverse (0:4) to (4:0)

reverse_q7 <- q7[c(2, 5, 6, 8, 9)]
df[reverse_q7] <- (df[reverse_q7]-5)*-1 # reverse (1:4) to (4:1)

reverse_q8 <- q8[c(3, 6)]
df[reverse_q8] <- (df[reverse_q8]-8)*-1 # reverse (1:7) to (7:1)

reverse_q9d <- q9d[2]
df[reverse_q9d] <- (df[reverse_q9d]-3)*-1 # reverse (1:2) to (2:1)

df[q9d[4]] <- ifelse(df[q9d[4]] == 1, 1, 0) # 1 is correct

q9ba <- q9b[1:2]
q9bb <- q9b[3]
q9bc <- q9b[4:5]
q9da <- q9d[1:2]
q9db <- q9d[3]
q9dc <- q9d[4]

df <- df %>% 
  mutate(q1a = rowSums(.[q1a], na.rm = FALSE),
         q1b = rowSums(.[q1b], na.rm = FALSE), 
         q2 = rowSums(.[q2], na.rm = FALSE), # Subjective Happiness (SHS) range(4-28)
         q3neg = rowSums(.[reverse_q3], na.rm = FALSE),# Negative Affect (PANAS) range(10-50)
         q3pos = rowSums(.[q3[!(q3 %in% reverse_q3)]], na.rm = FALSE), # Positive Affect (PANAS) range(10-50)
         q4a = rowSums(.[q4a], na.rm = FALSE), # Strengths Knowledge Scale (SKS) range(8-56)
         q4b = rowSums(.[q4b], na.rm = FALSE), # Strengths Use Scale (SUS) range(14-98)
         q5neg = rowSums(.[reverse_q5], na.rm = FALSE),  # Negative Thinking (CATS-N/P) range(0-40)
         q5pos = rowSums(.[q5[!(q5 %in% reverse_q5)]], na.rm = FALSE),  # Positive Thinking (CATS-N/P) range(0-40)
         q6 = rowSums(.[q6], na.rm = FALSE), # C-IRI perspective taking range(0-24)
         q7 = rowSums(.[q7], na.rm = FALSE), # Self-esteem (RSES) range(10-40)
         q8 = rowSums(.[q8], na.rm = FALSE), # Gratitude GQ-6 range(6-42)
         q9a = rowMeans(.[q9a], na.rm = FALSE), # Compassion - common humanity subscale range(0-4), mean instead of sum
         q9b = rowMeans(.[q9b], na.rm = FALSE), # self-compassion range(0-4), mean instead of sum
         q9ba = rowMeans(.[q9ba], na.rm = FALSE), # self-compassion - self-kindness subscale range(0-4), mean instead of sum
         q9bb = .[[q9bb]], # self-compassion - self-judgement subscale range(0-4), mean instead of sum
         q9bc = rowMeans(.[q9bc], na.rm = FALSE), # self-compassion - common humanity subscale range(0-4), mean instead of sum
         # q9c = rowSums(.[q9c], na.rm = FALSE), # help-seeking, range(2-5)
         q9da =  rowMeans(.[q9da], na.rm = FALSE), # prejudice - fear/avoidance subscale range(0-2), mean instead of sum
         q9db =  .[[q9db]], # prejudice - unpredictability subscale range(0-2), mean instead of sum
         q9d2 =  .[[q9dc]] # understanding subscale range(0-1), mean instead of sum
  )

df$q1 <- df$q1a + df$q1b # Mental Health Knowledge range(0-15)
df$q9d1 <- (df$q9da*2 + df$q9db)/3 # weighted average because q9da has two questions

df$q9c1 <- as.factor(df[[q9c1]])
levels(df$q9c1) <- c("Yes","No","Not sure")

df$q9c2 <- as.factor(df[[q9c2]])
levels(df$q9c2) <- c("Yes","No")

df$q9d2 <- as.factor(df$q9d2)
levels(df$q9d2) <- c("Incorrect","Correct")

outcomes <- t(array(c(c("q1", "Mental Health Knowledge"), 
                     c("q2", "Subjective Happiness (SHS)"),  
                     c("q3neg", "Negative Affect (PANAS)"),  
                     c("q3pos", "Positive Affect (PANAS)"), 
                     c("q4a", "Strengths Knowledge Scale (SKS)"),
                     c("q4b", "Strengths Use Scale (SUS)"), 
                     c("q5neg", "Negative Thinking (CATS-N/P)"),  
                     c("q5pos", "Positive Thinking (CATS-N/P)"),
                     c("q6", "empathy - perspective taking (C-IRI)"),
                     c("q7", "Self-esteem (RSES)"),
                     c("q8", "Gratitude (GQ-6)"),
                     c("q9a", "Compassion - common humanity subscale"),
                     c("q9b", "Self-compassion"),
                     c("q9c1", "Help-seeking 1"),
                     c("q9c2", "Help-seeking 2"),
                     c("q9d1", "Prejudice"),
                     c("q9d2", "Understanding")
                     ), dim = c(2,17)))

# tabulate results for S10 ----
for (var in outcomes[,1]){
  cat(outcomes[outcomes[,1] %in% var, 1], '\n')
  cat(outcomes[outcomes[,1] %in% var, 2], '\n')
  if (var %in% c('q9c1', 'q9c2', 'q9d2')){
    summary <- eval_('aggregate(', var, '~ class+sex, df, table)'
    )
    } else {
    summary <- eval_('aggregate(', var, '~ class+sex, df, function(x) c(n = round_format(length(x), 0),
                                                      mean = round_format(mean(x), 3),
                                                      sd = round_format(sd(x), 3)
                                                      ))'
    )
    }
  assign(paste0('summary_', var), do.call(data.frame, summary)) # do.call puts each vector (column) of the list as arguement in func data.frame(), otherwise multiple columns are in one from aggregate()
  print(summary)
}

write_excel("S10_scores.xlsx", df[c(10:13,132:155)], remove_char = " ")

eval_('write_excel("S10_summary_class_sex.xlsx", ',
      paste(sprintf('summary_%s',  outcomes[,1]), collapse = ', '),
      ', remove_char = "summary_")')