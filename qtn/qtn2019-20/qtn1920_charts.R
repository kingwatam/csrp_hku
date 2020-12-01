rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/RA HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(ggplot2)

SCHOOL <- array(c(c("primary", "secondary"),                      # switch between universal primary schools [1,1] & universal secondary schools [2,1]
                  c("primary_selective", "secondary_selective")), # switch between selective primary schools [1,2] & selective secondary schools [2,2]
                dim=c(2,2))                                         [2,1] # <- switch here

LEVEL <-  c("level1", "level2", "level3",
            "level1_both", "level2_both", "level3_both",
            "level23", "") [7] # <- input program level (based on Excel file sheet names)
# INDEX <- 1 # input outcome index
upper_perc <- 0.85 # y-axis lower bound as percentage of range (e.g. 0.85 means 85% of the maximum value)
lower_perc <- 1-upper_perc # y-axis lower bound as percentage of range

# Outcomes ----
if (SCHOOL == "primary"){
  questions_level1 <- t(array(c(c("q1", "Mental Health Knowledge", "0-21"),  
                                c("q2", "Subjective Happiness", "1-7"),  
                                c("q3", "Anxiety", "0-18"),  
                                c("q4neg", "Negative Thinking", "0-40"), 
                                c("q4pos", "Positive Thinking", "0-40"),
                                c("q5", "Empathy","0-24"),
                                c("q6", "Self-esteem", "10-40"), 
                                c("q7", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 8)))
  
  questions_level2 <- t(array(c(c("q1", "Mental Health Knowledge", "0-13"),  
                                c("q2", "Subjective Happiness", "4-28"),  
                                c("q3neg", "Negative Affect", "10-50"),  
                                c("q3pos", "Positive Affect", "10-50"),  
                                c("q4a", "Strengths Knowledge", "8-56"),  
                                c("q4b", "Strengths Use", "14-98"),  
                                c("q5neg", "Negative Thinking", "0-40"), 
                                c("q5pos", "Positive Thinking", "0-40"),
                                c("q6", "Empathy","0-24"),
                                c("q7", "Self-esteem", "10-40"), 
                                c("q8", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 11)))
  
  questions_level3 <- t(array(c(c("q1", "Mental Health Knowledge", "0-15"),  
                                c("q2", "Subjective Happiness", "4-28"),  
                                c("q3neg", "Negative Affect", "10-50"),  
                                c("q3pos", "Positive Affect", "10-50"),  
                                c("q4a", "Strengths Knowledge", "8-56"),  
                                c("q4b", "Strengths Use", "14-98"),  
                                c("q5neg", "Negative Thinking", "0-40"), 
                                c("q5pos", "Positive Thinking", "0-40"),
                                c("q6", "Empathy","0-24"),
                                c("q7", "Self-esteem", "10-40"), 
                                c("q8", "Gratitude", "6-42")
                                # c("q9a", "Compassion (CS)", "1-5"), 
                                # c("q9b", "Self-compassion (SCS)", "1-5"), 
                                # c("q10a_b", "Help-seeking", "1-2"), 
                                # c("q10c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 11)))
  
} else if(SCHOOL == "secondary") {
  questions_level1 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-20"),  
                                c("q3", "Psychological Distress", "0-36"),  
                                c("q4neg", "Negative Thinking", "0-40"), 
                                c("q4pos", "Positive Thinking", "0-40"),
                                # c("q5a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q5a2", "Life Satisfaction (1-item)", "1-7"),
                                c("q5b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q6", "Empathy", "0-24"), 
                                c("q7", "Gratitude", "6-42")
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-3")
  ), dim = c(3, 7)))
  questions_level2 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-14"),  
                                c("q3", "Empathy (C-IRI)", "0-88"),  
                                c("q3_PD", "Empathy (C-IRI) - Personal Distress", "0-28"),
                                c("q3_FS", "Empathy (C-IRI) - Fantasy", "0-16"),
                                c("q3_ES", "Empathy (C-IRI) - Empathy", "0-44"),
                                c("q4", "Empathy Quotient", "0-44"),
                                c("q5a", "Emotional Competence", "6-36"),
                                c("q5b", "Behavioural Competence", "6-36"),
                                # c("q6a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q6a2", "Life Satisfaction (1-item)","1-7"),
                                c("q6b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q7", "Psychological Distress", "0-36") 
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-2")
  ), dim = c(3, 10)))
  
  questions_level3 <- t(array(c(c("q1_2", "Mental Health Knowledge", "0-14"),  
                                c("q3", "Empathy (C-IRI)", "0-88"),  
                                c("q3_PD", "Empathy (C-IRI) - Personal Distress", "0-28"),
                                c("q3_FS", "Empathy (C-IRI) - Fantasy", "0-16"),
                                c("q3_ES", "Empathy (C-IRI) - Empathy", "0-44"),
                                c("q5a", "Emotional Competence", "6-36"),
                                c("q5b", "Behavioural Competence", "6-36"),
                                # c("q6a", "Life Satisfaction (SLSS)","7-42"),
                                # c("q6a2", "Life Satisfaction (1-item)","1-7"),
                                c("q6b", "Life Satisfaction (BMSLSS)", "5-35"),
                                c("q7", "Psychological Distress", "0-36") 
                                # c("q8a", "Compassion (CS)", "1-5"), 
                                # c("q8b", "Self-compassion (SCS)", "1-5"), 
                                # c("q9a_b", "Help-seeking", "1-2"), 
                                # c("q9c", "Prejudice (PPMI)", "1-2")
  ), dim = c(3, 9)))
  
  
} else if(SCHOOL == "primary_selective") {
  questions <- t(array(c(c("q1", "Subjective Happiness", "4-28"),  
                         c("q2neg", "Negative Affect", "10-50"),  
                         c("q2pos", "Positive Affect", "10-50"),  
                         c("q3", "Self-esteem", "10-40"), 
                         c("q4a", "Strengths Knowledge", "8-56"),  
                         c("q4b", "Strengths Use", "14-98"),  
                         c("q5neg", "Negative Thinking", "0-40"), 
                         c("q5pos", "Positive Thinking", "0-40")
  ), dim = c(3, 8)))
  
} else if(SCHOOL == "secondary_selective") {
  
}

 

# Create charts ----
if (SCHOOL == "primary"){
  setwd(sprintf("~%s/qtn/qtn2019-20/%s", setpath, "/primary/results"))
  df <- openxlsx::read.xlsx("qtn1920_primary_results.xlsx", sheet = LEVEL)
  TITLE <- "Primary School Universal Program"

} else if(SCHOOL == "secondary") {
  setwd(sprintf("~%s/qtn/qtn2019-20/%s", setpath, "/secondary/results"))
  df <- openxlsx::read.xlsx("qtn1920_secondary_results.xlsx", sheet = LEVEL)
  TITLE <- "Secondary School Universal Program"

} else if(SCHOOL == "primary_selective") {
  setwd(sprintf("~%s/qtn/qtn2019-20/%s", setpath, "/primary/results"))
  df <- openxlsx::read.xlsx("qtn1920_primary_selective_results.xlsx")
  LEVEL <- ""
  TITLE <- "Primary School Selective Program"
}

level_num <- as.numeric(paste0(stringr::str_extract_all(LEVEL, "[0-9]")[[1]], collapse = ""))
if (level_num == 1){
  questions <- questions_level1
} else if (level_num == 2){
  questions <- questions_level2
} else if (level_num == 3){
  questions <- questions_level3
} else if (LEVEL %in% "level23"){
  questions <- questions_level2
}

df <- iferror({df %>% 
  select(Outcome.Measure, Intervention.Group.T0, Intervention.Group.T1, Control.Group.T0, Control.Group.T1)},
  {df %>% 
      select(Outcome.Measure, Intervention.Group.T0, Intervention.Group.T1)})

df <- df[-seq(2, nrow(df), 2),]

df <- reshape2::melt(df, id.vars = c("Outcome.Measure"),
                  variable.name = "group", 
                  value.name = "score")

df$group <- as.character(df$group)
df$time <- after_char(df$group, ".")

df$group <- car::recode(df$group, "
'Intervention.Group.T0' = 'Intervention Group';
'Intervention.Group.T1' = 'Intervention Group';
'Control.Group.T0' = 'Control Group';
'Control.Group.T1' = 'Control Group'
")

df$time <- car::recode(df$time, "
'Group.T0' = 'Pre-test (T0)';
'Group.T1' = 'Post-test (T1)'
")

for (INDEX in 1:nrow(questions)){
  outcome <- unique(df$Outcome.Measure)[INDEX]
  abbrev <- questions[INDEX,1]
  range <- questions[INDEX,3]
  upper_ylim <- upper_perc * (as.numeric(after_char(range,"-"))-as.numeric(before_char(range,"-"))) + as.numeric(before_char(range,"-"))
  lower_ylim <- lower_perc * (as.numeric(after_char(range,"-"))-as.numeric(before_char(range,"-"))) + as.numeric(before_char(range,"-"))
  
  df$score <- as.numeric(df$score)
  df$time <- factor(df$time, levels = c("Pre-test (T0)", "Post-test (T1)"))
  df$group <- factor(df$group, levels = c("Intervention Group", "Control Group"))
  
  chart <- 
    ggplot(data =  df[df$Outcome.Measure %in% outcome,], 
           aes(x = time, y =  score, group =  group)) +
    geom_line(aes(linetype = group)) +
    geom_point(size = 1) +
    ylim(c(lower_ylim, upper_ylim)) + 
    # scale_y_continuous(limits=c(0, NA)) + 
    labs(title = paste0(questions[INDEX,2], " - ", TITLE))
  
  setwd(sprintf("~%s/qtn/qtn2019-20/charts/%s/%s", setpath, SCHOOL, LEVEL))
  # Cairo::CairoWin()
  options(device="CairoWin")
  ggsave(chart, filename = sprintf('%s_%s_%s.png', SCHOOL, LEVEL, abbrev), dpi = 300, type = 'cairo',
         width = 8, height = 4, units = 'in')
}
