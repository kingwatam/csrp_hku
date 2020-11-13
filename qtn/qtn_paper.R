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
library(ggplot2)
library(lme4)
library(lmerTest) # calculate p-values in summary()
library(multcomp) # glht
library(ggpubr) # ggarrange

setwd(sprintf("~%s/qtn/", setpath))
df <- readRDS("qtn1519_secondary_long.rds")

df <- df %>%
  mutate(uid = paste(year, sch, class, student_num, dob, sex)) 
# df %>%
#   summarize(unique_id = n_distinct(uid))
df$uid <- as.factor(df$uid)
levels(df$uid) <- 1:n_distinct(df$uid)

df$dob <- as.Date(df$dob)
df$submitdate <- as.Date(df$submitdate)

questions <- t(array(c(c("q1_2", "Mental Health Knowledge"),  
                       c("q3", "Psychological Distress (GHQ)"),  
                       c("q4neg", "Negative Thinking"), 
                       c("q4pos", "Positive Thinking"), 
                       c("q5a", "Life Satisfaction (SLSS)"),
                       c("q5b", "Life Satisfaction (BMSLSS)"),
                       c("q6", "Empathy: Perspective Taking (C-IRI)"),  
                       c("q7", "Gratitude (GQ-6)")), dim = c(2,8)))

# regression analysis ----
reg_table <- function(data, within_group = FALSE, random_effect = '~ 1 | sch/uid'){
  require(nlme)
  require(dplyr)
  table <- data.frame(matrix(ncol = 10,  nrow = 0))
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    df <- eval_("data[which(!is.na(data$", dep_var, ")),]")
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    row_count <- row_count + 1
    
    colnames(table)  <-  c("Variable", "N", "Beta", "Lower CI", "Upper CI",  "p-value", "significance", "adj. p-value", "significance2", "L.Ratio")
    
    if (within_group){
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~T1+age+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~T1+age+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    } else {
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~intervention*T1+age+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      # iferror(
      #   fit_lm <- eval_(
      #     "lme(", dep_var, "~intervention*T1+age+gender, 
      # data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      # random = ~ 1  | sch/uid, 
      # na.action=na.omit, control=lmeControl(opt='optim'))")
      #   , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~intervention*T1+age+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    }
    
    for (var in c('(Intercept)', 'gendermale', 'age', 'T1', 'intervention', 'intervention:T1')){
      if (within_group){
        if (var %in% c('intervention', 'intervention:T1')){
          next
        }
      }
      
      table[row_count, 1] <- var
      
      n <- iferror(summary(fit)$dim$N, NA)
      beta <- iferror(summary(fit)$tTable[var, 1], NA)
      se <-  iferror(summary(fit)$tTable[var, 2], NA)
      
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
      p_value <- iferror(summary(fit)$tTable[var, 5], NA)
      table[row_count, 2] <-  n/2
      table[row_count, 3] <-  round_format(beta, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  format(p_value, scientific = FALSE)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    }
    table <- format(table, scientific = FALSE)
    table[is.na(table)] <- ""
    table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  }
  return(table)
}

reg_table_form <- function(data, within_group = FALSE, random_effect = '~ 1 | sch/uid'){
  require(nlme)
  require(dplyr)
  table <- data.frame(matrix(ncol = 10,  nrow = 0))
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    df <- eval_("data[which(!is.na(data$", dep_var, ")),]")
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    row_count <- row_count + 1
    
    colnames(table)  <-  c("Variable", "N", "Beta", "Lower CI", "Upper CI",  "p-value", "significance", "adj. p-value", "significance2", "L.Ratio")
    
    if (within_group){
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~T1+form+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~T1+form+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    } else {
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~intervention*T1+form+gender, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      # iferror(
      #   fit_lm <- eval_(
      #     "lme(", dep_var, "~intervention*T1+form+gender, 
      # data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      # random = ~ 1  | sch/uid, 
      # na.action=na.omit, control=lmeControl(opt='optim'))")
      #   , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~intervention*T1+form+gender, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    }
    
    for (var in c('(Intercept)', 'gendermale', 'form', 'T1', 'intervention', 'intervention:T1')){
      if (within_group){
        if (var %in% c('intervention', 'intervention:T1')){
          next
        }
      }
      
      table[row_count, 1] <- var
      
      n <- iferror(summary(fit)$dim$N, NA)
      beta <- iferror(summary(fit)$tTable[var, 1], NA)
      se <-  iferror(summary(fit)$tTable[var, 2], NA)
      
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
      p_value <- iferror(summary(fit)$tTable[var, 5], NA)
      table[row_count, 2] <-  n/2
      table[row_count, 3] <-  round_format(beta, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  format(p_value, scientific = FALSE)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    }
    table <- format(table, scientific = FALSE)
    table[is.na(table)] <- ""
    table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  }
  return(table)
}

reg_table_yr <- function(data, within_group = FALSE, random_effect = '~ 1 | sch/year/uid'){
  require(nlme)
  require(dplyr)
  table <- data.frame(matrix(ncol = 10,  nrow = 0))
  row_count <- 1
  for (dep_var in questions[,1]){
    print(dep_var)
    
    df <- eval_("data[which(!is.na(data$", dep_var, ")),]")
    
    table[row_count, 1] <- questions[which(questions==dep_var),2]
    row_count <- row_count + 1
    
    colnames(table)  <-  c("Variable", "N", "Beta", "Lower CI", "Upper CI",  "p-value", "significance", "adj. p-value", "significance2", "L.Ratio")
    
    if (within_group){
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~T1+age+gender+year, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~T1+age+gender+year, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    } else {
      iferror(
        fit <- eval_(
          "lme(", dep_var, "~intervention*T1+age+gender+year, 
      data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      random = ", random_effect, ", 
      na.action=na.omit, control=lmeControl(opt='optim'))")
        , {row_count <- row_count + 1 ; next})
      
      # iferror(
      #   fit_lm <- eval_(
      #     "lme(", dep_var, "~intervention*T1+age+gender, 
      # data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),], 
      # random = ~ 1  | sch/uid, 
      # na.action=na.omit, control=lmeControl(opt='optim'))")
      #   , {row_count <- row_count + 1 ; next})
      
      iferror(
        fit_lm <- eval_("lm(", dep_var, "~intervention*T1+age+gender+year, data = df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),]
                         , na.action=na.omit)")
        , {row_count <- row_count + 1 ; next})
      
      table[row_count, 10] <- paste(round_format(anova(fit, fit_lm)[2,'L.Ratio'], 1),
                                    round_format(anova(fit, fit_lm)[2,'p-value'], 2),
                                    sep = ", ")
    }
    
    for (var in c('(Intercept)', 'year2016-17', 'year2017-18', 'year2018-19', 'gendermale', 'age', 'T1', 'intervention', 'intervention:T1')){
      if (within_group){
        if (var %in% c('intervention', 'intervention:T1')){
          next
        }
      }
      
      table[row_count, 1] <- var
      
      n <- iferror(summary(fit)$dim$N, NA)
      beta <- iferror(summary(fit)$tTable[var, 1], NA)
      se <-  iferror(summary(fit)$tTable[var, 2], NA)
      
      lowerCI <-  iferror(beta + qnorm(0.025) * se, NA)
      upperCI <- iferror(beta + qnorm(0.975) * se, NA)
      p_value <- iferror(summary(fit)$tTable[var, 5], NA)
      table[row_count, 2] <-  n/2
      table[row_count, 3] <-  round_format(beta, 5)
      table[row_count, 4] <-  round_format(lowerCI, 5)
      table[row_count, 5] <-  round_format(upperCI, 5)
      table[row_count, 6] <-  format(p_value, scientific = FALSE)
      table[row_count, 7] <-  starred_p(p_value, 3)
      
      row_count <- row_count + 1
    }
    table <- format(table, scientific = FALSE)
    table[is.na(table)] <- ""
    table[3:6] <- mutate_all(table[3:6], function(x) as.numeric(as.character(x)))
  }
  return(table)
}

# questions <- t(array(c(
#                        c("q6", "Empathy: Perspective Taking (C-IRI)")
#                        ), dim = c(2,1)))

table <- reg_table(df, random_effect = '~ 1 | sch/year/uid')
table <- reg_table_form(df, random_effect = 'list(sch = ~ 1+intervention*T1, year = ~ 1, uid = ~ 1)') 

table <- reg_table(df[which(df$intervention==1),], within_group = TRUE)

# lmer plot mixed effects estimates of intervention:T1 ----
yearname <- c('2015-16', '2016-17', '2017-18', '2018-19')
for (wave in unique(df$year)){
  for (dep_var in questions[,1]){
    if (all(is.na(df[df$year %in% wave, dep_var]))){
      next
    }
    print(wave)
    print(dep_var)
    dat <- df[df$year %in% wave,]
    fit <- eval_(
      "lmer(", dep_var, "~ 1+intervention*T1+age+gender + (1 | sch:uid) +  (1+intervention*T1 | sch) , 
                      REML = TRUE, data = dat[which(duplicated(dat$uid,  fromLast = FALSE) | duplicated(dat$uid,  fromLast = TRUE)),])")
    FE <- merTools::FEsim(fit, n.sims = 1000)
    RE <- merTools::REsim(fit, n.sims = 1000)
    
    matrix_nrow <- nrow(RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1', c('mean', 'median', 'sd')])
    RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1', c('mean', 'median', 'sd')] <- 
      sweep(RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1', c('mean', 'median', 'sd')], MARGIN = 2, STAT = as.matrix(FE[FE$term %in% 'intervention:T1', c('mean', 'median', 'sd')]), FUN = "+")
    
    temp <- RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1',] 
    sch_names <- temp$groupID[order(temp$median)]
    
    # chart <- merTools::plotREsim(RE, facet= list(groupFctr= "sch", term ='intervention:T1'), labs = TRUE) +
    # scale_x_discrete(labels = sch_names) + xlab(label =  "School") + labs(title =  paste0(questions[which(questions==dep_var),2], ", ", yearname[wave]))
    
    setwd(sprintf("~%s/qtn/draft_paper/plots", setpath))
    ggsave(paste0("year", wave, "_", dep_var, ".png"), plot = chart, width = 12, height = 4)
    rm(chart)
  }
}

# lmer plot mixed effects estimates of intervention:T1, no fixed-effects for int*T1 ----
yearname <- c('2015-16', '2016-17', '2017-18', '2018-19')
for (wave in unique(df$year)){
  for (dep_var in questions[,1]){
    if (all(is.na(df[df$year %in% wave, dep_var]))){
      next
    }
    print(wave)
    print(dep_var)
    dat <- df[df$year %in% wave,]
    fit <- eval_(
      "lmer(", dep_var, "~ 1+age+gender + (1 | sch:uid) +  (1+intervention*T1 | sch) , 
                      REML = TRUE, data = dat[which(duplicated(dat$uid,  fromLast = FALSE) | duplicated(dat$uid,  fromLast = TRUE)),])")
    FE <- merTools::FEsim(fit, n.sims = 1000)
    RE <- merTools::REsim(fit, n.sims = 1000)
    
    matrix_nrow <- nrow(RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1', c('mean', 'median', 'sd')])
    
    temp <- RE[RE$groupFctr %in% 'sch' & RE$term %in% 'intervention:T1',] 
    sch_names <- temp$groupID[order(temp$median)]
    
    chart <- merTools::plotREsim(RE, facet= list(groupFctr= "sch", term ='intervention:T1'), labs = TRUE) +
      scale_x_discrete(labels = sch_names) + xlab(label =  "School") + labs(title =  paste0(questions[which(questions==dep_var),2], ", ", yearname[wave]))
    
    setwd(sprintf("~%s/qtn/draft_paper/plots", setpath))
    ggsave(paste0("year", wave, "_", dep_var, ".png"), plot = chart, width = 12, height = 4)
    rm(chart)
  }
}

# single-school model, between-group (intervention*T1) and within-group (T1, T1+int*T1) pre-post difference ----
yearname <- c('2015-16', '2016-17', '2017-18', '2018-19')
data <- data.frame()
for (wave in c(1:4)){
  for (dep_var in questions[,1]){
    if (all(is.na(df[df$year %in% wave, dep_var]))){
      next
    }
    print(wave)
    print(dep_var)
    for (sch in unique(df$sch)){
      dat <- df[df$year %in% wave & df$sch %in% sch,]
      dat <- dat %>% 
        mutate(dob = as.Date(as.numeric(dob), origin = "1970-01-01"), submitdate = as.Date(as.numeric(submitdate), origin = "1970-01-01")) %>% 
        filter(!is.na(!!as.symbol(dep_var)) & !is.na(T1) & !is.na(gender) & !is.na(age)) %>% # not missing any of these variables before next step, otherwise there might be individuals with only pre/post and not both
        group_by(uid) %>% 
        filter(n() == 2) # both pre & post 
      # dat <- dat[which(duplicated(dat$uid,  fromLast = FALSE) | duplicated(dat$uid,  fromLast = TRUE)),]
      if (length(unique(na.omit(dat$intervention))) <2){
        next
      }
      if (length(unique(na.omit(dat$gender))) == 2){
        fit <- trycatchNA(eval_(
          "lmer(", dep_var, "~ 1+age+gender+T1*intervention + (1 | uid) , 
                      REML = TRUE, data = dat)"))
      } else {
        fit <- trycatchNA(eval_(
          "lmer(", dep_var, "~ 1+age+T1*intervention + (1 | uid) , 
                      REML = TRUE, data = dat)"))
      }

      data <- trycatch_(rbind(data, data.frame(question = questions[questions[, 1] %in% dep_var, 2], 
                                               year = yearname[wave], 
                                               school = sch, 
                                               N = summary(fit)$devcomp$dims["N"]/2,
                                               N_ctrl = dat %>%
                                                   filter(intervention == 0) %>%
                                                   nrow()/2,
                                               T1 = summary(fit)$coef["T1", 1],
                                               SE_1 = summary(fit)$coef["T1", 2],
                                               T1_intervention = summary(fit)$coef["T1:intervention", 1],
                                               SE_2 = summary(fit)$coef["T1:intervention", 2],
                                               N_int = dat %>%
                                                 filter(intervention == 1) %>%
                                                 nrow()/2,
                                               T1_T1_intervention = summary(fit)$coef["T1", 1] + summary(fit)$coef["T1:intervention", 1],
                                               SE_3 = sqrt(vcov(fit)['T1', 'T1'] + vcov(fit)['T1:intervention', 'T1:intervention'] + 2* vcov(fit)['T1', 'T1:intervention']),
                                               Age_ctrl = dat %>% ungroup() %>% 
                                                 filter(intervention == 0) %>% summarise(mean(age, na.rm = TRUE)) %>% as.double(),
                                               SD_ctrl = dat %>% ungroup() %>% 
                                                 filter(intervention == 0) %>% summarise(sd(age, na.rm = TRUE)) %>% as.double(),
                                               Age_int = dat %>% ungroup() %>% 
                                                 filter(intervention == 1) %>% summarise(mean(age, na.rm = TRUE)) %>% as.double(),
                                               SD_int = dat %>% ungroup() %>% 
                                                 filter(intervention == 1) %>% summarise(sd(age, na.rm = TRUE)) %>% as.double()
                                               )), data)
    }
  }
}

setwd(sprintf("~%s/qtn/draft_paper/plots", setpath))
write_excel("adj_estimates.xlsx", data)

# get descriptive statistics ----
data %>% group_by(year, school) %>% filter(rank(N, ties.method="first")==1) %>% clipr::write_clip()

dat <- df
dat <- dat[!(is.na(dat$q1_2) & dat$year == 3 & dat$sch %in% c("MKES", "LWLC", "MHLSS", "LHKSS")),]
dat <- dat[!(is.na(dat$q4neg) & dat$year == 3 & dat$sch %in% c("CWKMSS", "CWSFMSS")),]
dat <- dat[!(is.na(dat$q4pos) & dat$year == 3 & dat$sch %in% c("HLC")),]
dat <- dat[!(is.na(dat$q1_2) & dat$year == 4 & dat$sch %in% c("MKES", "TKP")),]
dat <- dat[!(is.na(dat$q4neg) & dat$year == 4 & dat$sch %in% c("MHLSS", "CWSFMSS", "DMS")),]
dat <- dat[df$year %in% (3:4),]  %>% 
  mutate(dob = as.Date(as.numeric(dob), origin = "1970-01-01"), submitdate = as.Date(as.numeric(submitdate), origin = "1970-01-01")) %>% 
  filter(!is.na(T1) & !is.na(gender) & !is.na(age) & !is.na(intervention)) %>% # not missing any of these variables before next step, otherwise there might be individuals with only pre/post and not both
  group_by(uid) %>% 
  filter(n() == 2) # both pre & post 

dat %>% 
  filter((sch %in% c("MKES", "LWLC", "MHLSS", "LHKSS", "CWKMSS", "CWSFMSS", "HLC") & year == 3)|
                (sch %in% c("MKES", "TKP", "MHLSS", "CWSFMSS", "DMS") & year == 4)) %>% 
  group_by(year, intervention, gender) %>% filter(T1==1) %>% 
  summarise(n = n(), mean =  mean(age, na.rm = TRUE), sd = sd(age, na.rm = TRUE)) %>% View

# plot ----
setwd(sprintf("~%s/qtn/draft_paper/plots/", setpath))
table <- openxlsx::read.xlsx("adj_estimates.xlsx")
yearname <- c('2015-16', '2016-17', '2017-18', '2018-19')
group <- data.frame(intervention = c(0, 1, ""), name = c("Control group", "Intervention group", "Between-group"), abbrev. = c("ctrl", "int", "between"))
for (wave in c(1, 2, 3, 4)){
  for (dep_var in questions[,1]){
    for (grp in c(0, 1, "")){
      if (all(is.na(table[table$year %in% yearname[wave] & table$question %in% questions[questions[, 1] %in% dep_var, 2], ]))){
        next
      }
      print(wave)
      print(dep_var)
      if (grp == 1){
        data <- table[table$year %in% yearname[wave] & table$question %in% questions[questions[, 1] %in% dep_var, 2], c("school", "T1_T1_intervention", "SE_3")]
        names(data)[names(data)=="T1_T1_intervention"] <- "estimate"
        names(data)[names(data)=="SE_3"] <- "se"
      } else if (grp == 0) {
        data <- table[table$year %in% yearname[wave] & table$question %in% questions[questions[, 1] %in% dep_var, 2], c("school", "T1", "SE_1")]
        names(data)[names(data)=="T1"] <- "estimate"
        names(data)[names(data)=="SE_1"] <- "se"
      } else {
        data <- table[table$year %in% yearname[wave] & table$question %in% questions[questions[, 1] %in% dep_var, 2], c("school", "T1_intervention", "SE_2")]
        names(data)[names(data)=="T1_intervention"] <- "estimate"
        names(data)[names(data)=="SE_2"] <- "se"
      }
      percent_CI <- 0.84 # 84% percentile CI (ref: https://rstudio-pubs-static.s3.amazonaws.com/132971_a902bb2b962b407e9e9436559c6f5d36.html)
      data[, "ci"] <- data[, "se"] * qnorm(1-((1-percent_CI)/2)) # z dist is used, but could use t-dist qt(1-((1-percent_CI)/2), n-1)
      data[, "ymax"] <- data[, "estimate"] + data[, "ci"]
      data[, "ymin"] <- data[, "estimate"] - data[, "ci"]
      data <- data[order(data[, "estimate"]),]
      data[,"xvar"] <- factor(data$school,
                              levels=unique(data$school),
                              ordered=TRUE)
      chart <- ggplot(data, aes_string(x = "xvar", y = "estimate", ymax = "ymax", ymin = "ymin")) +
        # geom_hline(yintercept = 0, color = I("gray75"), size = I(0.5)) +
        geom_point(color = "gray75", alpha=1/(nrow(data)^.33), size=I(0.5)) +
        geom_point(data = data, size=I(2)) +
        geom_pointrange(alpha = 1/(nrow(data)^.33)) +
        geom_pointrange(data = data, alpha = 0.25) +
        labs(x = "School", y = "Score", title = paste0(questions[which(questions==dep_var),2], ", ", group[group$intervention == grp, 2], ", ", yearname[wave])) +
        theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(face = "bold", angle=90, vjust=.5),
              axis.ticks.x = element_blank()) +
        scale_x_discrete(labels = data$school) 
      # exists(c("chart"))
      assign(paste0("chart_", grp), chart)
      rm(chart)
    }
    trycatchNA({
    chart <- ggarrange(chart_1 + rremove("x.title"), chart_0 + rremove("x.title"), chart_,
                       heights = 1,
                       # labels = c("Intervention group", "Control group", "Between-group intervention effect"),
                       ncol = 1, nrow = 3)
    setwd(sprintf("~%s/qtn/draft_paper/plots", setpath))
    ggsave(paste0("year", wave, "_", dep_var,"_", after_char(percent_CI, "."),"CI.png"), plot = chart, width = 12, height = 12)
    rm(chart, chart_0, chart_1, chart_)
    })
  }
}

# delta method to compute standard error of sum of two coefficients
sqrt(vcov(fit)['T1', 'T1'] + vcov(fit)['T1:intervention', 'T1:intervention'] + 2* vcov(fit)['T1', 'T1:intervention'])

# between school significance testing ----
yearname <- c('2015-16', '2016-17', '2017-18', '2018-19')
data <- data.frame()
for (wave in c(3:4)){
  for (dep_var in questions[,1]){
    if (all(is.na(df[df$year %in% wave, dep_var]))){
      next
    }
    print(wave)
    print(dep_var)
    sch_done <- c()
    for (sch_i in c("LWLC", "LHKSS", "CWKMSS", "HLC", "MKES", "MHLSS", "CWSFMSS", "DMS", "TKP")){
      for (sch_j in c("LHKSS", "CWKMSS", "HLC", "MKES", "MHLSS", "CWSFMSS", "DMS", "TKP")){
        if (sch_i == sch_j | is.na(sch_i) | is.na(sch_j) | paste(sch_j, sch_i, sep = "_") %in% sch_done){
          next
        }
        dat <- df[df$year %in% wave & df$sch %in% c(sch_i, sch_j),]
        dat <- dat %>% 
          mutate(dob = as.Date(as.numeric(dob), origin = "1970-01-01"), submitdate = as.Date(as.numeric(submitdate), origin = "1970-01-01")) %>% 
          filter(!is.na(!!as.symbol(dep_var)) & !is.na(T1) & !is.na(gender) & !is.na(age)) %>% # not missing any of these variables before next step, otherwise there might be individuals with only pre/post and not both
          group_by(uid) %>% 
          filter(n() == 2) # both pre & post 
        if (length(unique(na.omit(dat$intervention[dat$sch %in% sch_i]))) <2 |
            length(unique(na.omit(dat$intervention[dat$sch %in% sch_j]))) <2){
          next # only schools with a control group
        }
        sch_done <- c(sch_done, paste(sch_i, sch_j, sep = "_"))
        print(paste(sch_i, sch_j, sep = "_"))
        dat$sch <- factor(dat$sch, levels = c(sch_j, sch_i))
        if (length(unique(na.omit(dat$gender))) == 2){
          fit <- trycatchNA(eval_(
            "lmer(", dep_var, "~ 1+age*sch+gender*sch+T1*intervention*sch+ (1 | uid) , 
                      REML = TRUE, data = dat)"))
        } else {
          fit <- trycatchNA(eval_(
            "lmer(", dep_var, "~ 1+age*sch+T1*intervention*sch + (1 | uid) , 
                      REML = TRUE, data = dat)"))
        }
        # between-group
        beta1 <- iferror(summary(fit)$coef[paste0("sch", sch_j, ":T1:intervention"), 1],
                           summary(fit)$coef[paste0("sch", sch_i, ":T1:intervention"), 1])
        p_value1 <- summary(fit)$coef[paste0("sch", sch_i, ":T1:intervention"), 5] # summary is augmented by loading lmerTest package
        
        # control group
        beta2 <- iferror(summary(fit)$coef[paste0("sch", sch_j, ":T1"), 1],
                           summary(fit)$coef[paste0("sch", sch_i, ":T1"), 1])
        p_value2 <- summary(fit)$coef[paste0("sch", sch_i, ":T1"), 5] # summary is augmented by loading lmerTest package
        
        # intervention group
        # beta3 <- iferror(summary(fit)$coef[paste0("sch", sch_j, ":T1"), 1] + summary(fit)$coef[paste0("sch", sch_j, ":T1:intervention"), 1],
        #                    summary(fit)$coef[paste0("sch", sch_i, ":T1"), 1] + summary(fit)$coef[paste0("sch", sch_i, ":T1:intervention"), 1])
        # 
        # vcov_A <- iferror(vcov(fit)[paste0("sch", sch_j, ":T1"), paste0("sch", sch_j, ":T1")],
        #                   vcov(fit)[paste0("sch", sch_i, ":T1"), paste0("sch", sch_i, ":T1")])
        # vcov_B <- iferror(vcov(fit)[paste0("sch", sch_j, ":T1:intervention"), paste0("sch", sch_j, ":T1:intervention")],
        #                   vcov(fit)[paste0("sch", sch_i, ":T1:intervention"), paste0("sch", sch_i, ":T1:intervention")])
        # vcov_AB <- iferror(vcov(fit)[paste0("sch", sch_j, ":T1"), paste0("sch", sch_j, ":T1:intervention")],
        #                   vcov(fit)[paste0("sch", sch_i, ":T1"), paste0("sch", sch_i, ":T1:intervention")])
        # SE <-  sqrt(vcov_A + vcov_B + 2* vcov_AB)
        # t_value3 <- beta3/SE
        # p_value3 <- (1-pnorm(abs(t_value3)))*2 # delta method should use z dist instead of t dist (ref: https://stats.stackexchange.com/questions/333445/degrees-of-freedom-for-t-test-after-delta-method)
        outputs <- iferror(glht(fit, linfct = paste0("sch", sch_j, ":T1", " + ", "sch", sch_j, ":T1:intervention", " = 0"))  %>% summary(), 
                            glht(fit, linfct = paste0("sch", sch_i, ":T1", " + ", "sch", sch_i, ":T1:intervention", " = 0"))  %>% summary())
        beta3 <- outputs$test$coefficients[1]
        p_value3 <- outputs$test$pvalues[1] # alternative method using contrast statements & glht
                          
        sch_list <- c(sch_i, sch_j)
        sch_col <- rownames(summary(fit)$coef)[3] %>% substr(., 4, nchar(.))
        sch_row <- sch_list[!(sch_list %in% sch_col)]
        
        if (sch_row %in% data$school[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                       data$year %in% yearname[wave] &
                       data$group %in% "between-group")]){
          data[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                       data$year %in% yearname[wave] &
                       data$group %in% "between-group" & 
                       data$school %in% sch_row), sch_col] <- starred_p(p_value1, 3, beta1)
        } else {
          data[nrow(data)+1, ] <- NA
          data[nrow(data), 'question'] <- questions[questions[, 1] %in% dep_var, 2]
          data[nrow(data), 'year'] <- yearname[wave]
          data[nrow(data), 'group'] <- "between-group" 
          data[nrow(data), 'school'] <-  sch_row
          data[nrow(data), sch_col] <-  starred_p(p_value1, 3, beta1)
        }
        
        if (sch_row %in% data$school[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                                           data$year %in% yearname[wave] &
                                           data$group %in% "control group")]){
          data[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                       data$year %in% yearname[wave] &
                       data$group %in% "control group" & 
                       data$school %in% sch_row), sch_col] <- starred_p(p_value2, 3, beta2)
        } else {
          data[nrow(data)+1, ] <- NA
          data[nrow(data), 'question'] <- questions[questions[, 1] %in% dep_var, 2]
          data[nrow(data), 'year'] <- yearname[wave]
          data[nrow(data), 'group'] <- "control group" 
          data[nrow(data), 'school'] <-  sch_row
          data[nrow(data), sch_col] <-  starred_p(p_value2, 3, beta2)
        }

        if (sch_row %in% data$school[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                                           data$year %in% yearname[wave] &
                                           data$group %in% "intervention group")]){
          data[which(data$question %in% questions[questions[, 1] %in% dep_var, 2] &
                       data$year %in% yearname[wave] &
                       data$group %in% "intervention group" & 
                       data$school %in% sch_row), sch_col] <- starred_p(p_value3, 3, beta3)
        } else {
          data[nrow(data)+1, ] <- NA
          data[nrow(data), 'question'] <- questions[questions[, 1] %in% dep_var, 2]
          data[nrow(data), 'year'] <- yearname[wave]
          data[nrow(data), 'group'] <- "intervention group" 
          data[nrow(data), 'school'] <-  sch_row
          data[nrow(data), sch_col] <-  starred_p(p_value3, 3, beta3)
        }
      }
    }
  }
}

# bootstrapping ----
boot_func <- function(data, indices){
  require(lme4)
  dep_var = "q3"
  dat <- data[indices, ]
  fit <- lmer(q3~ 1+age+gender + (1 | sch:uid) +  (1+intervention*T1 | sch) , 
              REML = TRUE, data = dat)
  RE <- ranef(fit)
  print(RE$sch)
  return(RE$sch['CWSFMSS', 'intervention:T1'])
}
dat <- df[df$year == 4, ]
system.time(
  lmer_boot <-
    boot::boot(data = dat[which(duplicated(dat$uid,  fromLast = FALSE) | duplicated(dat$uid,  fromLast = TRUE)),],
               statistic = boot_func, parallel = 'snow', ncpus = parallel::detectCores(), R = 100)
)

fit <- lmer(q3~ 1+age+gender + (1 | sch:uid) + (-1+intervention*T1 | sch) , 
            REML = TRUE, data = dat[which(duplicated(dat$uid,  fromLast = FALSE) | duplicated(dat$uid,  fromLast = TRUE)),])
RE <- merTools::REsim(fit, n.sims = 1000)
merTools::plotREsim(RE, facet= list(groupFctr= "sch", term ='intervention:T1'), labs = TRUE) 

# descriptive statistics ----
df$yearname <- car::recode(df$year, "1 = '2015-16'; 2 = '2016-17'; 3 = '2017-18'; 4 = '2018-19' ")

# dt <- data.table::as.data.table(
#   df[which(duplicated(df$uid,  fromLast = FALSE) | duplicated(df$uid,  fromLast = TRUE)),])

dt <- df %>% 
  mutate(dob = as.Date(as.numeric(dob), origin = "1970-01-01"), submitdate = as.Date(as.numeric(submitdate), origin = "1970-01-01")) %>% 
  filter(!is.na(T1) & !is.na(gender) & !is.na(age)) %>% # not missing any of these variables before next step, otherwise there might be individuals with only pre/post and not both
  group_by(uid) %>% 
  filter(n() == 2) %>% # both pre & post 
  data.table::as.data.table()

dt[!is.na(intervention) , 
   .(n = .N,
     age = mean(age),
     sd = sd(age),
     n = length(q1_2[!is.na(q1_2)]),
     'Mental Health Knowledge' = mean(q1_2, na.rm = TRUE),
     n = length(q3[!is.na(q3)]),
     'Psychological Distress (GHQ)' = mean(q3, na.rm = TRUE),
     n = length(q4neg[!is.na(q4neg)]),
     'Negative Thinking'  = mean(q4neg, na.rm = TRUE), 
     n = length(q4pos[!is.na(q4pos)]),
     'Positive Thinking'  = mean(q4pos, na.rm = TRUE), 
     n = length(q5a[!is.na(q5a)]),
     'Life Satisfaction (SLSS)' = mean(q5a, na.rm = TRUE),
     n = length(q5b[!is.na(q5b)]),
     'Life Satisfaction (BMSLSS)' = mean(q5b, na.rm = TRUE),
     n = length(q6[!is.na(q6)]),
     'Empathy: Perspective Taking (C-IRI)' = mean(q6, na.rm = TRUE),
     n = length(q7[!is.na(q7)]),
     'Gratitude (GQ-6)' = mean(q7, na.rm = TRUE)
   ), by = .(
     year = yearname, school = sch,
     intervention, T1, gender)] -> table


