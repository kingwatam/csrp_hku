## Overview: 
## These helper functions contain useful functions:
## to emulate and extend the functionality of some commonly used Stata commands (e.g. tab, sum) 
## to emulate Excel functions (e.g. iferror)
## to simplify the wordy syntax of certain commonly used R functions & combinations thereof (e.g. eval, get)
## 
## Author: King Wa Tam
##
## Notes:
## summ() & tab() work like sum & tab functions in Stata
## summ() e.g. summ(df$var1, df$var2, ...) or summ(df1, df2, ....) or sum(df$var1, df2)
## tab() e.g. tab(df$var1, df$var2, df$var3….)
## round_format(value, decimal_places =  2, check_object = FALSE) 
## starred_p(p_value, decimal_places, related_value = NULL)
## recode_age(var, age_labels = NULL, second_group = 25, interval = 5, last_group = 65) recodes age into age groups - either arbitrary age groups by providing age_lables or by a fixed interval. 
#### Default values are equivalent to using age_labels = c("0-24", "25-29" ,"30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+") 
#### Otherwise, skip the age_labels option by specifying the second group starting age, and fixed interval and the last group starting age, i.e. recode_age(df$age, , 25, 5, 65). 
## import_func() imports only functions from another R script, analagous to the base source() function but ignores non-functions inside the script
## write_excel() writes tables as multiple sheets in an Excel file
## iferror() works like the iferror function in Excel
## ifwarning() works like iferror for warnings instead
## trycatchNA() when a function results in an error, returns NA. trycatch_() is the same except returns a user-defined object
## convert2NA() replaces user-defined values with NAs
## convert2value() replaces certain values with any single value
## after_dollarsign() returns string after dollar sign
## after_char() and before_char() return string after/before a given character
## get_freqtable() returns up to 3-way frequency table as a data frame
## eval_() simplifies eval(parse()) globally, while concatenating strings so that paste/sprintf functions are not needed. Beware of certain problems since it evaluates objects from the global environment
## get_() works like get() in the global environment. Beware of certain problems since it gets objects from the global environment
## summary.lm() lm summary for robust (sandwich) SEs and clustered SEs (up to 2 cluster variables)

sum_func <- function(x, var_name) { # sum_func() is used inside summ()
  argx <- var_name
  if (is.factor(x)){
    x <- as.numeric(x)
  }

  sumx <- suppressWarnings(data.frame("Name"=argx, 
                                      N=length(na.omit(x)), # not including missing values
                                      Mean=iferror(mean(x, na.rm = TRUE), NA), # not including missing values
                                      # Median=median(x, na.rm = TRUE),
                                      SD=iferror(sd(x, na.rm = TRUE), NA),
                                      Min=iferror(min(x, na.rm = TRUE), NA),
                                      Max=iferror(max(x, na.rm = TRUE), NA)
                                      ))
  if (var_name == "X[[i]]"){
    sumx <- sumx[2:length(sumx)]
  }
  return(sumx)
}

summ <- function(...) { # summ(df$var1, df$var2, ...) or summ(df1, df2, ....) or sum(df$var1, df2)
  require(dplyr)
  n_args <- nargs()
  arg <- substitute(list(...))
  arg_values <- list(...)
  if (is.data.frame(arg_values[[1]])){
    summary <- suppressWarnings(t(sapply(arg_values[[1]], summ)))
    summary <- as.data.frame(summary)
    summary$Name <- rownames(summary)
    summary <- summary %>%  select("Name", everything())
    } else {
    summary <- sum_func(arg_values[[1]], deparse(arg[[2]]))
  }
  if (n_args>1) {
    for (i in 2:n_args) {
      x <- i+1
      if (is.data.frame(arg_values[[i]])){
        sum_i <- suppressWarnings(t(sapply(arg_values[[i]], summ)))
        sum_i <- as.data.frame(sum_i)
        sum_i$Name <- rownames(sum_i)
        sum_i <- sum_i %>% as.data.frame() %>% select("Name", everything())
        summary <- rbind(summary, sum_i)
      }
      else {
        sum_i <- sum_func(arg_values[[i]], deparse(arg[[x]]))
        summary <- rbind(summary, sum_i)
      }
    }
  }
  max_print <- ifelse(dim(summary)[1]*dim(summary)[2] < 1000, 
                      1000, as.character(dim(summary)[1]*dim(summary)[2]))
  options(max.print = max_print)
  rownames(summary) <- 1:nrow(summary)
  return(summary) 
} 

tab <- function(...){
  n_args <- nargs()
  arg <- substitute(list(...))
  arg <- arg[2:length(arg)]
  arg_values <- list(...)
  # d <- data.frame(.x, .y, .z)
  if (n_args <= 1){
    Freq <- rep("N",length(arg_values[[1]]))
    newTab <- eval_(sprintf("xtabs(~ %s + Freq, addNA = TRUE)", paste(arg, collapse = " + ")))
    names(dimnames(newTab))[1] <- after_dollarsign(arg[1])
    names(dimnames(newTab))[2] <- "" # this must take place before ftable() otherwise dimnames are NULL
    newTab <- ftable(addmargins(newTab, 1, FUN = list(Total=sum), quiet = TRUE)) 
    return(newTab)
  }
  newTab <- eval_(sprintf("xtabs(~ %s, addNA = TRUE)", paste(arg, collapse = " + ")))
  for (i in (1:n_args)){
    if (length(unique(before_char(arg, '$'))) == 1){ # only remove dataframe names when they're identical
      names(dimnames(newTab))[i] <- after_dollarsign(arg[i])
    }
  }
  newTab <- ftable(addmargins(newTab, FUN = list(Total=sum), quiet = TRUE)) # flatten table and add margins/totals
  return(newTab)
}

round_format <- function(value, decimal_places = 2, check_object = FALSE){
  if (check_object == TRUE){
    if (!exists(deparse(substitute(value)), parent.frame())){
      return(NA)
    }
  }
  if (is.vector(value)){
    for (i in (1:length(value))){
      if (is.na(value[i])){
        value[i] <- NA
      } else {
        value[i] <- format(round(as.numeric(value[i]), decimal_places), nsmall = decimal_places)
      }
    }
    return(value)
  } else if (is.na(value)){
    return(NA)
  }
  return(format(round(value, decimal_places), nsmall = decimal_places))
} 

starred_p <- function(p_value, decimal_places, related_value = NULL){
  if (is.null(related_value)){
    value <- p_value
  } else {
    value <- related_value
  }
  p <- as.numeric(p_value)
  if (is.na(p)){
    return(NA)
  }
  if(p < 0.001){
    return(paste0(round_format(value, decimal_places),"***"))
  }
  else if(p >= 0.001 & p < 0.01){
    return(paste0(round_format(value, decimal_places),"**"))
  }
  else if(p >= 0.01 & p < 0.05){
    return(paste0(round_format(value, decimal_places),"*"))
  }
  else{
    return(round_format(value, decimal_places))
  }
}

recode_age <- function(var, age_labels = NULL, second_group = 25, interval = 5, last_group = 65){ # either input age_labels directly or input starting value for second age group, the interval, and starting value for the last group
  if (is.null(age_labels)){
    starting_group <- paste0('0-', second_group - 1)
    
    age_labels <- c(starting_group, paste(seq(second_group, last_group-interval, by = interval), seq(second_group + interval - 1, last_group - 1, by = interval),
                                          sep = "-"), paste(last_group, "+", sep = ""))
    
    return(
      cut(as.integer(var), breaks = c(0, seq(second_group, last_group, by = interval), Inf), labels = age_labels, right = FALSE)
    )
  } else {
    return(
      cut(as.integer(var), breaks = c(as.vector(as.numeric(before_char(age_labels, "-|+"))), Inf), labels = age_labels, right = FALSE)
    )
  }
}

import_func <- function(R_file){
  expressions <- parse(R_file)
  functions_only <- sapply(expressions, function(x) {
    if(x[[1]]=="<-") {
      if(x[[3]][[1]]=="function") {
        return(TRUE)
      }
    }
    return(FALSE)
  })
  eval(expressions[functions_only], parent.frame())
}

write_excel <- function(filename = "sheet.xlsx", ..., remove_char =  NULL){ 
  require(xlsx)
  require(rJava)
  require(XLConnect)
  wb <- xlsx::createWorkbook("xlsx")
  if (is.null(remove_char)){
    n_args <- nargs()-1 # subtract first arg
  } else {
    n_args <- nargs()-2 # subtract first and last args
  }
  arg <- substitute(list(...))
  arg_values <- list(...)
  for (i in 1:n_args) {
    # sheetname <- gsub("([._-])|[[:punct:]]", "\\1", deparse(arg[[i+1]])) # whatever is inside () is kept when "\\1" used  
    if (n_args <= 1) {
      sheetname <- sub("\\..*", "", filename) # keep string before "."
    } else {
      if (is.null(remove_char)){
        sheetname <- gsub("[^[:alnum:][:blank:]+|~`!@#$%^&()_-{};,<.>]", "", deparse(arg[[i+1]])) # keep a few punctuation marks
      } else{
        sheetname <- gsub(remove_char, "", deparse(arg[[i+1]]))
        sheetname <- gsub("[^[:alnum:][:blank:]+|~`!@#$%^&()_-{};,<.>]", "", sheetname) # keep a few punctuation marks
      }
    }
    sheet <- xlsx::createSheet(wb, sheetName = sheetname)  # certain punctuation marks are removed due to Excel worksheet names not allowing them
    xlsx::addDataFrame(as.data.frame(arg_values[[i]]), sheet,
                       startRow=1, startColumn=1,
                       row.names = FALSE, showNA = FALSE)
  }
  xlsx::saveWorkbook(wb, filename, password=NULL)
}

iferror <- function(expr, error_expr){
  tryCatch(expr, 
           error=function(e){error_expr}
  )
}

ifwarning <- function(expr, warning_expr){
  tryCatch(expr, 
           warning=function(w){warning_expr})
}

trycatchNA <- function(func){
  tryCatch(func, error=function(err) NA)
}

trycatch_ <- function(func, x){
  tryCatch(func, error=function(err) x)
}

convert2NA <- function(df, values){
  for (c in (1:ncol(df))){
    df[df[,c] %in% values, c] <- NA
  }
  return(df)
  ## Quite slow when not vectorise
  # for (r in (1:nrow(df))){ 
  #   for (c in (1:length(colnames(df)))){
  #     if (df[r,c] %in% values){
  #       df[r,c] <- NA
  #     }
  #   }
  # }
  # return(df)
}

convert2value <- function(df, values, value){
  for (c in (1:ncol(df))){
    was_factor <- FALSE
    if (is.factor(df[,c])){ # change from factor to character
      was_factor <- TRUE
      df[,c] <- as.character(df[,c])
    }
    df[df[,c] %in% values, c] <- value
    if (was_factor){ 
      df[,c] <- as.factor(df[,c])
    }
  }
  return(df)
}

after_dollarsign <- function(x) {
  return(substring(x, regexpr("[$]", x)+1, nchar(x)))
}

after_char <- function(x, char) {
  return(substring(x, regexpr(paste0("[", char,"]"), x)+1, nchar(x)))
}

before_char <- function(x, char) {
  return(substring(x, 1, regexpr(paste0("[", char,"]"), x)-1))
}

get_freqtable <- function(x, y=NULL, z=NULL){
  argx <- after_dollarsign(deparse(substitute(x)))
  if (is.null(z) & is.null(y)){
    dat <- as.data.frame(table(x, useNA = 'ifany'))
    names(dat)[names(dat) == "x"] <- argx
  } else if (is.null(z)) {
    argy <- after_dollarsign(deparse(substitute(y)))
    dat <- as.data.frame(table(x, y, useNA = 'ifany'))
    # reshape table from long to wide
    dat <- reshape2::dcast(dat, x ~ y, value.var = "Freq", fun.aggregate = NULL)
    names(dat)[names(dat) == "x"] <- paste0(argx, '\\', argy)
  } else {
    argy <- after_dollarsign(deparse(substitute(y)))
    argz <- after_dollarsign(deparse(substitute(z)))
    dat <- as.data.frame(table(x, y, z, useNA = 'ifany'))
    names(dat)[names(dat) == "x"] <- argx
    names(dat)[names(dat) == "y"] <- argy
    names(dat)[names(dat) == "z"] <- argz
    # reshape table from long to wide
    dat <- reshape2::dcast(dat, x + y ~ z, value.var = "Freq", fun.aggregate = length)
    names(dat)[names(dat) == "x"] <- argx
    names(dat)[names(dat) == "y"] <- paste0(argy, '\\', argz)
  }
  dat <- convert2value(dat, NA, "NA")
  # dat <- rbind(dat[-which(x=="NA"),], dat[which(x=="NA"),])
  return(dat)
}

eval_<- function(...){ # evaluate text & variables as expression 
  n_args <- nargs()
  arg_values <- list(...)
  myvector <- c()
  if (n_args>0) {
    for (i in 1:n_args) {
      myvector[i] <- arg_values[[i]]
    }
  }
  return(eval(parse(text = paste0(myvector, collapse = "")), parent.frame())) # evaluating in parent environment
}

get_<- function(...){ # evaluate text as expression (faster than eval(parse()) by a factor of 2-7!)
  n_args <- nargs()
  arg_values <- list(...)
  myvector <- c()
  if (n_args>0) {
    for (i in 1:n_args) {
      myvector[i] <- arg_values[[i]]
    }
  }
  return(get(paste0(myvector, collapse = ""), parent.frame() )) # evaluating in parent.frame()
}






# robust SEs for lm()
# import the function from repository
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
text_file <- httr::content(httr::GET(url_robust), "text")
eval(parse(text = text_file),
     envir=.GlobalEnv)
rm(url_robust, text_file)
