rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(textreadr) # read_docx
library(quanteda) # tokens, tokens_remove
library(tidytext) # stop_words dataset
library(stringr)
# library(stopwords)

setwd(sprintf("~%s/poverty/monograph", setpath))

# df <- read_docx("monograph_25.11-pyip.docx")
# saveRDS(df, 'monograph_25.11-pyip.RDS')

df <- readRDS("monograph_25.11-pyip.RDS")

words <- quanteda::tokens(df,
                           what="word",
                           remove_numbers=TRUE,
                           remove_punct=TRUE,
                           remove_symbols=TRUE,
                           remove_separators=TRUE,
                           remove_url=TRUE) 

get_ngram <- function(tokens, n, remove_stop_words = TRUE, capitalized_only = FALSE){
  if (capitalized_only){
    df <- tokens # input must be df instead of tokens
    df <- stringr::str_extract_all(df, "[A-Z][a-z]+[ ]*")
    tokens <- quanteda::tokens(df,
                              what="word",
                              remove_numbers=TRUE,
                              remove_punct=TRUE,
                              remove_symbols=TRUE,
                              remove_separators=TRUE,
                              remove_url=TRUE) 
  }
  
  new_stop_words <- c("Hong", "Kong", "Kong's", "HK", "Hong Kong",
                      "fig", "figures", "figure", "tables", "table",
                      "Yip", "al", "", " ", "final.pdf" , "e.g.", "i.e.")
  if (remove_stop_words){
    data("stop_words")
    tokens  <- tokens_remove(tokens, c(stop_words$word, new_stop_words)) # remove stop words
  } else {
    tokens  <- tokens_remove(tokens, c(new_stop_words)) # remove stop words
  }
  
  words <- tokens_ngrams(tokens, n = n)
  words <- paste(words)
  words <- tokens(words,
                   what="word")
  words <- data.frame(paste(words))
  
  names(words)[names(words) == "paste.words."] <- "tokens"
  words$tokens <- gsub("_", " ", words$tokens) 
  words$tokens <- gsub("-", "", words$tokens) 
  words <- words %>% group_by(tokens) %>% count()
  
  if (n > 1){
    new_stop_words <- c(" ")
    words <- words[-which(tolower(words$tokens) %in% tolower(new_stop_words)),] # remove extra stop words
  }
  words <- words[!grepl("^-?#?[[:digit:]]",words$tokens),]
  
  names(words)[names(words) == "n"] <- "frequency"
  words <- words %>% arrange(desc(frequency)) 
  return(words)
}

ngram1 <- get_ngram(words, n = 1)
ngram2 <- get_ngram(words, n = 2)
ngram3 <- get_ngram(words, n = 3)
ngram3_ <- get_ngram(words, n = 3, remove_stop_words = FALSE)
# ngram4 <- get_ngram(words, n = 4)
# ngram4_ <- get_ngram(words, n = 4, remove_stop_words = FALSE)
# ngram5 <- get_ngram(words, n = 5)
# ngram5_ <- get_ngram(words, n = 5, remove_stop_words = FALSE)

ngram1cap <- get_ngram(df, n = 1, capitalized_only = TRUE) # input must be df instead of tokens

write_excel("monograph_ngram.xlsx", 
            ngram1, ngram1cap, ngram2, ngram3, ngram3_)
