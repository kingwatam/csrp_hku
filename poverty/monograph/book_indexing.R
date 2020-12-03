rm(list=ls())
graphics.off()
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(dplyr)
library(textreadr) # read_docx
library(quanteda) # tokens, tokens_remove
library(tidytext) # stop_words dataset
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

data("stop_words")

new_stop_words <- c("Hong", "Kong", "Kong's", "HK",
                    "fig", "figures", "figure", "tables", "table",
                    "Yip", "al", "", " ", "final.pdf" , "e.g.", "i.e.")

words  <- tokens_remove(words, c(stop_words$word, new_stop_words)) # remove stop words

get_ngram <- function(tokens, n){
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


write_excel("monograph_ngram.xlsx", ngram1, ngram2, ngram3)

