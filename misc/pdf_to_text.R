rm(list=ls())
graphics.off()
# par(mar=c(0,0,0,0)) # set plot margins to 0
setpath <- "/MEGAsync/Work/HKU/CSRP"
setwd(sprintf("~%s", setpath))
source("helper_functions.R")

library(pdftools)
library(magrittr)
library(readr)

setwd(sprintf("~%s/misc", setpath))

text <- pdf_text('example.pdf')
cat(text, file = 'example.txt')
