rm(list=ls())
graphics.off()
# par(mar=c(0,0,0,0)) # set plot margins to 0
if (substring(getwd(),2,2) == ":") {
  setpath <- "/MEGAsync/Work/RA HKU/CSRP"
} else {
  setpath <- ""
}
setwd(sprintf("~%s/", setpath))
source("helper_functions.R")

library(pdftools)
library(magrittr)
library(readr)

setwd(sprintf("~%s/misc", setpath))

text <- pdf_text('Haby (2012) BMI prediction 2025 Australia.pdf')
cat(text, file = 'haby.txt')
