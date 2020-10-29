library(pdftools)

setwd("C:\\Users\\kingtam\\Desktop")

# extract a page from each PDF and combine them

for (i in (2002)){
  pdf_subset(paste0(i,'.pdf'),
             pages = 21, output =   paste0(i,'f.pdf')) # 30:50 would extract pages 30 to 50
}

for (i in (2003:2007)){
  pdf_subset(paste0(i,'.pdf'),
             pages = 20, output =   paste0(i,'f.pdf')) # 30:50 would extract pages 30 to 50
}

pdf_combine(sprintf("%s%s", (2002:2007), "f.pdf"), output = "joined0207f.pdf")
