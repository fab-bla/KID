# load package 
source("C:/Users/blasc/OneDrive/Documents/GitHub/KID/Code/Package/KIDs")
devtools::load_all()


# locate scale uising pdf_data
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/KIDS/Allianz")
pd <- list.files(pattern = ".pdf")[2]

# algo that identifies whether there are 
tt <- pdftools::pdf_render_page(pd, dpi = 67.31)

# calc multiple and then use that for tranlation


# get data
pdf.dat <-pdftools::pdf_data(pd)[[1]]

#  page size to determin dpi
ps <- as.numeric(sapply(pdf.dat, max)[c(3, 4)])
ps[1] / ps[2]

## ToDo ##
  # insert conditionals to catch groups of numbers between 1 and 7 that do not rep the scale
  # catch cases where scale is incomplete
  # catch cases where no scale was detected
  # write coord translation function to switch between pdf_data and pdf_render_page aka bitmaps



