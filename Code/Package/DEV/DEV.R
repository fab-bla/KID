# load package 
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/Code/Package/KIDs")
devtools::load_all()


# locate scale uising pdf_data
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/KIDS/Allianz")
pd <- list.files(pattern = ".pdf")[2]

# function that translates coordinates
debugonce(SRRI_ext_loc)
SRRI_ext_loc(pd, "#A6A6A6")


# debug coord cobnv
debugonce(coord_conv)
coord_conv(ext.loc, bit.map)

## Notes ##
  # pdf_data yields different coords because it does not contain the page margin
  # get the native dpi of the pdf by comparing to the scale obtained via the margin

pdftools::pdf_info(pd)

## ToDo ##
  # insert conditionals to catch groups of numbers between 1 and 7 that do not rep the scale
  # catch cases where scale is incomplete
  # catch cases where no scale was detected
  # write coord translation function to switch between pdf_data and pdf_render_page aka bitmaps


