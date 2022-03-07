# load package 
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/Code/Package/KIDs")
devtools::load_all()

# locate scale uising pdf_data
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/KIDS/Erste")
pd <- list.files(pattern = ".pdf")[1]

# function that translates coordinates
#debugonce(SRRI_ext_loc)
SRRI_ext_loc(pd, "#A6A6A6")

# debug coord cobnv
#debugonce(coord_conv)
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

## find native dpi by plotting ##

# prep for loc file
loc <- scale_cand_coord(pd)[[1]]
coord_id(loc)

# id coord of scale
scale <- coord_id(loc)

# compare to scale obtained from page margins
tmp[[5]]

# plot
tmp[[3]]

# plot
# get bitmap
bit.map <- pdftools::pdf_render_page(pd, page = 1, dpi = 71.4)

# extr shading
coo <- which(bit.map[1, , ] == "a6" & "a6" == "a6" & 
               bit.map[3, , ] == "a6", arr.ind = T)

# plot
plot(coo[, 1], coo[, 2])
points(x = scale[[1]][, 1], y = scale[[1]][, 2], col = "red", pch = 19)
abline(h = c(scale[[1]][, 2] - 50, scale[[1]][, 2] + 50), col = "red")


# Debug Raiffeisen

# col
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/KIDS/Auxiliary")
Col_Hex <- readRDS(file = "list_col_KAG.rds")

# setwd
setwd("C:/Users/blasc/OneDrive/Documents/GitHub/KID/KIDS/Union")
pd <- list.files(pattern = ".pdf")[6]


# debug
debugonce(SRRI_ext_loc)
SRRI_ext_loc(pd, col = Col_Hex[["Union"]], tol = 70)

# handle scanned pdfs
# example Union

