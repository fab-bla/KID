# Debug IQAM utilizing a example pdf
source("C:/Users/blasc/OneDrive/Documents/GitHub/KID/Code/Functions/SRRI_ext_fast.R")

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

# examine



# first filter for 1 to 7 
pdf.dat <- within(pdf.dat, 
                  ind <- nchar(gsub("\\s", "", text)) == 1 & grepl("[1-7]", text))

# location
loc <- with(pdf.dat, pdf.dat[ind, c("x", "y", "text")])

# function that identifies the coordinates of the scale.

# helper function that find the vertical distance of all elements within a vector
abs.dis.vec <- function(x, set = 999){
  # pairwise distance
  dis <- abs(outer(x, x, "-"))
  
  # set diag to NA
  diag(dis) <- set
  
  # return
  return(dis)
}

abs.dis.vec(rnorm(10))
# input should be a n x 3 

coord.id <- function(loc){
  
  # all abs diff of horizontal and vertical coords
  lapply(loc[, c("x", "y")], function(x){
                                          # dis
                                          tmp <- abs.dis.vec(x)
                                          
                                          # name and text
                                          rownames(tmp) <- loc$text
                                          
                                          # return 
                                          tmp
                                          }) -> dis
  
  
  # identify whether the scale is vertical or horizontal
  
  lapply(dis, function(x){
    
    # find all rows which absolute distance is less than three pixels
    apply(x, 1, function(y){
      sum(y <= 3) >= 5
    }) 
  }) -> ind.int
  
  # vert or horz
  ind.fin <- ind.int[[which.max(sapply(ind.int, sum))]]
  
  # subset
  as.data.frame(loc[ind.fin, ])
}

# run exmpl
coord.id(loc)

## ToDo ##
  # insert conditionals to catch groups of numbers between 1 and 7 that do not rep the scale
  # catch cases where scale is incomplete
  # catch cases where no scale was detected
  # write coord translation function to switch between pdf_data and pdf_render_page aka bitmaps
  # write function that creates the input for coord.id from pdf_data


