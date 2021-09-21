# function that pulls all potential candidates for scale identification of SRRI
# given a KID PDF file

#' @export
scale_cand_coord <- function(file, page = 1){
  
  # import
  pdf.dat <- pdftools::pdf_data(file)[[page]]
  
  # filter for 1 to 7 
  pdf.dat <- within(pdf.dat, 
                    ind <- nchar(gsub("\\s", "", text)) == 1 & grepl("[1-7]", text))
  
  # location
  loc <- with(pdf.dat, pdf.dat[ind, c("x", "y", "text")])
  
  # return 
  return(list(loc, pdf.dat))
  
}

