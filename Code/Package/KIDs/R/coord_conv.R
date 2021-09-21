# function that converts coordinates from pdf_data to pdf_render_page

#' @export
coord_conv <- function(c_data, bitmap){
  
  # subset (col = y, row = x)
  co_dat <- c_data[, c("x", "y", "text")]
  co_rend <- dim(bitmap)[2:3]

  # convert x
  scx <- max(co_dat[, "x"]) / co_rend[1]
  co_dat[, "x"] <- round(co_dat[, "x"] * scx)
  
  # convert y which is flipped
  scy <- max(co_dat[, "y"]) / co_rend[2]
  co_dat[, "y"] <- round((max(co_dat[, "y"]) - co_dat[, "y"]) * scy)
  
  # return
  return(co_dat)
}

