# doc ... path to file 
# dpi ... resolution of the input
# page ... page location of the SRRi shade
# tolh ... horizontal tolerance when extracting the rectangles from the SRRI scale
# tolv ... vertical tolerance when extracting the rectangles from the SRRI scale

# SRRI extraction based on the relative amount of non-back ground colored pixels
# in close proximity to the respective SRRI 

#' @export
SRRI_ext_rec <- function(doc, dpi = 71.5, page = 1, tolh = 35, tolv = 10.5){
  
  ## load ##
  
  # bitmap
  bitmap <- pdftools::pdf_render_page(doc, page = page, dpi = dpi)
  
  # scale cand
  pdf.data  <- KIDs::scale_cand_coord(pd)
  
  # extract scale 
  scale <- KIDs::coord_id(pdf.data[[1]])$Scale
  
  ## calculate rel. amount of non white pixels in proximity of all scale entries ##
  
  # build index to cut out respective rectangles around each scale point
  Map(function(x, y){
    
    # build ranges
    rbind("lower" = floor(x - y),
          "upper" = ceiling(x + y)) |> as.data.frame()
    
    
  }, scale[, c("x", "y")], c(tolh, tolv)) |> setNames(c("Horizontal", "Vertical")) -> Sub_ind
  
  # subset and calculate rel. amount of non-white pixels
  mapply(function(x, y){
    
    # subset bitmap
    bitmap[ , x[1]:x[2], y[1]:y[2]] -> sub.bit
    
    # calculate percentage of points that are not the background color
    (sub.bit[1, , ] %in% "ff" &
     sub.bit[2, , ] %in% "ff" &
     sub.bit[3, , ] %in% "ff" &
     sub.bit[1, , ] %in% "ff") -> logi
    
    # rel. amount of non white pixels
    1 - mean(logi)
    
  }, Sub_ind[[1]], Sub_ind[[2]]) -> p_share

  # return predicted SRRI
  unname(which.max(p_share))
  
}

