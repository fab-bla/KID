# given a n x 3 matrix of x, y coordinates and the corresponding text this 
# finds the coordinates of the scale of the SRRI no matter if it is horizontal 
# or vertical 

#' @export
coord_id <- function(loc){
  
  # all abs diff of horizontal and vertical coords
  lapply(loc[, c("x", "y")], function(x){
    # dis
    tmp <- KIDs::abs_dis_vec(x)
    
    # name and text
    rownames(tmp) <- loc$text
    
    # return 
    tmp
  }) -> dis
  
  
  # identify whether the scale is vertical or horizontal
  lapply(dis, function(x){
    
    # find all rows which absolute distance is less than three pixels
    apply(x, 1, function(y){
      sum(y <= 3) == 6
    }) 
    
  }) -> ind.int
  
  # vert or horz
  ind.fin <- ind.int[[which.max(sapply(ind.int, sum))]]
  
  # subset
  as.data.frame(loc[ind.fin, ])
  
}