# given a n x 3 matrix of x, y coordinates and the corresponding text this 
# finds the coordinates of the scale of the SRRI no matter if it is horizontal 
# or vertical 

# loc is the output obtained from scale_cand_coord

# tol ... is the tolerance that is used to match the vertical/horizontal components 
# of the scale. The default tolerance is set to three pixels

#' @export
coord_id <- function(loc, tol = 3){
  
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
      sum(y <= tol) == 6
    }) 
    
  }) -> ind.int
  
  # vert or horz
  vh <- which.max(sapply(ind.int, sum))
  ind.fin <- ind.int[[vh]]
  
  # det vert or horz
  if(vh == 1){dir <- "v"} 
    else{dir <- "h"}
        
  # subset
  scale <- as.data.frame(loc[ind.fin, ])
  
  # return
  list("Scale" = scale,
       "Alignment" = dir)
}
