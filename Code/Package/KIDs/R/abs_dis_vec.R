# helper function for coord.id that calculates the pairwise absolute distance of 
# all vector elements

#' @export
abs_dis_vec <- function(x, set.diag = 999){
  
  # pairwise distance
  dis <- abs(outer(x, x, "-"))
  
  # set diag
  diag(dis) <- set.diag
  
  # return
  return(dis)
  
}