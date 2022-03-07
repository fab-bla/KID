# as "%in%" does not preserve dimensions we have to write a helper function
# that allows us to use which and obtain the desired array index

#' @export
match_keep_dim <- function(x, y){

  # match
  tmp <- x %in% y

  # reassign dim
  dim(tmp) <- dim(x)

  # return
  return(tmp)

}

# operator
`%IN%` <- function(x,  y){

  # match
  tmp <- x %in% y

  # reassign dim
  dim(tmp) <- dim(x)

  # return
  return(tmp)

}
