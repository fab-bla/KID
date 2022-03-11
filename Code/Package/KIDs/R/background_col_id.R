# extracts the background color of a pdf files

# doc...input .pdf file
# dpi...desired input res

#' @export
bg_col <- function(doc, dpi = 71.5){
  
  # input
  btmp <- pdftools::pdf_render_page(doc, dpi = dpi)
  
  # flatten array to matrix 
  colm <- apply(btmp, 2:3, \(x) paste(x, collapse = ""))
  
  # return most common color 
  names(which.max(table(colm)))
  
}

