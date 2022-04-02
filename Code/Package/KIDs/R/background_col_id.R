# extracts the background color of a pdf files

# doc...input .pdf file
# dpi...desired input res
# pass...assuming the bg-color is white this skips the extraction process

#' @export
bg_col <- function(doc, dpi = dpi, page = page, pass = FALSE){

  # input bitmap
  btmp <- pdftools::pdf_render_page(doc, dpi = dpi, page = page)


  # if the bg-color is assumed to be white without extraction
  if(pass){

    # return white as bg and bitmap
    list("bitmap" = btmp,
         "bgCol" = c("ff", "ff", "ff", "ff"))

  }

  else{

    # flatten array to matrix
    colm <- apply(btmp, 2:3, \(x) paste(x, collapse = ""))

    # identify background color
    colm <- names(which.max(table(colm)))

    # split
    col_split <- gsub("(.{2})", "\\1 ", colm) |> strsplit("\\s+") |> unlist()

    # return most common color and bitmap
    list("bitmap" = btmp,
         "bgCol" = col_split)

  }

}

