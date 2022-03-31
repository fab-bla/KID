# extracts the background color of a pdf files

# doc...input .pdf file
# dpi...desired input res

#' @export
bg_col <- function(doc, dpi = dpi, page = page, pass = FALSE){

  # if is assumed to be white without extraction
  if(pass) "#ffffffff"

  else{

    # input
    btmp <- pdftools::pdf_render_page(doc, dpi = dpi, page = page)

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

