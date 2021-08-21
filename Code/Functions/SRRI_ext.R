# doc ... path to file 
# col ... HEX code of color shade
# off ... vertical offset when cutting the bitmap right under the predicted 
#         position of the SRRI graph

SRRI_ext <- function(doc, col, off = 0.05){
  
  ## FIND PAGE ##
  
  # convert pdf to text and identify line of interest
  pdf.text <- strsplit(pdftools::pdf_text(doc), "\n") 
  
  # obtain relative line on page
  sapply(pdf.text, function(y){
    # id
    tmp1 <- grep("Risiko- und Ertragsprofil", y) / length(y) 
    
    # return
    if(length(tmp1) == 0){
      return(NA)
    } else {
      return(tmp1)
    }
  }) -> pos.vec
  
  # ERROR if both pages yield a value different to NA 
  if(sum(!is.na(pos.vec)) > 1) stop("Error: Could not uniquely identify position of SRRI.")
  
  # ERROR if header was not detected
  if(sum(!is.na(pos.vec)) == 0) stop("Error: Could not detect SRRI.")
  
  ##  BITMAP  ##
  
  # identify Page
  page.SRRI <- which(!is.na(pos.vec))
  
  # generate bitmap
  bit.map <- pdftools::pdf_render_page(doc, page = page.SRRI, dpi = 50)
  
  # subset array
  ind.page.len <- round(dim(bit.map)[3] * (pos.vec[page.SRRI] - off))
  
  # return
  bit.map.sub <- bit.map[ , , -c(ind.page.len:1)]
  
  ## COLOR ##
  
  # split HEX
  col.split <- unlist(strsplit(gsub("(.{2})", "\\1 ", 
                                    unlist(strsplit(col, "#"))[[2]]), " "))
  
  # convert to lower case
  col.split <- tolower(col.split) 
  
  ## COORDINATES ##
  
  # Shade
  coo <- which(bit.map.sub[1,,] == col.split[1] & bit.map.sub[2,,] == col.split[2] & 
                 bit.map.sub[3,,] == col.split[3], arr.ind = T)
  
  # stopif no pixels of desired color detected
  if(nrow(coo) < 1) stop("Error: No pixels of given color detected.")
  
  # margin
  coob <- which(bit.map[1,,] == "00" & bit.map[2,,] == "00" & bit.map[3,,] == "00", 
                arr.ind = T)
  
  # lsm / rsm
  lsm <- min(coob[, 1])
  rsm <- max(coob[, 1])
  
  # scale
  int_leng <- (rsm - lsm) / 7
  
  # midpoints
  scale <- setNames(cumsum(c(lsm + int_leng / 2, rep(int_leng, 6))), 1:7)
  
  ## CLASSIFICATIOn ##
  
  # p = 5, method = "average"
  
  # get grouping
  grps <- agnes(coo, method = "average", diss = F)
  
  # restrict amnt of groups
  grps <- cutree(grps, k = 5)
  
  # bind
  dat.grps <- as.data.frame(cbind(coo, grps))
  
  ## IDENTIFY CLUSTER ##
  
  # identify cluster with minimum sum of variance
  # which.min(rowSums(aggregate(dat.grps[, 1:2], by = dat.grps[, 3, drop = F], 
  #                           var))) -> rect.grp
  
  # horizontal variance scaled (alt decision rule)
  which.min(tapply(dat.grps[, 1], dat.grps[, 3], 
                   function(x) var(x) / length(x))) -> rect.grp
  
  # median
  med.rect.grp <- median(dat.grps[, 1][which(dat.grps[, 3] == rect.grp)])
  
  # return minimum absolute difference
  dif <- abs(med.rect.grp - scale)
  
  # which 
  SRRI <- which.min(dif)
  
  # return
  list(dif,
       SRRI,
       dat.grps,
       med.rect.grp,
       scale)
}

