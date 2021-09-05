# doc ... path to file 
# col ... Vector of RGB values
# off ... vertical offset when cutting the bitmap right under the predicted 
#         position of the SRRI graph

SRRI_ext_test <- function(doc, col, off = 0.05, maxColorValue = 255){
  
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
  bit.map <- pdftools::pdf_render_page(doc, page = page.SRRI, dpi = 50, numeric = TRUE) # change back to assign to bit.map
  
  # subset array
  ind.page.len <- round(dim(bit.map)[1] * (pos.vec[page.SRRI] - off))

  # return
  bit.map.sub <- bit.map[-c(ind.page.len:1) , , ]

  ## COORDINATES ##
  
  # adjust color input
  col <- col / maxColorValue
  
  # Shade
  coo <- which(bit.map.sub[, , 1] == col[1] & bit.map.sub[, , 2] == col[2] & 
               bit.map.sub[, , 3] == col[3], arr.ind = T)
  
  # stopif no pixels of desired color detected
  if(nrow(coo) < 1) stop("Error: No pixels of given color detected.")
  
  # margin
  coob <- which((bit.map[, , 1] == 0 & bit.map[, , 2] == 0 & bit.map[, , 3] == 0), 
                arr.ind = T)  # change back to bit.map
  
  # lsm / rsm
  lsm <- min(coob[, 1])
  rsm <- max(coob[, 1])
  
  # scale
  int_leng <- (rsm - lsm) / 7
  
  # midpoints
  scale <- setNames(cumsum(c(lsm + int_leng / 2, rep(int_leng, 6))), 1:7)
  
  ## CLASSIFICATIONs ##
  
  # hierarchical clustering: k = 5, method = "average"
  
  # get grouping
  grps <- agnes(coo, method = "average", diss = F)
  
  # restrict amnt of groups
  grps <- cutree(grps, k = 5)
  
  # bind
  dat.grps <- as.data.frame(cbind(coo, grps))
  
  ## IDENTIFY CLUSTER ##
  
  # discard all grps with less than 10% of all points
  tbl.rel <- table(dat.grps$grps) / length(dat.grps$grps)
  
  # grps with less than 10% of all pixels
  excl.nom <- as.numeric(names(tbl.rel[tbl.rel > 0.2]))
  
  # match ad subset
  dat.grps <- dat.grps[dat.grps$grps %in% excl.nom, ]
  
  # horizontal variance scaled (alt decision rule)
  which.min(tapply(dat.grps[, 1], dat.grps[, 3], 
                   function(x) var(x) / length(x))) -> rect.grp
  
  # median
  med.rect.grp <- median(dat.grps[which(dat.grps[, 3] == names(rect.grp)), 2])
  
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
  
  # list(pos.vec,
  #      pdf.text,
  #      bit.map,
  #      dif,
  #      SRRI,
  #      dat.grps,
  #      med.rect.grp,
  #      scale)
}