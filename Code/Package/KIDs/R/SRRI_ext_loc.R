# doc ... path to file 
# col ... HEX code of color shade
# off ... vertical offset when cutting the bitmap right under the predicted 
#         position of the SRRI graph

#' @export
SRRI_ext_loc <- function(doc, col, off = 0.1){
  
  ## DATA ##
  pdf.data  <- scale_cand_coord(doc)
  scale.data <- pdf.data[[1]]
  
  # extract scale
  ext.loc <- KIDs::coord_id(scale.data)
  
  # convert pdf to text
  pdf.text <- strsplit(pdftools::pdf_text(doc), "\n") 
  
  # generate bitmap
  bit.map <- pdftools::pdf_render_page(doc, page = 1, dpi = 50)
  
  # convert coords
  scale.coords <- KIDs::coord_conv(ext.loc, bit.map)
  
  ## COLOR ##
  
  # split HEX
  col.split <- unlist(strsplit(gsub("(.{2})", "\\1 ", 
                                    unlist(strsplit(col, "#"))[[2]]), " "))
  
  # convert to lower case
  col.split <- tolower(col.split) 
  
  ## COORDINATES ##
  
  # Shade
  coo <- which(bit.map[1, , ] == col.split[1] & bit.map[2, , ] == col.split[2] & 
                 bit.map[3, , ] == col.split[3], arr.ind = T)
  
  # stopif no pixels of desired color detected
  if(nrow(coo) < 1) stop("Error: No pixels of given color detected.")
  
  ## CLASSIFICATIONs ##
  
  # hierarchical clustering: k = 5, method = "average"
  
  # get grouping
  grps <- fastcluster::hclust(dist(coo), method = "average") 
  
  # restrict amnt of groups
  grps <- cutree(grps, k = 5)
  
  # bind
  dat.grps <- as.data.frame(cbind(coo, grps))
  
  ## IDENTIFY CLUSTER ##
  
  # discard all grps with less than 10% of all points
  tbl.rel <- table(dat.grps$grps) / length(dat.grps$grps)
  
  # grps with less than 20% of all pixels
  excl.nom <- as.numeric(names(tbl.rel[tbl.rel > 0.2]))
  
  # match and subset
  dat.grps <- dat.grps[dat.grps$grps %in% excl.nom, ]
  
  # horizontal variance scaled (alt decision rule)
  which.min(tapply(dat.grps[, 1], dat.grps[, 3], 
                   function(x) var(x) / length(x))) -> rect.grp
  
  # median
  med.rect.grp <- median(dat.grps[which(dat.grps[, 3] == names(rect.grp)), 1])
  
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