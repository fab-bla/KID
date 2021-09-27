# doc ... path to file 
# col ... HEX code of color shade
# off ... vertical offset when cutting the bitmap right under the predicted 
#         position of the SRRI graph

#' @export
SRRI_ext_loc <- function(doc, col, dpi = 71.5, tol = 50){
  
  ## DATA ##
  pdf.data  <- KIDs::scale_cand_coord(doc)
  scale.data <- pdf.data[[1]]
  
  # extract scale
  ext.loc <- KIDs::coord_id(scale.data)
  
  # horz or vert
  if(ext.loc[[2]] == "h"){dir <- 1} else{dir <- 2}

  # location vertically / horizontally
  cut.off.point <- which.min(sapply(ext.loc[[1]][, c(1, 2)], var))
  
  # generate bitmap
  bit.map <- pdftools::pdf_render_page(doc, page = 1, dpi = dpi)
  
  # midpoint of scale 
  mid.point <- median(ext.loc[[1]][, cut.off.point])
  
  # subset bitmap using scale
  bit.map <- bit.map[ , , (mid.point - tol) : (mid.point + tol)]
  
  ## COLOR ##
  lapply(col, function(x){
    
    # split HEX
    col.split <- unlist(strsplit(gsub("(.{2})", "\\1 ", 
                                      unlist(strsplit(x, "#"))[[2]]), " "))
    
    # convert to lower case
    col.split <- tolower(col.split) 
    
    # return
    col.split
    
  }) -> prep.col.list
 
  # transpose list 
  prep.col.list.t <- data.table::transpose(prep.col.list)
  
  ## COORDINATES ##
  
  # Shade
  coo <- which(KIDs::match_keep_dim(bit.map[1, , ], prep.col.list.t[[1]]) & 
               KIDs::match_keep_dim(bit.map[2, , ], prep.col.list.t[[2]]) & 
               KIDs::match_keep_dim(bit.map[3, , ], prep.col.list.t[[3]]), 
               arr.ind = TRUE)

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
  dif <- abs(med.rect.grp - ext.loc[[1]][, dir])
  
  # which 
  SRRI <- which.min(dif)
  
  # return
  list(dif,
       SRRI,
       dat.grps,
       med.rect.grp,
       ext.loc[[1]][, dir])
}
