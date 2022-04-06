## sampling documents into test set ##

# first generate test sample folder in each KAG folder #

# setwd
setwd("GitHub/KID/KIDs")

# dirs
dirs <- list.dirs()[-c(1, 4)] 

# step into first folder
setwd(dirs[1])

# loop over dirs to create test folder
lapply(dirs, \(x){

  # set
  setwd(paste0("./../", x))
  
  # sample two documents
  sam <- sample(list.files(pattern = ".pdf"), size = 2)
  
  # create dir
  dir.create("Test")
  
  # move files into Test
  lapply(sam, \(y){
    
    # move via rename
    file.rename(from = y,
                to = paste0("./Test/", y))
    
  })

})

