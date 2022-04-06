## sampling documents into test set ##

# setwd
setwd("GitHub/KID/KIDs")

# dirs
dirs <- list.dirs()[-c(1, 4)] 

# step into first folder
setwd(dirs[1])

# loop over dirs to create test folder and randomly sample two KIDs 
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

