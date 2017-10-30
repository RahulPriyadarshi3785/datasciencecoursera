corr <-  function(directory, threshold = 0){
  setwd(directory)
  correlationvector <- NULL
  for(i in 1:332) {
    if(i < 10) {
      name <- paste("0", "0", as.character(i), ".csv", sep = "")
    }else if(i > 9 & i < 100){
      name <- paste("0", as.character(i), ".csv", sep = "")
    }else{
      name <- paste(as.character(i), ".csv", sep = "")
    }
    readdata <- na.omit(read.csv(name))
    if(nrow(readdata) > threshold){
      correlationvector <- c(correlationvector, cor(readdata[, 2], readdata[, 3]))
    }
  }
  setwd("..")
  correlationvector
}