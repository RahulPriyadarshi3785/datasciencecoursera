complete <- function(directory, id = 1:332) {
  setwd(directory)
  forallfile <- NULL
  for(i in id) {
    if(i < 10) {
      name <- paste("0", "0", as.character(i), ".csv", sep = "")
    }else if(i > 9 & i < 100){
      name <- paste("0", as.character(i), ".csv", sep = "")
    }else{
      name <- paste(as.character(i), ".csv", sep = "")
    }
    forallfile <- rbind(forallfile, c(i , dim.data.frame(na.omit(read.csv(name)))[1]))
  }
  setwd("..")
  forallfile
}