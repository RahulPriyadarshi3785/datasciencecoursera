pollutantmean <- function(directory, pollutant, id = 1: 332){
  setwd(directory)
  totalpollutant <- 0
  size <- 0
  for(i in id) {
    if(i > 0 & i < 10){
      name <- paste("0", "0", as.character(i), ".csv", sep = "")
    }else if(i > 9 & i < 100){
      name <- paste("0", as.character(i), ".csv", sep = "")
    }else{
      name <- paste(as.character(i), ".csv", sep = "")
    }
    readdata <- read.csv(name)
    vect <- na.omit(readdata[, pollutant])
    size <- size + length(vect)
    totalpollutant <- totalpollutant + sum(vect, na.rm = TRUE)
  }
  setwd("..")
  totalpollutant/size
}