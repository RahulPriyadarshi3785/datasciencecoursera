rankhospital <- function(state , outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  States <- unique(data$State)
  Outcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(state %in% States) & !(outcome %in% Outcomes)){
    stop(print("Invalid state"))
  }
  mortalityRates <- c(11, 17, 23)
  colOfDeathIn30Days <- mortalityRates[outcome == Outcomes]
  data[,colOfDeathIn30Days] <- as.numeric(data[,colOfDeathIn30Days])
  newData <- na.omit(subset(data, data$State == state,
                            select = c(2,colOfDeathIn30Days)))
  hospitalsWithMortality <- newData[order(newData[,2], newData[,1]),]
  if(num == "best") {
    numRank <- 1
  } else if(num == "worst") {
    numRank <- nrow(hospitalsWithMortality)
  } else {
    numRank <- num
  }
  if(numRank > nrow(newData)){
    return(NA)
  }
  return(hospitalsWithMortality[numRank,1])
}