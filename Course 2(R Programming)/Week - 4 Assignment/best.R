best <- function(state, outcome) {
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
  hospitalsWithLeastMortality <- 
    sort(newData[which(newData[,2] == min(newData[,2])),1])
  hospitalsWithLeastMortality[1]
}