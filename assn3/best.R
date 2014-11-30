# Tyler Brown
# Coursera R Programming -- Assignment 3
## Part 2: Finding the best hospital in a state

best <- function(state,outcome){
  # Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv",
                    colClasses = "character")

  # Check that state and outcome are valid
  if (state %in% data$State != TRUE){
    stop("invalid state")
  }

  # Return hospital name in that state with lowest 30-day health rate
  new <- data[data$State == state,]
  if (outcome == "heart attack"){
    return(new$Hospital.Name[which.min(
      new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
  }
  else if (outcome == "heart failure"){
    return(new$Hospital.Name[which.min(
      new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)])
  }
  else if (outcome == "pneumonia"){
    return(new$Hospital.Name[which.min(
      new$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
  }
  else {
    # check to see if outcome is valid
    stop("invalid outcome")
  }

}


