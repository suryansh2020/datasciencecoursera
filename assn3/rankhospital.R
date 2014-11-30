# Tyler Brown
# Coursera R Programming -- Assignment 3
## Part 3: Ranking hospitals by outcome in a state

# data <- data[order(data$mortality, data$Hospital.Name), ]

rankhospital <- function(state, outcome, num = "best"){
  # Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv",
                    colClasses = "character")
  worstFlag <- FALSE

  # Check that state and outcome are valid
  if (state %in% data$State != TRUE){
    stop("invalid state")
  }

  # deal with best/worst
  if (num == "best"){
    num <- 1
  }
  if (num == "worst"){
    worstFlag <- TRUE
  }
  if (is.numeric(num) != FALSE && num > nrow(data)){
    return(NA)
  }

  # Return hospital name in that state with the given rank
  # 30-day death rate
  new <- data[data$State == state,]
  if (outcome == "heart attack"){
    new <- new[order(
      as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),
      new$Hospital.Name), ]
    if (worstFlag == TRUE){
      worst <- as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      worst <- sum(is.na(worst) == FALSE)
      return(new$Hospital.Name[worst])
    }
    else {
      return(new$Hospital.Name[num])
    }
  }
  else if (outcome == "heart failure"){
    new <- new[order(
      as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),
      new$Hospital.Name), ]
    if (worstFlag == TRUE){
      worst <- as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      worst <- sum(is.na(worst) == FALSE)
      return(new$Hospital.Name[worst])
    }
    else {
      return(new$Hospital.Name[num])
    }
  }
  else if (outcome == "pneumonia"){
    new <- new[order(
      as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
      new$Hospital.Name), ]
    if (worstFlag == TRUE){
      worst <- as.numeric(new$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      worst <- sum(is.na(worst) == FALSE)
      return(new$Hospital.Name[worst])
    }
    else {
      return(new$Hospital.Name[num])
    }
  }
  else {
    # check to see if outcome is valid
    stop("invalid outcome")
  }
}
