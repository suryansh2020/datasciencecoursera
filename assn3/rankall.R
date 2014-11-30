# Tyler Brown
# Coursera R Programming -- Assignment 3
## Part 4: Ranking hospitals in all states
library(data.table)
library(sqldf)

rankall <- function (outcome, num = "best"){
  # Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv",
                    colClasses = "character")
  statesTemp <- sqldf("SELECT State FROM data GROUP BY State;")
  states <- unlist(statesTemp)

  # create a data.table structure
  if (outcome == "heart attack"){
    temp <- cbind(data$State, data$Hospital.Name,
        as.numeric(
          data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
          ))
    colnames(temp) <- c("state","hospital","rate")
    # convert to data table
    d <- data.table(temp[complete.cases(temp),])
  }
  else if (outcome == "heart failure"){
    temp <- cbind(data$State, data$Hospital.Name,
        as.numeric(
          data$
          Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
          ))
    colnames(temp) <- c("state","hospital","rate")
    d <- data.table(temp[complete.cases(temp),])
  }
  else if (outcome == "pneumonia"){
    temp <- cbind(data$State, data$Hospital.Name,
        as.numeric(
          data$
          Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
          ))
    colnames(temp) <- c("state","hospital","rate")
    d <- data.table(temp[complete.cases(temp),])
  }
  else {
    # check to see if outcome is valid
    stop("invalid outcome")
  }
  
  # For each state, find the hospital of the given rank
  setkey(d, state, rate, hospital)
  d[, Rank := 1:.N, by = state]

  # Return a data frame with the hospital names and the
  # (abbreviated) state name
  setkey(d, state, Rank)
  d[.(states, rank=1), .(hospital, state)]
  d <- data.frame(subset(d, Rank == num))
  output <- sqldf("SELECT d.hospital, statesTemp.State AS state FROM statesTemp LEFT OUTER JOIN d ON d.state = statesTemp.State;")
  output <- data.frame(output)

}

# lets give this thing a break & come back to it in the morning.


