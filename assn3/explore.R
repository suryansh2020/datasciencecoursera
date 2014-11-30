# Tyler Brown
# Coursera R Programming -- Assignment 3
## Part 1: Plot the 30-day mortality rates for heart attack

outcome <- read.csv("data/outcome-of-care-measures.csv",
                    colClasses = "character")

head(outcome)

# make a histogram
outcome[,11] <- as.numeric(outcome[,11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[,11])
