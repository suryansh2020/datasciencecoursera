# Data Science from Coursera
library(downloader)

downloadData <- function(url, directoryName, fileName){
  download(url, fileName)
  unzip(fileName, exdir = directoryName)
}

loadData <- function(file1, file2){
  rbind(read.table(file1),
        read.table(file2))
}

features <- function(joinData, path){
  features <- read.table(path)
  meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
  joinData <- joinData[, meanStdIndices]
  names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) 
  names(joinData) <- gsub("mean", "Mean", names(joinData)) 
  names(joinData) <- gsub("std", "Std", names(joinData)) 
  names(joinData) <- gsub("-", "", names(joinData))
  joinData
}

activity <- function(path){
  activity <- read.table(path)
  activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
  substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
  substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
  activity
}

labelActivity <- function(joinLabel, activity){
  activityLabel <- activity[joinLabel[, 1], 2]
  joinLabel[, 1] <- activityLabel
  names(joinLabel) <- "activity"
  joinLabel
}

cleanData <- function(joinSubject){
  names(joinSubject) <- "subject"
  cleanedData <- cbind(joinSubject, joinLabel, joinData)
  dim(cleanedData) # 10299*68
  cleanedData
}

results <- function(joinSubject, activity, cleanedData){
  subjectLen <- length(table(joinSubject)) # 30
  activityLen <- dim(activity)[1] # 6
  columnLen <- dim(cleanedData)[2]
  result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
  result <- as.data.frame(result)
  colnames(result) <- colnames(cleanedData)
  row <- 1
  for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
      result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
      result[row, 2] <- activity[j, 2]
      bool1 <- i == cleanedData$subject
      bool2 <- activity[j, 2] == cleanedData$activity
      result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
      row <- row + 1
    }
  }
  head(result)
  result
}


######################################################################
## Results from course project
##
######################################################################

# 1. ## Merges the training and the test sets to create one data set.


url <- paste("https://d396qusza40orc.cloudfront.net/",
             "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
             sep="")
directoryName <- "data"
fileName <- "canIgetaWhatWhat.zip"

#downloadData(url, directoryName, fileName)
joinData <- loadData("./data/train/X_train.txt",
                     "./data/test/X_test.txt")
joinLabel <- loadData("./data/train/y_train.txt",
                      "./data/test/y_test.txt")
joinSubject <- loadData("./data/train/subject_train.txt",
                        "./data/test/subject_test.txt")
joinData <- features(joinData, "./data/features.txt")
activity <- activity("./data/activity_labels.txt")
joinLabel <- labelActivity(joinLabel, activity)
cleanedData <- cleanData(joinSubject)
results <- results(joinSubject, activity, cleanedData)

head(results)


# 2. ## Extracts only the measurements on the mean and standard
#    ## deviation for each measurement.


# 3. ## Uses descriptive activity names to name the activities in the
#    ## data set


# 4. ## Appropriately labels the data set with descriptive variable
#    ## names.


# 5. ## From the data set in step 4, creates a second, independent
#    ## tidy data set with the average of each variable for each
#    ## activity and each subject.
#write.table(cleanSummaryTable(cleanData, subject, activity),
#            "cleanSummaryTable.txt")
