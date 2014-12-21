# Step1. Merges the training and the test sets to create one data set.
# setwd("~/Desktop/Online Coursera/Coursera-Getting-and-Cleaning-Data/peer_assessment/")

data <- function(){
  trainData <- read.table("./data/train/X_train.txt")
  dim(trainData) # 7352*561
  head(trainData)
  testData <- read.table("./data/test/X_test.txt")
  dim(testData) # 2947*561
  joinData <- rbind(trainData, testData)
  dim(joinData) # 10299*561
  joinData
}

label <- function(){
  trainLabel <- read.table("./data/train/y_train.txt")
  table(trainLabel)
  testLabel <- read.table("./data/test/y_test.txt") 
  table(testLabel)
  joinLabel <- rbind(trainLabel, testLabel)
  dim(joinLabel) # 10299*1
  joinLabel
}

subject <- function(){
  trainSubject <- read.table("./data/train/subject_train.txt")
  testSubject <- read.table("./data/test/subject_test.txt")
  joinSubject <- rbind(trainSubject, testSubject)
  dim(joinSubject) # 10299*1
  joinSubject
}

# Step2. Extracts only the measurements on the mean and standard 
# deviation for each measurement.

features <- function(joinData){
  features <- read.table("./data/features.txt")
  dim(features)  # 561*2
  meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
  length(meanStdIndices) # 66
  joinData <- joinData[, meanStdIndices]
  dim(joinData) # 10299*66
  names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
  names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
  names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
  names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names
  joinData
}

# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- function(){
  activity <- read.table("./data/activity_labels.txt")
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

# Step4. Appropriately labels the data set with descriptive activity 
# names.
cleanData <- function(joinSubject){
  names(joinSubject) <- "subject"
  cleanedData <- cbind(joinSubject, joinLabel, joinData)
  dim(cleanedData) # 10299*68
  cleanedData
}

#write.table(cleanedData, "merged_data.txt") # write out the 1st dataset


# Step5. Creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
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

## ok
joinData <- data()
joinLabel <- label()
joinSubject <- subject()
joinData <- features(joinData)
activity <- activity()
joinLabel <- labelActivity(joinLabel, activity)
cleanedData <- cleanData(joinSubject)
results <- results(joinSubject, activity, cleanedData)

head(results)
