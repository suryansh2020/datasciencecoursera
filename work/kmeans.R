## Simulation for K-means clustering
# In Datameer
## http://www.datameer.com/documentation/current/Clustering
# Online references
## http://www.statmethods.net/advstats/cluster.html
## http://stackoverflow.com/questions/15376075
## https://class.coursera.org/ml-005/lecture/78
## https://class.coursera.org/ml-005/lecture/79
## https://class.coursera.org/ml-005/lecture/80
## https://class.coursera.org/ml-005/lecture/81
library(cluster)
library(ggplot2)
library(fpc)
# clean house
#rm(list = ls(), envir = globalenv())

###############################
### Create simulation data ####
###############################

# Create 100 columns and 1000 rows
nrow <- 1000
ncol <- 100

# Generate continuous normally distributed variables.
dataCont <- data.frame(matrix( 
  rnorm(nrow*ncol, mean=0,sd=1), nrow, ncol))

# Generate categorical normally distributed variables.
dataCat <- data.frame(matrix(
  rnorm(nrow*ncol, mean=0,sd=1), nrow, ncol))
dataCat[dataCat <= 0] <- 0
dataCat[dataCat > 0] <- 1

# Generate a set of outliers based on a normal distribution.
# def: an outlier occurs >= 3 standard deviations from the mean or
#      < 99.7 percent.
# Outliers already exist for the continuous variables so no need to
# write/include anything in this function.

outlierSample <- function(mydata, percent){
  # mydata: data frame
  # percent: as a decimal
  if (percent > 1 || percent < 0){
    stop("Percent must be a decimal < 1 && > 0")
  }
  else{
    n <- round(percent * nrow(mydata))
    mydata[] <- 0
    # make sure each column is sampled independently
    col <- 1
    while (col <= ncol(mydata)){
      mydata[sample(nrow(mydata), n), col] <- 1
      col <- col + 1
    }
    return(mydata)
  }
}

dataOut <- outlierSample(dataCont, 0.003)

### Scatter plots ###

# scatterplot for continuous normally distributed vars.

scatterplots <- function(){
  ## Various scatterplots for generated data
  
  ggplot(dataCont)+
    ggtitle("Continuous normally distributed variables") +
      geom_point(aes(x=dataCont$X1, y=dataCont$X2), size=3)
  ggsave("plots/cont_scatter.png", width=4, height=4, dpi=100) #plot 1
  
  # scatterplot for categorical normally distributed vars.
  ggplot(dataCat)+
    ggtitle("Categorical normally distributed variables") +
      geom_point(aes(x=dataCat$X1, y=dataCat$X2), size=3)
  ggsave("plots/cat_scatter.png", width=4, height=4, dpi=100) #plot 2

  # scatterplot for categorical outliers 
  ggplot(dataCat)+
    ggtitle("Categorical variables with outliers") +
      geom_point(aes(x=dataCat$X1, y=dataCat$X2), size=3)
  ggsave("plots/cat_scatter.png", width=4, height=4, dpi=100) #plot 3
}
#scatterplots()

### Descriptive statistics ###

## Categorical

matchingRows <- function(mydata, decrease=TRUE){
  # Categorical: how many 1's in each row?
  matches <- numeric(nrow(mydata))
  for (row in 1:nrow(mydata)){
    matches[row] <- sum(mydata[row,])
  }
  return(sort(matches, decreasing=decrease))
}

matches <- matchingRows(dataOut)

png('plots/hist_cat_outliers.png')
hist(matches, main= "Histogram of Outliers as Categorical Variables",
     xlab= "Sum of each row", ylab="Occurences out of 1000")
dev.off()

################################
### Create K-means clusters ####
################################

# Continuous variables
cont <- kmeans(dataCont, centers = 3, iter.max= 100, 
        algorithm="Hartigan-Wong")

# Categorical variables
cat <- kmeans(dataCat, centers = 3, iter.max= 100,
              algorithm="Hartigan-Wong")

# Categorical as outliers
catout <- kmeans(dataOut, centers = 3, iter.max= 100,
                 algorithm="Hartigan-Wong")

##############################################################
### Choosing the number of clusters: must be done manually ###
##############################################################

# Elbow method (K(no. of clusters), Cost function J)
# ...'worth a shot'

# stackoverflow post using elbow method
elbowMethod <- function(mydata, numClusters){
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:numClusters) wss[i] <- sum(kmeans(mydata,
                                        centers=i)$withinss)
  
  plot(1:numClusters, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

plotMonster <- function(){
  ## Plots that show the 'elbow method'
  
  # continuous
  png('plots/cont_elbow_3clus.png')
  elbowMethod(dataCont, 3)
  dev.off()
  
  png('plots/cont_elbow_500clus.png')
  elbowMethod(dataCont, 500)
  dev.off()
  
  # categorical
  png('plots/cat_elbow_3clus.png')
  elbowMethod(dataCat, 3)
  dev.off()
  
  png('plots/cat_elbow_500clus.png')
  elbowMethod(dataCat, 500)
  dev.off()

  # outliers as categorical
  png('plots/catout_elbow_3clus.png')
  elbowMethod(dataOut, 3)
  dev.off()

  png('plots/catout_elbow_20clus.png')
  elbowMethod(dataOut, 20)
  dev.off()
}

#plotMonster()

# Substantive approach (for what purpose are you running kmeans?

## This seems to be the better approach.



#################################
### Plot K-means clusters #######
#################################

plotMonster2 <- function(){
  ## Various plots for k-means clusters
  
  # Continuous variables
  clusplot(dataCont, cont$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0)

  plotcluster(dataCont, cont$cluster)

  # Categorical variables
  clusplot(dataCat, cat$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0)

  plotcluster(dataCat, cat$cluster)

  # Outliers as categorical variables
  clusplot(dataOut, catout$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0)
  
  plotcluster(dataOut, catout$cluster)
}

# plotMonster2()

######################
### Use cases ########
######################

# When variables are normally distributed, either continuous or
#   categorical, we aren't able to generate clusters that return
#   anything that would appear to be meaningful for indicators.
# When we simulate outliers as categorical variables, then kmeans
#  does a pretty reasonable job clustering the rows that include the
#  maximum number of matches across each row.
# If we could write a function similar to matchingRows() and then
#   filter on the number of matching entries in each row, it would
#   probably be more helpful than kmeans.


# generated data for outliers as categorical variables
summary(dataOut)

# matching rows for generated data
plotMatches <- function(dataOut){
  matches <- matchingRows(dataOut)

  png('plots/sum_matchingrows_categorical.png')
  hist(matches, main= paste("Histogram of Summed Outliers Across",
                  " Rows (Categorical)", sep=""),
       xlab= "Sum of each row", ylab="Occurences out of 1000")
  dev.off()
}

#plotMatches(dataOut)

# comparing kmeans to matchingRows()
clusterCount <- function(kmeansObject){
  counts <- numeric(length(unique(kmeansObject$cluster)))
  for (clusterNum in 1:max(length(counts)){
    counts[clusterNum] <- sum(kmeansObject$cluster == clusterNum)
  }
  return(counts)
}



rowCount <- function(rowcountObject){
  counts <- matrix(nrow=2,
                   ncol=length(unique(matchingRows(rowcountObject))))
  count <- 1
  for (rowcount in unique(matchingRows(rowcountObject))){
    counts[1, count] <- rowcount
    counts[2, count] <- sum(matchingRows(rowcountObject) == rowcount)
    count <- count + 1
  }
  return(counts)
}

# kmeans membership
clusterCount(catout)
# summed rows membership
rowCount(dataOut)




























## Pros so far
# This may be a good approach for clustering on outliers
# Categorical variables don't seem to really effect the outcome,
#     although we don't really have an outcome either way. The effect
#     might be smaller than anticipated. Can you show why this happens?

## Cons so far
# Distinct clusters aren't really formed when the data is normally
#          distributed
# It's hard to choose the 'right' number of clusters
#
# An incorrect cluster choice could pull 'outliers' into a larger
#          cluster

## Additional things to check
# How will the clustering work if we anticipate outliers?
#
# Need to have a better explanation of which math you're using when
#      you calculate the cost function. How exactly does the cost
#      function work? What's your x coordinate? why? Need to be able
#      to better explain the 'elbow method'.
#
# Need to have more patience stepping through the math. It seems like
#      you have an intuitive understanding for how the algorithm works.
#
# How does this specifically tie back into datameer?
#
# Remember what Glenn said -- we're really just filtering on 1's.
#     It's not any more complicated. Does kmeans make it easier to
#     filter on 1's?
