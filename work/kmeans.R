## Simulation for K-means clustering
# In Datameer
## http://www.datameer.com/documentation/current/Clustering
# Online references
## http://www.statmethods.net/advstats/cluster.html
## http://stats.stackexchange.com/questions/48520
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

simulations <- function(nrow, ncol){
  # Generate continuous normally distributed variables.
  dataCont <- data.frame(matrix( 
    rnorm(nrow*ncol, mean=0,sd=1), nrow, ncol))

  # Generate categorical normally distributed variables.
  dataCat <- data.frame(matrix(
    rnorm(nrow*ncol, mean=0,sd=1), nrow, ncol))
  dataCat[dataCat <= 0] <- 0
  dataCat[dataCat > 0] <- 1

  # return values
  return(list(dataCont= dataCont, dataCat= dataCat))
}

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

### Scatter plots ###

# scatterplot for continuous normally distributed vars.

scatterplots <- function(dataCont, dataCat, dataOut){
  ## Various scatterplots for generated data
  
  ggplot(dataCont)+
    ggtitle("Continuous normally distributed variables") +
      geom_point(aes(x=X1, y=X2), size=3)
  ggsave("plots/cont_scatter.png", width=4, height=4, dpi=100) #plot 1
  
  # scatterplot for categorical normally distributed vars.
  ggplot(dataCat)+
    ggtitle("Categorical normally distributed variables") +
      geom_point(aes(x=X1, y=X2), size=3)
  ggsave("plots/cat_scatter.png", width=4, height=4, dpi=100) #plot 2

  # scatterplot for categorical outliers 
  ggplot(dataOut)+
    ggtitle("Categorical variables with outliers") +
      geom_point(aes(x=X1, y=X2), size=3)
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

matchingPlot <- function(mydata, filename, chart.title,
                         xaxis, yaxis){ # dataOut
  matches <- matchingRows(mydata)

  png(paste('plots/', filename, '.png', sep=""))
      hist(matches, main= chart.title,
           xlab= xaxis, ylab= yaxis)
  dev.off()
}

################################
### Create K-means clusters ####
################################

getKmeans <- function(dataCont, dataCat, dataOut){
    # Continuous variables
    cont <- kmeans(dataCont, centers = 3, iter.max= 100, 
                   algorithm="Hartigan-Wong")

    # Categorical variables
    cat <- kmeans(dataCat, centers = 3, iter.max= 100,
                  algorithm="Hartigan-Wong")

    # Categorical as outliers
    catout <- kmeans(dataOut, centers = 3, iter.max= 100,
                     algorithm="Hartigan-Wong")
    return(list(cont= cont,
                cat= cat,
                catout= catout))
}

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

plotMonster <- function(dataCont, dataCat, dataOut){
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

# Substantive approach (for what purpose are you running kmeans?

## This seems to be the better approach.

#################################
### Plot K-means clusters #######
#################################

plotMonster2 <- function(dataCont, dataCat, dataOut){
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

# generated data for outliers as categorical variables

# matching rows for generated data
plotMatches <- function(dataOut){
  matches <- matchingRows(dataOut)

  png('plots/sum_matchingrows_categorical.png')
  hist(matches, main= paste("Histogram of Summed Outliers Across",
                  " Rows (Categorical)", sep=""),
       xlab= "Sum of each row", ylab="Occurences out of 1000")
  dev.off()
}

# comparing kmeans to matchingRows()
clusterCount <- function(kmeansObject){
  counts <- numeric(length(unique(kmeansObject$cluster)))
  for (clusterNum in 1:max(length(counts))){
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

  
 
