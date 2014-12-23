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

# scatterplot for continuous normally distributed vars.
ggplot(dataCont)+
  ggtitle("Continuous normally distributed variables") +
  geom_point(aes(x=dataCont$X1, y=dataCont$X2), size=3)
ggsave("plots/cont_scatter.png", width=4, height=4, dpi=100) #plot 1

# scatterplot for categorical normally distributed vars.
ggplot(dataCat)+
  ggtitle("Categorical normally distributed variables") +
  geom_point(aes(x=dataCat$X1, y=dataCat$X2), size=3)
ggsave("plots/cat_scatter.png", width=4, height=4, dpi=100) #plot 2


################################
### Create K-means clusters ####
################################

# Continuous variables
cont <- kmeans(dataCont, centers = 3, iter.max= 100, 
        algorithm="Hartigan-Wong")

# Categorical variables
cat <- kmeans(dataCat, centers = 3, iter.max= 100,
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

# Substantive approach (for what purpose are you running kmeans?

## This seems to be the better approach.



#################################
### Plot K-means clusters #######
#################################

# Continuous variables
clusplot(dataCont, cont$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

plotcluster(dataCont, cont$cluster)

# Categorical variables
clusplot(dataCat, cat$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

plotcluster(dataCat, cat$cluster)


## Pros so far
# This may be a good approach for clustering on outliers
# Categorical variables don't seem to really effect the outcome, although
#     we don't really have an outcome either way. The effect might be smaller
#     than anticipated. Can you show why this happens?

## Cons so far
# Distinct clusters aren't really formed when the data is normally distributed
# It's hard to choose the 'right' number of clusters
# An incorrect cluster choice could pull 'outliers' into a larger cluster

## Additional things to check
# How will the clustering work if we anticipate outliers?
#
# Need to have a better explanation of which math you're using when
#      you calculate the cost function. How exactly does the cost
#      function work? What's your x coordinate? why? Need to be able
#      to better explain the 'elbow method'.
#
# Need to have more patience stepping through the math. It seems like you
# have an intuitive understanding for how the algorithm works.
#
# How does this specifically tie back into datameer?
#
# Remember what Glenn said -- we're really just filtering on 1's. It's not
# any more complicated. Does kmeans make it easier to filter on 1's?
