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
# Remember what Glenn said -- we're really just filtering on 1's.
#     It's not any more complicated. Do kmeans make it easier to
#     filter on 1's?

source("kmeans.R")

# Create 100 columns and 1000 rows
sim <- simulations(nrow=1000, ncol=100)
dataOut <- outlierSample(sim$dataCont, 0.003)

kmResults <- getKmeans(dataCont= sim$dataCont,
                       dataCat= sim$dataCat,
                       dataOut= dataOut)

# scatterplots for simulated data
scatter <- scatterplots(dataCont= sim$dataCont,
                        dataCat= sim$dataCat,
                        dataOut= dataOut)

## Alternative to Kmeans
# Categorical: how many 1's in each row? 
matches <- matchingRows(dataOut)
  
# Histogram of matching rows
matchPlot <- matchingPlot(mydata=dataOut,
              filename='hist_cat_outliers',
              chart.title= "Outliers as Categorical Variables",
              xaxis= "Sum of each row",
              yaxis= "Occurences out of 1000"
              )

# Methods for determining the number 'right' number of clusters
  # 'elbow method' must be plotted, try to find the 'elbow'

# Literature doesn't have a best practice determining cluster number
   # What makes sense from a substantive perspective?

# Plotting for the 'elbow' method
#pm <- plotMonster(dataCont= sim$dataCont,
#                  dataCat= sim$dataCat,
#                  dataOut)

# Plotting to view the clusters
#pm2 <- plotMonster2(dataCont= sim$dataCont,
#                    dataCat= sim$dataCat,
#                    dataOut)
  
#plotMatches(dataOut)

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


# Comparing results returned from outliers as categorical variables
  # with K-Means and Row-Count methods
kout <- clusterCount(dataOut)
rout <- rowCount(dataOut)    

