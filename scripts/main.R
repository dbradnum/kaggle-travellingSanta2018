library(tidyverse)
library(memoise)
library(primes)
library(TSP)
library(doParallel)
library(glue)
library(yaImpute)

registerDoParallel()
theme_set(theme_bw())

source("scripts/helpers.R")


memDistance = memoise(distance)

primes = generate_primes(max = 200000)


all = read_csv("data/cities.csv") 
# cities = all %>% rbind(all %>% filter(CityId == 0))

kClusters = 750

clusters = kmeans(all %>% select(X,Y), 
      centers = kClusters,
      iter.max = 50)

clusterCentreDF = as.tibble(clusters$centers)

centreSolution = getTSPSolution(clusterCentreDF,
                                "farthest_insertion",
                                20)

clusterCentrePath = clusterCentreDF %>% 
  rowid_to_column("clusterId") %>% 
  slice(centreSolution)

ggplot(clusterCentrePath) + geom_path(aes(X,Y))

tibble(cluster = clusters$cluster) %>% count(cluster) %>% ggplot() + geom_histogram(aes(n))


allWithCluster = all %>% cbind(cluster = clusters$cluster)

# allWithCluster %>% count(cluster) %>% arrange(-n)
# 
# ggplot() +
#   geom_point(data = allWithCluster,
#              aes(X,Y, color = as.factor(cluster)),alpha = 0.2) +
#   geom_path(data = clusterCentrePath,aes(X,Y))


getPointsInCluster = function(df,clusterId){
  df %>% 
    filter(cluster == clusterId) 
}


# clusterToFilter = 1
# 
# filteredByCluster = allWithCluster %>% filter(cluster == clusterToFilter)
# 
# withinClusterPath = getShortestPathBetween(filteredByCluster,55,80)
# ggplot(withinClusterPath) + geom_path(aes(X,Y))


nClustersToWalk = kClusters

withinClusterPaths = tibble()
for (clusterIndex in 1:nClustersToWalk) {
  links = findNearestPointsBetweenClusters(clusterIndex,clusterIndex + 1)
  
  withinClusterPaths = withinClusterPaths %>% 
    rbind(cbind(clusterIndex,links))
}

# shift fromId down to match up with next cluster
withinClusterPaths = withinClusterPaths %>% 
  rbind(tibble(clusterIndex = clusterIndex + 1,fromId = NA,toId = NA)) %>% 
  mutate(fromId = lag(fromId))

# now find nearest link between last cluster and first, to close ends

links = findNearestPointsBetweenClusters(clusterIndex + 1,1)

withinClusterPaths$fromId[1] = links$fromId
withinClusterPaths$toId[nrow(withinClusterPaths)] = links$toId

totalPath = tibble()
for (clusterIndex in 1:(nrow(withinClusterPaths) - 1)) {
  clusterId = clusterCentrePath$clusterId[clusterIndex]
  
  print(glue("Finding path through cluster {clusterIndex}, clusterId = {clusterId}"))
  
  pathWithinCluster = getShortestPathBetween(
    getPointsInCluster(allWithCluster,clusterId),
    withinClusterPaths$fromId[clusterIndex], 
    withinClusterPaths$toId[clusterIndex],
    reps = 3)
  
  
  if (!exists("totalPath")) {
    totalPath = pathWithinCluster
  } else {
    totalPath = totalPath %>% rbind(pathWithinCluster)
  }
}


ggplot(totalPath) + 
  geom_path(aes(X,Y,color = as.factor(cluster))) + 
  theme(legend.position = "none")


start = which(totalPath$CityId == 0)
pathFromStart = rbind(totalPath[start:nrow(totalPath),],
                      totalPath[1:start,])

cost = pathFromStart %>% computeCostForRoute() 
finalCost = sum(cost$totalCost,na.rm = T)

cat(glue("***** FINAL COST = {finalCost} *****"))

outDir = "output"
if (!dir.exists(outDir)) {
  dir.create(outDir)
}

write_csv(pathFromStart %>% select(CityId) %>% rename(Path = CityId),
          file.path(outDir,"result.csv"))

cost %>% 
  group_by(cluster) %>% 
  summarise(totalCost = sum(totalCost),
            n = n(),
            meanCost = totalCost/n) %>% 
  arrange(-meanCost)

