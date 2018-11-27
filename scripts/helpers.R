distance = function(X1,Y1,X2,Y2){
  return(sqrt((Y2-Y1)^2 + (X2-X1)^2))
}


computeCostForRoute = function(routeDF) {
  routeDF %>% 
    rowid_to_column("stepNo") %>% 
    rename(from = CityId) %>% 
    mutate(to = lead(from),
           X2 = lead(X),
           Y2 = lead(Y),
           isPrime = from %in% primes) %>% 
    mutate(distanceCost = memDistance(X,Y,X2,Y2),
           primeCost = ifelse((stepNo %% 10 == 0) & !isPrime ,distanceCost * 0.1,0),
           totalCost = distanceCost + primeCost)
  
}


getTSPSolution = function(df,method = "nn",reps = 4) {
  dist = dist(as.matrix(df %>% select(X,Y)))
  
  tsp = TSP(dist)
  tspPath = solve_TSP(tsp,method,
                      control = list(repetitions = reps))
  
  # improvedPath = solve_TSP(tsp, method ="two_opt", control = list(tour = tspPath))
  completePath = c(tspPath,tspPath[1])
  completePath
}

getShortestPathBetween = function(df,fromId,toId,method = "nn",reps = 4) {
  dist = dist(as.matrix(df %>% select(X,Y)))
  tsp = TSP(dist,labels = df$CityId)
  
  from = which(df$CityId == fromId)
  to = which(df$CityId == toId)
  
  m = as.matrix(tsp)
  atsp <- ATSP(m[-c(from,to), -c(from,to)])
  
  marker = "inserted"
  atsp <- insert_dummy(atsp, label = marker)
  markerIndex <- which(labels(atsp) == marker)
  atsp[markerIndex, ] <- c(m[-c(from,to), from], 0)
  atsp[, markerIndex] <- c(m[to, -c(from,to)], 0)
  
  if (nrow(df) < 500){
    method = "farthest_insertion"
  }
  
  tspPath = solve_TSP(atsp,method,
                      control = list(repetitions = reps))
  
  path_labels <- c(fromId,
                   labels(cut_tour(tspPath, markerIndex)), 
                   toId)
  path_ids <- match(path_labels, labels(tsp))
  result = df %>% slice(path_ids)
  
  # if from and to are the same (which can happen if entry and exit points to cluster
  # are the same), then simply strip off last point
  
  if (fromId == toId)
  {
    result = result[-nrow(df),]
  }
  result
  
  # improvedPath = solve_TSP(tsp, method ="two_opt", control = list(tour = tspPath))
}

findNearestPointsBetweenClusters = function(fromClusterIndex,toClusterIndex) {
  
  prevClusterId = clusterCentrePath$clusterId[fromClusterIndex]
  prevClust = getPointsInCluster(allWithCluster,prevClusterId)
  
  nextClusterId = clusterCentrePath$clusterId[toClusterIndex]
  nextCluster = getPointsInCluster(allWithCluster,nextClusterId)
  
  print(glue("Finding closest links between clusters {prevClusterId} and {nextClusterId}"))
  
  k = min(3,nrow(prevClust),nrow(nextCluster))
  knn.out <- ann(as.matrix(prevClust %>% select(X,Y)), 
                 as.matrix(nextCluster %>% select(X,Y)), 
                 k)
  
  firstPointInNext = which.min(knn.out$knnIndexDist[,k + 1])
  firstIdInNext = nextCluster$CityId[firstPointInNext]
  
  lastPointInPrev = knn.out$knnIndexDist[firstPointInNext,1]
  lastIdInPrev = prevClust$CityId[lastPointInPrev]
  
  tibble(fromId = firstIdInNext,toId = lastIdInPrev)
}
