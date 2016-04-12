# rCluster.R

#' @get /cluster
cluster <- function(){
  library(jsonlite)
  library(dbscan)
  library(dplyr)
  data <- fromJSON("http://159.203.247.240:3000/reports.json")
  
  data$type = as.factor(data$type)
  data$latitude = as.numeric(data$latitude)
  data$longitude = as.numeric(data$longitude)
  
  
  centroids = data.frame(latitude = numeric(), 
                         longitude = numeric(),
                         type = factor(),
                         cluster = factor())
  
  findCenters <- function(df){
    clusts = levels(df$clust)
    for(c in clusts){
      if(c != 0){
        a = df %>% filter(clust == c)
        latMed = median(a$latitude)
        longMed = median(a$longitude)
        centroids = rbind(centroids, data.frame(latitude=latMed,longitude=longMed,type=df$type[[1]],cluster=c))
      }
    }
    centroids
  }
  
  types = levels(data$type)
  for(t in types){
    type.equalsT = filter(data, type==t)
    clusts = dbscan(type.equalsT[,c("latitude","longitude")],eps = .05, minPts = 10)
    type.equalsT = mutate(type.equalsT, clust = as.factor(clusts$cluster))
    centroids = findCenters(type.equalsT)
  }
  centroids
}