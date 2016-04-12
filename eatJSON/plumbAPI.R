library(plumber)
r <- plumb("/Users/brandonsherman/Documents/CodeForSac/eatJSON/rCluster.R")
r$run(port=8000)
