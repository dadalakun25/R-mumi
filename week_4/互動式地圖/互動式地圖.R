library(leaflet)
mountain <- read.csv("mountain.csv")
options(digits=8)
mountain$X <- as.character(mountain$X)
mountain$mountain <- as.character(mountain$mountain)
mountain$long <- as.numeric(paste(substr(mountain$X,14,16),".",substr(mountain$X,18,19),substr(mountain$X,22,23),sep=''))
mountain$lat <- as.numeric(paste(substr(mountain$X,2,3),".",substr(mountain$X,5,6),substr(mountain$X,9,10),sep=''))
str(mountain)
map <- leaflet() %>%
  addMarkers(lng=mountain$long,lat=mountain$lat,popup=mountain$mountain) %>%
  addTiles() %>%
  fitBounds("120","23","122","25")
map
