library("jsonlite")
library("leaflet")
library("dplyr")
library("tidyr")
library("RColorBrewer")

l <- read_json("data/lokacije.json")
lokacije <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))
names(lokacije) <- c("ID", "Naziv", "Podaci", "long", "lat")


l <- read_json("data/polen.json")
names(polen) <- c("id", "long", "lat", "date", "concetration")

#Parsing json
#id: l[[row]][1][[1]]
#location: l[[row]][2][[1]]
#date: l[[row]][2][[1]]
#concetration: l[[row]][4][[1]][number]

for(i in 1:length(l)){
  if(length(l[[i]][4][[1]]) == 0){
    next
  }
  for(j in 1:length(l[[i]][4][[1]])){
    index <- which(lokacije$ID == l[[i]][2][[1]])
    long <- as.numeric(as.character(lokacije$long[index]))
    lat <- as.numeric(as.character(lokacije$lat[index]))
    polen[nrow(polen) + 1,] = list(l[[i]][[1]], long, lat, as.Date(l[[i]][3][[1]]), l[[i]][4][[1]][[j]])
  }
}


#write.csv(polen,"data/polenSaPozicijom.csv", row.names = TRUE)

#class(polen$concetration)

indices <- which(polen$date == "2016-03-02")

polen <- polen[indices,]

minlat <- min(polen$lat)
maxlat <- max(polen$lat)
minlong <- min(polen$long)
maxlong <- max(polen$long)

boje <- vector()
kategorije <- cut(polen$concetration, breaks = 8)


for(i in 1:length(polen$concetration))
  boje <- c(boje, brewer.pal(n = 8, name = 'Greens')[as.integer(kategorije[i])])

leaflet(data = polen) %>%
  addTiles() %>%
  fitBounds(~minlong, ~minlat, ~maxlong, ~maxlat) %>% 
  addCircleMarkers(lng = ~long, lat = ~lat,
             radius = 10, weight = 5, color = "black",
             fillColor = ~boje, fillOpacity = 0.7, popup = ~as.character(concetration))