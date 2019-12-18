# Reading the list of stations
stationList <- read_json("http://polen.sepa.gov.rs/api/opendata/locations/")
locations <- data.frame(matrix(unlist(stationList), nrow=length(stationList), byrow=T))
names(locations) <- c("ID", "Naziv", "lat", "long", "podaci")

# Reading the list of concetrations
pollenList <- read_json("http://polen.sepa.gov.rs/api/opendata/pollens/")

# pollenList[4][[1]] contains the actual data
pollenList <- pollenList[4][[1]]

pollen <- data.frame(id = integer(), long = numeric(), lat = numeric(), date = as.Date(character()),
                     concetration = numeric())

# Parsing json by iterating through the lists returned from read_json
# and putting entries into a data.frame
for(i in 1:length(pollenList)){
  id <- pollenList[[i]]$id
  locationID <- pollenList[[i]]$location
  date <- as.Date(pollenList[[i]]$date)
  concetrations <- pollenList[[i]]$concentrations
  
  index <- which(locations$ID == locationID)
  long <- as.numeric(as.character(locations$long[index]))
  lat <- as.numeric(as.character(locations$lat[index]))
  
  if(length(concetrations) == 0){
    next
  }
  
  if(lat == 0 & long == 0){
    next
  }
  
  for(j in 1:length(concetrations)){
    pollen[nrow(pollen) + 1,] = list(i, long, lat, date, concetrations[[j]])
  }
}
