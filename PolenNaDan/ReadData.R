# Downloads every entry from the API,
# assigns locations to the entries and
# writes them into a csv file

# Reading the list of stations
stationList <- read_json("http://polen.sepa.gov.rs/api/opendata/locations/")
locations <- data.frame(matrix(unlist(stationList), nrow=length(stationList), byrow=T))
names(locations) <- c("ID", "Naziv", "lat", "long", "podaci")

# Reading the list of concetrations
pollenList <- read_json("http://polen.sepa.gov.rs/api/opendata/pollens/")

pollenListResults <- pollenList["results"][[1]]

parseList <- function(resultList){
  # Parsing json by iterating through the lists returned from read_json
  # and putting entries into a data.frame
  
  pollen <- data.frame(id = integer(), long = numeric(), lat = numeric(), date = as.Date(character()),
                       concetration = numeric())
  
  for(i in 1:length(resultList)){
    id <- resultList[[i]]$id
    locationID <- resultList[[i]]$location
    date <- as.Date(resultList[[i]]$date)
    concetrations <- resultList[[i]]$concentrations
    
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
  pollen
}

pollen <- parseList(pollenListResults)

# Read all the pages
while(!is.null(unlist(pollenList["next"]))){
  pollenList <- read_json(unlist(pollenList["next"]))
  pollenListResults <- pollenList["results"][[1]]
  pollen <- rbind(pollen, parseList(pollenListResults))
}

write.csv(pollen, "data/pollen.csv")

