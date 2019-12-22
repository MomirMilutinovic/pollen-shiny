source("ReadData.R")
everythingFromAPI <- function(firstPage, parse) {
  # Reads and parses all pages of a piece of data
  # from the api and returns them as a single data.frame
  
  data <- read_json(firstPage)
  result <- parse(data)
  
  i <- 1
  # Read all the pages
  while(!is.null(data$'next')){
    print(i)
    i <- i + 1
    data <- read_json(data$'next')
    result <- rbind(result, parse(data))
  }
  
  result
}

getData <- function() {
  # Downloads all the data about the measured concentrations of pollen
  # and joins them toghether into one data.frame
  
  locations <- getAndParse("http://polen.sepa.gov.rs/api/opendata/locations/", c("id", "location_name", "lat", "long", "desc"))
  allergens <- getAndParse("http://polen.sepa.gov.rs/api/opendata/allergens/", c("id", "allergen_name", "localized_name", "margine_top", "margine_bottom", "type", "allergenitcity", "allergenitcity_display"))
  pollens <- everythingFromAPI("http://polen.sepa.gov.rs/api/opendata/pollens/", parsePollen)
  concentrations <- everythingFromAPI("http://polen.sepa.gov.rs/api/opendata/concentrations/", parseConcentrations)
  
  pollen_location <- merge(pollens, locations, by.x = "location", by.y="id")
  concentration_allergen <- merge(concentrations, allergens, by.x = "allergen", by.y="id")
  
  pollendf <- merge(pollen_location, concentration_allergen, by.x = "concentration_id", by.y="id")
  
  pollendf$lat <- as.numeric(as.character(pollendf$lat))
  pollendf$long <- as.numeric(as.numeric(pollendf$long))
  
  #Delete unnecessary columns
  pollendf <- subset(pollendf, select = -c(concentration_id, location, desc, allergen, value, concentration, margine_top, margine_bottom, type))
  
  pollendf
}

# Download and write the data into a file
pollenDF <- getData()
write.csv(pollenDF, "data/pollens.csv")
