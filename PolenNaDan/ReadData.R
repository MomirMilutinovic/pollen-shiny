getAndParse <- function(url, nameVector) {
  # Downloads and parses a single list
  # with elements of the same type
  
  # Assings names to the columns of the resulting data.frame 
  # to the ones passed through nameVector
  
  # Used for downloading locations and
  # allergens
  
  data <- read_json(url)
  data <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T), stringsAsFactors = FALSE)
  names(data) <- nameVector
  
  data
}



parseConcentrations <- function(concentrationList) {
  # Parses the concentration JSONs
  
  data <- lapply(concentrationList$results, unlist)
  
  concentrations <- data.frame(id = integer(), allergen = numeric(), value = numeric(), concentration = numeric())
  
  for(i in 1:length(data)) {
    concentrations[nrow(concentrations) + 1,] <- data[[i]]
  }
  
  concentrations
}

parsePollen <- function(pollenList){
  # Parses the pollen JSONs
  
  pollendf <- data.frame(id = integer(), location = integer(), date = as.Date(character()), concentration = integer())
  for(i in 1:length(pollenList$results)){
    element <- pollenList$results[[i]]

    id <- element$id
    location <- element$location
    date <- as.Date(element$date)
    
    if(length(element$concentrations) == 0){
      next
    }
    
    for(j in 1:length(element$concentrations)){
      pollendf[nrow(pollendf) + 1,] <- list(id, location, date, element$concentrations[[j]])
    }
  
  }
  names(pollendf) <- c("id", "location", "date", "concentration_id")
  
  pollendf
}


