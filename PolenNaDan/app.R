#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("jsonlite")
library("leaflet")
library("dplyr")
library("tidyr")
library("RColorBrewer")

source("ReadData.R")

# Read from local file
pollen <- read.csv("data/pollen.csv", stringsAsFactors = FALSE)
pollen$date <- as.Date(pollen$date)

# Read one page from API
#pollen <- parseList(read_json("http://polen.sepa.gov.rs/api/opendata/pollens/")["results"][[1]])

# Read all pages from API
#pollen <- everythingFromAPI()

# Define UI for application that draws a map
ui <- fluidPage(

    # Application title
    titlePanel("Pollen on a given day"),

    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                        "Date:")
        ),

        # Show a map
        mainPanel(
            leafletOutput("map", width = "100%", height = 600),
            plotOutput("legend", width = "100%", height = 200)
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {
    
    #Create color pallete
    mx <- max(pollen$concetration)
    mn <- min(pollen$concetration)
    d=(mx-mn)/8
    br=seq(from=mn,to=mx,by=d)
    colorPalette <- brewer.pal(n = length(br), name = 'Greens')
    
    output$legend <- renderPlot({
        
        # Create vector of breaks shifted by one
        # that will be used for displaying the
        # intervals for each color
        higherBr <- br[2]
        
        for(i in 3:length(br)){
            higherBr[i - 1] <- br[i]
        }
        
        # Create the legend
        plot.new()
        legend("center",title = "Legend",legend = paste(as.character(br), " - ", as.character(higherBr)),col = colorPalette,pch=19, ncol = length(br)/2)
    })
    
    mapData <- reactive({
        md <- pollen
        indices <- which(md$date == input$date)
        md <- md[indices,]
    })

    output$map <- renderLeaflet({
        md <- mapData()
        
        
        
        if(nrow(md) == 0){
            # Draw an empty map if there is no data for
            # the given day
            leaflet() %>%
                addTiles() %>%
                fitBounds(18, 41, 23, 46)
        }else{
            # Color coding values
            colors <- vector()
            categories <- cut(md$concetration, breaks = br, include.lowest = TRUE)
            # Asigning colors
            for(i in 1:length(md$concetration))
                colors <- c(colors, colorPalette[as.integer(categories[i])])
            
            # Color code legend


            
            # Drawing map
            leaflet(data = md) %>%
                addTiles() %>%
                fitBounds(min(md$long), min(md$lat), max(md$long), max(md$lat)) %>%
                addCircleMarkers(lng = ~long, lat = ~lat,
                                 radius = 10, weight = 5, color = "black",
                                 fillColor = ~colors, fillOpacity = 0.7, popup = ~paste("Concetration:", as.character(concetration)))
        }
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
