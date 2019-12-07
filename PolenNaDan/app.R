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

# Define UI for application that draws a map
ui <- fluidPage(

    # Application title
    titlePanel("Pollen on a given day"),

    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                        "Date:"),
        ),

        # Show a map
        mainPanel(
            leafletOutput("map", width = "100%", height = 600)
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {
    
    mapData <- reactive({
        md <- pollen
        indices <- which(md$date == input$date)
        md <- md[indices,]
    })

    output$map <- renderLeaflet({
        md <- mapData()
        
        if(nrow(md) == 0){
            #Draw an empty map if there is no data for
            #the given day
            leaflet() %>%
                addTiles() %>%
                fitBounds(18, 41, 23, 46)
        }else{
            #Color coding values
            colors <- vector()
            categories <- cut(md$concetration, breaks = 8)
            #Asigning colors
            for(i in 1:length(md$concetration))
                colors <- c(colors, brewer.pal(n = 8, name = 'Greens')[as.integer(categories[i])])
            
            #Drawing map
            print(head(md))
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
