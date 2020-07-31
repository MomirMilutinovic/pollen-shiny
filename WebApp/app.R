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
library("RColorBrewer")
library("httr")

#setwd("WebApp")

source("ReadData.R")
source("joinData.R")
source("getData.R")
source("get_JSON.R")
source("getHTMLTable.R")


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
        )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {
    
    # Create color pallete
    mx <- 1000
    mn <- 0
    d=(mx-mn)/8
    br=seq(from=mn,to=mx,by=d)
    colorPalette <- brewer.pal(n = length(br), name = 'Greens')
    
    mapData <- reactive({
        path <- paste("/api/opendata/pollens/?date_after=", input$date, "&date_before=", input$date, sep = "")
        pollendf <- parsePage("http://polen.sepa.gov.rs/", path, parsePollen)
        
        
        if(nrow(pollendf) > 0)
        {
            joinData(pollendf) %>% filter(long != 0, lat != 0)
        }
        else
        {
            pollendf
        }
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
            categories <- cut(md$value, breaks = br, include.lowest = TRUE)
            # Asigning colors
            
            for(i in 1:length(md$value))
                colors <- c(colors, colorPalette[as.integer(categories[i])] )
            # Drawing map
            leaflet(data = md) %>%
                addTiles() %>%
                fitBounds(min(md$long) - 0.01, min(md$lat) - 0.01, max(md$long) + 0.01, max(md$lat) + 0.01 ) %>%
                addCircleMarkers(lng = ~long, lat = ~lat,
                                 radius = 10, weight = 5, color = "black",
                                 fillColor = ~colors, fillOpacity = 0.7, 
                                 popup = ~getHTMLTables(md))
        }
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
