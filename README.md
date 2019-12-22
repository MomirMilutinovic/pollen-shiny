# pollen-shiny
Shiny app that shows the concentrations of pollen measured by various stations in Serbia on a given day 

## Data
The app uses pollen concentration data in Serbia from February 2016 onwards. The data is available at [Portal Otvorenih Podataka](https://data.gov.rs/sr/datasets/kontsentratsije-polena-u-vazdukhu/). 

## Usage
Just run WebApp/app.R and you should be ready to go. In case you want to get the newest data run getData.R.   
Markers are not drawn for dates on which there is no data for. If you happen to pick such a date, pick a different one (ex. 2019-11-01).

You can also see the app in action at: <https://floatingduck.shinyapps.io/PolenNaDan/>
