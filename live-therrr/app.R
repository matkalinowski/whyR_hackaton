library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(leaflet)

ui <- dashboardPage(
    dashboardHeader(title = "Live theRRR!"),
    dashboardSidebar(),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css",
                      href = paste0("main.css?", as.numeric(Sys.time())))
        ),
        leafletOutput("map")
    )
)

server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet() %>%
            leaflet::addScaleBar('bottomleft') %>%
            setView(21.0122, 52.2297, 11) %>%
            addProviderTiles(provider = providers$OpenStreetMap)
    })
}

shinyApp(ui, server)
