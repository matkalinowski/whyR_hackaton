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
            addScaleBar('bottomleft') %>%
            setView(21.0122, 52.2297, 11) %>%
            addProviderTiles(provider = providers$OpenStreetMap,group="OpenStreetMap") %>%
            addProviderTiles(provider = providers$CartoDB,group="CartoDB") %>%
            addProviderTiles(provider = providers$Esri.WorldTopoMap,group="Esri.WorldTopoMap")  %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "CartoDB","Esri.WorldTopoMap"),
                options = layersControlOptions(collapsed = T,position="topleft")
         )
    })
}

shinyApp(ui, server)


