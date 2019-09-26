library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)

categories <- read.csv("categories.csv", stringsAsFactors = FALSE)
types <- pull(categories, type)
names(types) <- pull(categories, label)
types <- sort(types)

ui <- dashboardPage(
    dashboardHeader(
        title = "Live theRRR!",
        titleWidth = 350
    ),
    dashboardSidebar(
        sliderInput("radius", "Point radius", min = 100, max = 1000, value = 100, step = 50),
        checkboxGroupInput("categories", "Categories", choices = types),
        width = 350
    ),
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
            
            setView(21.0122, 52.2297, 11) %>%
            addProviderTiles(provider = providers$OpenStreetMap,group="OpenStreetMap") %>%
            addProviderTiles(provider = providers$CartoDB,group="CartoDB") %>%
            addProviderTiles(provider = providers$Esri.WorldTopoMap,group="Esri.WorldTopoMap")  %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap", "CartoDB","Esri.WorldTopoMap"),
                options = layersControlOptions(collapsed = T,position="topleft")
         )
    })
    
     observe({
         point <- input$map_click
         req(!is.null(point))
         leafletProxy("map") %>%
            addScaleBar('bottomleft') %>%
            clearShapes() %>%
            addCircles(lng=point$lng,lat=point$lat,radius=1000) #, weight = 1, color = "#777777", fillOpacity = 0.7
     })
}

shinyApp(ui, server)


