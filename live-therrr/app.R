library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)

categories <- read.csv("categories.csv", stringsAsFactors = FALSE)
types <- pull(categories, type)
names(types) <- pull(categories, label)
types <- sort(types)
places <- read.csv("data/places.csv")


ui <- dashboardPage(
    dashboardHeader(
        title = "Live theRRR!",
        titleWidth = 350
    ),
    dashboardSidebar(
        sliderInput("radius", "Point radius", min = 1000, max = 10000, value = 100, step = 50),
        checkboxGroupInput("categories", "Categories", choices = types),
        width = 350
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css",
                      href = paste0("main.css?", as.numeric(Sys.time())))
        ),
        fluidRow(
            column(
                leafletOutput("map"),
                width = 6
            ),
            column(
                dataTableOutput("ratings"),
                width = 6
            )
        )
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
    
    output$ratings <- renderDataTable({
        categories
    })

     observe({
         point <- input$map_click
         req(!is.null(point))
         leafletProxy("map") %>%
            clearShapes() %>%
            addCircles(lng=point$lng,lat=point$lat,radius=input$radius) #, weight = 1, color = "#777777", fillOpacity = 0.7
     })
     
     observe({
         point <- input$map_click
         req(!is.null(point))
         
         testPoints <- data.frame(lng=c(21.014,21.1122,21.2122),lat=c(52.2297,52.2297,52.2297))
         filterdPoints <-  data.frame(testPoints,distTF=geosphere::distHaversine(testPoints,c(point$lng, point$lat))<input$radius) %>%
             mutate(col=ifelse(distTF,"red","blue"))
         
         leafletProxy("map") %>%
             clearMarkers() %>%
             addAwesomeMarkers(data=filterdPoints,icon=makeAwesomeIcon(icon='circle', library='fa', markerColor=~col))
     })
     
     observe({
         print(places %>% head)
     })
}

shinyApp(ui, server)


