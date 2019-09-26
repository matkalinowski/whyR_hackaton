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
        fluidRow(
            column(
                checkboxGroupInput("desired_categories", "Desired amenities", choices = types),
                width = 6
            ),
            column(
                checkboxGroupInput("undesired_categories", "Undesired amenities", choices = types),
                width = 6
            )
        ),
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
                width = 8
            ),
            column(
                dataTableOutput("ratings"),
                width = 4
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
        count(fp(),type)
    })

     observe({
         point <- input$map_click
         req(!is.null(point))
         leafletProxy("map") %>%
            clearShapes() %>%
            addCircles(lng=point$lng,lat=point$lat,radius=input$radius) #, weight = 1, color = "#777777", fillOpacity = 0.7
     })
     
     fp <- reactive({
         point <- input$map_click
         req(!is.null(point))
         
         fp <- places %>% filter(type %in% input$desired_categories) %>% 
             dplyr::select(name,rating,lat,lng,type)
         
         req(nrow(fp>0))
         fp <- fp %>% mutate(dist=geosphere::distHaversine(fp[,c("lng","lat")],c(point$lng, point$lat)),
                             distTF=dist<input$radius) %>% 
             filter(distTF)
         
         fp
     })
     
     criteria <- reactive({
         des <- input$desired_categories
         undes <- input$undesired_categories
         
         des_tibble <- tibble()
         undes_tibble <- tibble()
         
         if (!is.null(des)) {
             des_tibble <- tibble(
                 category = des,
                 weight = 1
             )
         }
         
         if (!is.null(undes)) {
             undes_tibble <- tibble(
                 category = undes,
                 weight = -1
             )
         }
         
         crit_tibble <- bind_rows(des_tibble, undes_tibble)
         
         return(crit_tibble)
     })
     
     observe({
         criteria()
     })
     
     observe({
         leafletProxy("map") %>%
             clearMarkers() %>%
             addAwesomeMarkers(data=fp(),label=~name, icon=makeAwesomeIcon(icon='circle', library='fa', markerColor=~col)) %>%
             fitBounds(lng1 = min(fp()$lng), 
                       lat1 = min(fp()$lat), 
                       lng2 = max(fp()$lng), 
                       lat2 = max(fp()$lat))
     })
     
}

shinyApp(ui, server)


