library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)

categories <- read.csv("categories.csv", stringsAsFactors = FALSE)
types <- pull(categories, type)
names(types) <- pull(categories, label)
types <- sort(types)
places <- read.csv("data/places.csv")

load("grid100m_category_distances.RData")
load("warsaw_100m.RData")

add_km_coordinates = function(df, lat_mean = 52.22922, lng_mean = 21.04778){
    m_x = 111132.954 - 559.822 * cos(2 * lat_mean * pi / 180) + 1.175 * cos(4 * lat_mean * pi / 180)
    m_y = 111132.954 * cos(lat_mean * pi / 180)
    
    x = df$lat * m_x
    y = df$lng * m_y
    
    x = x - lat_mean * m_x
    y = y - lng_mean * m_y
    
    df$x = x
    df$y = y
    
    df
}

calc_criteria_score_linear = function(grid100m_category_distances, criteria){
    grid100m_category_distances %>%
        inner_join(criteria, by = 'category') %>%
        mutate(criteria_score = -distance * weight) %>%
        group_by(lng, lat, district, x, y) %>%
        summarise(criteria_score = sum(criteria_score)) %>%
        ungroup()  %>%
        mutate(criteria_score_norm = (criteria_score - min(criteria_score) + 0.000001) / (max(criteria_score) - min(criteria_score) + 0.000001))
}


calc_criteria_score_hyperbolic = function(grid100m_category_distances, criteria, scaling_constant = 10000){
    grid100m_category_distances %>%
        inner_join(criteria, by = 'category') %>%
        mutate(criteria_score = scaling_constant / (scaling_constant + distance) * weight) %>%
        group_by(lng, lat, district, x, y) %>%
        summarise(criteria_score = sum(criteria_score)) %>%
        ungroup()  %>%
        mutate(criteria_score_norm = (criteria_score - min(criteria_score) + 0.000001) / (max(criteria_score) - min(criteria_score) + 0.000001))
}


calc_points_score_linear = function(warsaw_100m, important_points){
    important_points = add_km_coordinates(important_points)
    important_points = important_points %>%
        mutate(point_id = row_number()) %>%
        rename(
            point_lng = lng,
            point_lat = lat,
            point_x = x,
            point_y = y
        )
    
    warsaw_100m %>%
        crossing(important_points) %>%
        mutate(
            distance = sqrt((x - point_x)^2 + (y - point_y)^2),
            points_score = -distance
        ) %>%
        group_by(lng, lat, district, x, y) %>%
        summarise(points_score = sum(points_score)) %>%
        ungroup()  %>%
        mutate(points_score_norm = (points_score - min(points_score) + 0.000001) / (max(points_score) - min(points_score) + 0.000001))
}



calc_points_score_hyperbolic = function(warsaw_100m, important_points, scaling_constant = 10000){
    important_points = add_km_coordinates(important_points)
    important_points = important_points %>%
        mutate(point_id = row_number()) %>%
        rename(
            point_lng = lng,
            point_lat = lat,
            point_x = x,
            point_y = y
        )
    
    warsaw_100m %>%
        crossing(important_points) %>%
        mutate(
            distance = sqrt((x - point_x)^2 + (y - point_y)^2),
            points_score = scaling_constant / (scaling_constant + distance)
        ) %>%
        group_by(lng, lat, district, x, y) %>%
        summarise(points_score = sum(points_score)) %>%
        ungroup()  %>%
        mutate(points_score_norm = (points_score - min(points_score) + 0.000001) / (max(points_score) - min(points_score)+ 0.000001))
}



calc_score_linear = function(grid100m_category_distances, warsaw_100m, criteria, important_points){
    criteria_score = calc_criteria_score_linear(grid100m_category_distances, criteria)
    
    if(nrow(important_points) >= 1){
        points_score = calc_points_score_linear(warsaw_100m, important_points)
        return(
            criteria_score %>%
                inner_join(points_score, by = c("lng", "lat", "district", "x", "y")) %>%
                mutate(
                    score = criteria_score_norm + points_score_norm,
                    score_norm = (score - min(score) + 0.000001) / (max(score) - min(score) + 0.000001)
                )
        )
    } else {
        return(
            criteria_score %>%
                rename(score = criteria_score, score_norm = criteria_score_norm)
        )
    }
}



calc_score_hyperbolic = function(grid100m_category_distances, warsaw_100m, criteria, important_points, scaling_constant = 10000){
    criteria_score = calc_criteria_score_hyperbolic(grid100m_category_distances, criteria, scaling_constant = scaling_constant)
    
    if(nrow(important_points) >= 1){
        points_score = calc_points_score_hyperbolic(warsaw_100m, important_points, scaling_constant = scaling_constant)
        return(
            criteria_score %>%
                inner_join(points_score, by = c("lng", "lat", "district", "x", "y")) %>%
                mutate(
                    score = criteria_score_norm + points_score_norm,
                    score_norm = (score - min(score) + 0.000001) / (max(score) - min(score) + 0.000001)
                )
        )
    } else {
        return(
            criteria_score %>%
                rename(score = criteria_score, score_norm = criteria_score_norm)
        )
    }
}


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
         point <- input$map_click
         if (is.null(point)) {
             important_points <- tibble()
         } else {
             important_points <- tibble(
                 lng = point$lng,
                 lat = point$lat
             )
         }
         crit <- criteria()
         if (nrow(crit) > 0) {
             score <- calc_score_hyperbolic(grid100m_category_distances, warsaw_100m, crit, important_points)   
             glimpse(score)
         }
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


