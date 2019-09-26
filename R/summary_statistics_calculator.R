library(rgdal)
library(rgeos)

library(data.table)
library(magrittr)

districs <-
  rgdal::readOGR("app_data/geoJSON_shapes/warszawa-dzielnice.geojson", encoding = 'UTF-8', use_iconv = TRUE)

districs@polygons[[1]] <- NULL
districs@data <- districs@data[-1, ]

whole_places_table_raw <- fread('./data/places.csv', encoding = 'UTF-8') 

results_in_whole <-
  whole_places_table_raw[, .(mean_in_warsaw = round(mean(rating, na.rm = TRUE), 2),
                         warsaw__quant_25 = round(quantile(rating, .25, na.rm = TRUE), 2),
                         warsaw__quant_75 = round(quantile(rating, .75, na.rm = TRUE), 2)), by = type]

whole_places_table <- copy(whole_places_table_raw)

sp::coordinates(whole_places_table) <- ~lng+lat
sp::proj4string(whole_places_table) <- sp::proj4string(districs)

# tst <- rgeos::gWithin(whole_places_table, districs, byid = TRUE)
all_places_by_district <- sp::over(whole_places_table, districs, byid = TRUE)

whole_places_table <- cbind(whole_places_table_raw, all_places_by_district)

districts_statistics <-
  whole_places_table[, .(
    mean_in_district = round(mean(rating, na.rm = TRUE), 2),
    district_quant_25 = round(quantile(rating, .25, na.rm = TRUE), 2),
    district_quant_75 = round(quantile(rating, .75, na.rm = TRUE), 2)
  ), by = .(type, cartodb_id)]


get_stats_for_point <- function(point_coords) {
  sp::coordinates(point_coords) <- ~lng+lat
  sp::proj4string(point_coords) <- sp::proj4string(districs)
  
  sp::over(point_coords, districs, byid = TRUE)
}


#' @param places_table of filtered data of places to seach for.
#' 
get_categorical_summary_statistics <- function(places_table, point_coords) {
  results_in_radius <-
    places_table[, .(mean_in_radius = round(mean(rating, na.rm = TRUE), 2),
                     radius_quant_25 = round(quantile(rating, .25, na.rm = TRUE), 2),
                     radius_quant_75 = round(quantile(rating, .75, na.rm = TRUE), 2)), by = type]
  
  curr_district_stats <- districts_statistics[cartodb_id == get_stats_for_point(point_coords)$cartodb_id]
  return(
    merge(results_in_whole, results_in_radius, by = 'type', all.x = TRUE) %>% 
      merge(curr_district_stats, by = 'type', all.x = TRUE)
  )
}


# point_coords <- data.table(lng = mean(whole_places_table$lng), lat = mean(whole_places_table$lat))
get_categorical_summary_statistics(head(whole_places_table, 100), point_coords = point_coords)


# calculate means in warsaw
# get shapes of districts in warsaw
# assign point to shape of district 
# calculate statistics for shapes
