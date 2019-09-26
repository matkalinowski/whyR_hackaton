library(tidyverse)
library(RANN)

mean_lat = 52.22922

add_km_coordinates = function(df, lat_mean = mean(df$lat)){
  m_x = 111132.954 - 559.822 * cos(2 * lat_mean * pi / 180) + 1.175 * cos(4 * lat_mean * pi / 180)
  m_y = 111132.954 * cos(lat_mean * pi / 180)
  
  x = df$lat * m_x
  y = df$lng * m_y
  
  x = x - mean(x)
  y = y - mean(y)
  
  df$x = x
  df$y = y
  
  df
}


places = read_csv('data/places.csv')
warsaw_100m = read_tsv("data/warsaw_wgs84_every_100m.txt", col_names = c("lng", "lat", "district"))

warsaw_100m = warsaw_100m %>%
  mutate(
    district = case_when(
      district == "REMBERT├ôW" ~ "Rembertów",
      district == "OCHOTA" ~ "Ochota",
      district == "┼╗oliborz" ~ "Żoliborz",
      district == "WAWER" ~ "Wawer",
      district == "┼ÜR├ôDMIE┼ÜCIE" ~ "Śródmieście",
      district == "WOLA" ~ "Wola",
      district == "URSUS" ~ "Ursus",
      TRUE ~ district 
    )
  )

category_mappings = read_csv("app_data/categories.csv")
category_mappings = category_mappings %>%
  mutate(category = type)

# category_mappings = places %>%
#   group_by(type) %>%
#   tally() %>%
#   arrange(n) %>%
#   filter(n > 1000) %>%
#   mutate(category = type) %>%
#   select(-n)

places_with_categories = places %>%
  inner_join(category_mappings, by = 'type')

categories = unique(places_with_categories$category)





places_with_categories = add_km_coordinates(places_with_categories, lat_mean = mean_lat)
warsaw_100m = add_km_coordinates(warsaw_100m, lat_mean = mean_lat)


# save(places_with_categories, file = "app_data/places_with_categories.RData", compress = TRUE)
# save(warsaw_100m, file = "app_data/warsaw_100m.RData", compress = TRUE)


grid100m_category_distances = tibble()

for(ctg in categories){
  print(ctg)
  
  obj = places_with_categories %>%
    filter(category == ctg)
  
  closest_grid_to_obj = nn2(obj[,c('x', 'y')], warsaw_100m[,c('x', 'y')], k = 1, searchtype = "radius", radius = 30000)
  grid_category_dist = cbind(warsaw_100m, distance = closest_grid_to_obj$nn.dists, category = ctg)
  
  grid100m_category_distances = rbind(grid100m_category_distances, grid_category_dist)
}

grid100m_category_distances = as_tibble(grid100m_category_distances) %>%
  mutate(
    distance = pmin(distance, 4000),
    category = as.character(category)
  )

save(grid100m_category_distances, file = "app_data/grid100m_category_distances.RData", compress = TRUE)

