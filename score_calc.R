library(tidyverse)

load("app_data/grid100m_category_distances.RData")
load("app_data/warsaw_100m.RData")

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
    mutate(criteria_score_norm = (criteria_score - min(criteria_score) + 1) / (max(criteria_score) - min(criteria_score) + 1))
}

  
calc_criteria_score_hyperbolic = function(grid100m_category_distances, criteria, scaling_constant = 1000){
  grid100m_category_distances %>%
    inner_join(criteria, by = 'category') %>%
    mutate(criteria_score = scaling_constant / (scaling_constant + distance) * weight) %>%
    group_by(lng, lat, district, x, y) %>%
    summarise(criteria_score = -sum(criteria_score)) %>%
    ungroup()  %>%
    mutate(score_norm = (criteria_score - min(criteria_score) + 1) / (max(criteria_score) - min(criteria_score) + 1))
}


important_points = tibble(
  lng = c(21.1, 21.01),
  lat = c(52.1, 52.2)
)

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
  mutate(distance = sqrt((x - point_x)^2 + (y - point_y)^2)) %>%
  group_by()



