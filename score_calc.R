library(tidyverse)

load("app_data/grid100m_category_distances.RData")

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
    mutate(score = -distance * weight) %>%
    group_by(lng, lat, district, x, y) %>%
    summarise(score = sum(score)) %>%
    ungroup()  %>%
    mutate(score_norm = (score - min(score) + 1) / (max(score) - min(score) + 1))
}

  
calc_criteria_score_hyperbolic = function(grid100m_category_distances, criteria, scaling_constant = 1000){
  grid100m_category_distances %>%
    inner_join(criteria, by = 'category') %>%
    mutate(score = scaling_constant / (scaling_constant + distance) * weight) %>%
    group_by(lng, lat, district, x, y) %>%
    summarise(score = -sum(score)) %>%
    ungroup()  %>%
    mutate(score_norm = (score - min(score) + 1) / (max(score) - min(score) + 1))
}


important_points = tibble(
  lng = c(21.1, 20.01),
  lat = c(52.1, 52.2)
)

important_points = add_km_coordinates(important_points)

