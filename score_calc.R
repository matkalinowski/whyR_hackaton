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






