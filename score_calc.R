library(tidyverse)

load("app_data/grid100m_category_distances.RData")


criteria = tibble(
  category = c("bakery", "gym", "school"),
  weight = c(1, 0, -1)
)


grid100m_category_distances %>%
  inner_join(criteria, by = 'category') %>%
  mutate(score = distance * weight) %>%
  group_by(lng, lat, district, x, y) %>%
  summarise(score = sum(score))
  
