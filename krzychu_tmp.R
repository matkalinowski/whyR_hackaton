library(ggplot2)
library(geosphere)

grid100m_category_distances %>%
  summarise(max(distance))

grid100m_category_distances %>%
  filter(category == 'school') %>%
  ggplot() +
  geom_point(aes(x, y, colour = distance))





criteria = tibble(
  category = c("bakery", "gym", "school"),
  weight = c(1, 0, -1)
)

score = calc_criteria_score_linear(grid100m_category_distances, criteria)
score = calc_criteria_score_hyperbolic(grid100m_category_distances, criteria)

ggplot(score) +
  geom_point(aes(x, y, colour = criteria_score))













a = warsaw_100m[1,]
b = warsaw_100m[2888,]

distHaversine(a[c('lng','lat')], b[c('lng','lat')])

sqrt((a$x - b$x)^2 + (a$y - b$y)^2)

