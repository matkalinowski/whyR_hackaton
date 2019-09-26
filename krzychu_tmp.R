library(ggplot2)
library(geosphere)

grid100m_category_distances %>%
  summarise(max(distance))

grid100m_category_distances %>%
  filter(category == 'school') %>%
  ggplot() +
  geom_point(aes(x, y, colour = distance))




profiles = read_csv("app_data/profiles.csv")

profiles = profiles %>%
  rename(
    category = type,
    weight = desired
  )

criteria = tibble(
  category = c("bakery", "gym", "school"),
  weight = c(1, 0, -1)
)

criteria_score = calc_criteria_score_linear(grid100m_category_distances, criteria)
criteria_score = calc_criteria_score_hyperbolic(grid100m_category_distances, criteria)

ggplot(criteria_score) +
  geom_point(aes(x, y, colour = criteria_score_norm))



criteria_score = calc_criteria_score_hyperbolic(
  grid100m_category_distances,
  profiles %>%
    filter(profile == 'young_parent')
)

ggplot(criteria_score) +
  geom_point(aes(x, y, colour = criteria_score_norm))







important_points = tibble(
  lng = c(21.1, 21.01),
  lat = c(52.1, 52.2)
)

important_points = tibble(
  lng = c(21.11),
  lat = c(52.15)
)

points_score = calc_points_score_linear(warsaw_100m, important_points)
points_score = calc_points_score_hyperbolic(warsaw_100m, important_points)


ggplot(points_score) +
  geom_point(aes(x, y, colour = points_score_norm))




criteria = profiles %>%
  filter(profile == 'student')



criteria = tibble(
  category = c("bakery", "gym", "school"),
  weight = c(1, 0, -1)
)

important_points = tibble(
  lng = c(21.11),
  lat = c(52.15)
)


score = calc_score_linear(grid100m_category_distances, warsaw_100m, criteria, tibble())
score = calc_score_linear(grid100m_category_distances, warsaw_100m, criteria, important_points)
score = calc_score_hyperbolic(grid100m_category_distances, warsaw_100m, criteria, tibble())
score = calc_score_hyperbolic(grid100m_category_distances, warsaw_100m, criteria, important_points)


ggplot(score) +
  geom_point(aes(x, y, colour = score_norm))







a = warsaw_100m[1,]
b = warsaw_100m[2888,]

distHaversine(a[c('lng','lat')], b[c('lng','lat')])

sqrt((a$x - b$x)^2 + (a$y - b$y)^2)

