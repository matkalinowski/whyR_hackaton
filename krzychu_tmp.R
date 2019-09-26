library(ggplot2)

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
  geom_point(aes(x, y, colour = score))
