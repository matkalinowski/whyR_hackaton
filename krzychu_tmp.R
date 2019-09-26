library(ggplot2)

grid100m_category_distances %>%
  summarise(max(distance))

grid100m_category_distances %>%
  filter(category == 'dentist') %>%
  ggplot() +
  geom_point(aes(x, y, colour = distance))

