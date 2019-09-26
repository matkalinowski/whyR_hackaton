categories <- data.table(
  category = 'Health_care',
  type = c("dentist",
           "doctor",
           "hospital",
           "pharmacy",
           "veterinary_care")
) %>%
  rbind(data.table(category = 'Welness',
                   type = c("beauty_salon",
                            "gym"))) %>%
  rbind(data.table(category = 'Leisure',
                   type = c("park",
                            "cafe"))) %>%
  rbind(data.table(
    category = 'Shops',
    type = c("bakery",
             "supermarket",
             "store",
             "furniture_store")
  )) %>%
  rbind(data.table(category = 'Going_out',
                   type = c("bar",
                            "night_club"))) %>%
  rbind(data.table(
    category = 'Offices',
    type = c("city_hall",
             "post_office")
  )) %>%
  rbind(data.table(category = 'Schools',
                   type = c("school")))
write.table(categories,
            "./app_data/categories.csv",
            row.names = F,
            sep = ",")