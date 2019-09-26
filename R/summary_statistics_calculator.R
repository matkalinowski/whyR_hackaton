whole_places_table <- fread('./data/places.csv', encoding = 'UTF-8')

results_in_whole <-
  whole_places_table[, .(mean_in_warsaw = round(mean(rating, na.rm = TRUE), 2),
                         warsaw__quant_25 = round(quantile(rating, .25, na.rm = TRUE), 2),
                         warsaw__quant_75 = round(quantile(rating, .75, na.rm = TRUE), 2)), by = type]

#' @param places_table of filtered data of places to seach for.
#' 
get_categorical_summary_statistics <- function(places_table) {
  results_in_radius <-
    places_table[, .(mean_in_radius = round(mean(rating, na.rm = TRUE), 2),
                     radius_quant_25 = round(quantile(rating, .25, na.rm = TRUE), 2),
                     radius_quant_75 = round(quantile(rating, .75, na.rm = TRUE), 2)), by = type]
  return(
    merge(results_in_whole, results_in_radius, by = 'type', all.x = TRUE)
  )
}


get_categorical_summary_statistics(head(whole_places_table, 100))


# calculate means in warsaw
# get shapes of districts in warsaw
# assign point to shape of district 
# calculate statistics for shapes
