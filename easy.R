library(ggplot2)
library(animint2)

data <- data.frame(
  Economy = c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", 
             "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
             "Bahamas, The", "Bahrain", "Bangladesh", "Barbados", "Belarus", 
             "Belgium", "Belize", "Benin", "Bermuda", "Bhutan"),
  Year = rep(2022, 20),
  Fertility_rate_total_births_per_woman = c(4.523, 1.376, 2.829, 5.209, 1.58, 
                                         1.876, 1.575, 1.63, 1.41, 1.67, 
                                         1.38, 1.797, 1.95, 1.634, 1.495, 
                                         1.53, 1.989, 4.895, 1.3, 1.398)
)

my_plot <- list(
  pointPlot = ggplot(data, aes(x = reorder(Economy, Fertility_rate_total_births_per_woman), 
                               y = Fertility_rate_total_births_per_woman, fill = Economy)) +
    geom_bar(stat = "identity", aes(color = Economy), size = 3, show.legend = FALSE) +
    labs(
      title = "Fertility Rate by Country (2022)",
      x = "Economy",
      y = "Fertility Rate (births per woman)"
    ) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
)

animint2dir(my_plot, "fertility_rate_output")
