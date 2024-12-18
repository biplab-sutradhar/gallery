library(animint2)
library(ggplot2)

set.seed(123)

data <- data.frame(
  x = runif(10, min = 0, max = 10),
  y = runif(10, min = 0, max = 10),
  time = 1:10
)

data_cumulative <- data.frame()
for (i in 1:nrow(data)) {
  data_cumulative <- rbind(data_cumulative, data[1:i, ])
}

data_cumulative$step <- rep(1:nrow(data), times = 1:nrow(data))

viz <- list(
  cumulativePointsPlot = ggplot() +
    geom_point(aes(x = x, y = y), 
               data = data_cumulative, size = 4, color = "#000000", 
               showSelected = "step") +
    ggtitle("Cumulative Point Animation with Positions") +
    xlab("X-axis") + 
    ylab("Y-axis") +
    scale_x_continuous(
      limits = c(0, 10), 
      breaks = seq(0, 10, by = 1), 
      expand = c(0.05, 0.05)
    ) +
    scale_y_continuous(
      limits = c(0, 10), 
      breaks = seq(0, 10, by = 1), 
      expand = c(0.05, 0.05)
    ),
  time = list(variable = "step", ms = 1000)
)

animint2dir(viz, out.dir = "ani_record")
