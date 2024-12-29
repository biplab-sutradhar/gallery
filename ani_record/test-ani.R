library(testthat)
library(animint2)
library(XML)

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

map <- animint2HTML(viz)
html_content <- map$html

test_that("Data frame dimensions are consistent", {
  expect_equal(nrow(data_cumulative), sum(1:nrow(data)))
  expect_equal(ncol(data_cumulative), 4)
})

test_that("Cumulative steps are assigned correctly", {
  expect_equal(max(data_cumulative$step), nrow(data))
  expect_equal(min(data_cumulative$step), 1)
})

test_that("Visualization contains the correct elements", {
  expect_true(grepl("Cumulative Point Animation with Positions", saveXML(html_content)))
  expect_true(grepl("X-axis", saveXML(html_content)))
  expect_true(grepl("Y-axis", saveXML(html_content)))
})

test_that("Animation controls are present", {
  expect_true(grepl("Show animation controls", saveXML(html_content)))
})
