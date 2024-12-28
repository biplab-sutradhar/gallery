library(testthat)
library(animint2)
library(XML)

f <- function(x) x^3 - 7 * x - 10

tolerance <- 1e-4
max_iterations <- 30

a <- -3
b <- 5

iterations_df <- tibble(
  iteration = numeric(0),
  a = numeric(0),
  b = numeric(0),
  c = numeric(0),
  f_c = numeric(0)
)

for (i in 1:max_iterations) {
  c <- (a + b) / 2
  f_a <- f(a)
  f_c <- f(c)

  iterations_df <- bind_rows(iterations_df, tibble(
    iteration = i,
    a = a,
    b = b,
    c = c,
    f_c = f_c
  ))

  if (abs(f_c) < tolerance || (b - a) / 2 < tolerance) {
    break
  }

  if (sign(f_a) * sign(f_c) < 0) {
    b <- c
  } else {
    a <- c
  }
}

x_vals <- seq(floor(min(iterations_df$a)), ceiling(max(iterations_df$b)), length.out = 500)
function_data_df <- tibble(x = x_vals, y = f(x_vals))

viz <- list(
  bisectionMethodPlot = ggplot() +
    geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
    geom_line(
      data = function_data_df, aes(x = x, y = y),
      color = "black", size = 1.2
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = a),
      linetype = "dashed", color = "red", alpha = 0.5,
      showSelected = "iteration"
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = b),
      linetype = "dashed", color = "red", alpha = 0.5,
      showSelected = "iteration"
    ) +
    geom_point(
      data = iterations_df, aes(x = c, y = 0),
      color = "blue", size = 3, showSelected = "iteration"
    ) +
    geom_vline(
      data = iterations_df, aes(xintercept = c),
      color = "blue", size = 1, showSelected = "iteration"
    ) +
    labs(
      title = "Bisection Method - Root Finding",
      subtitle = "Solving f(x) = x³ - 7x - 10",
      x = "x",
      y = "f(x)"
    ),
  time = list(variable = "iteration", ms = 1000)
)

map <- animint2HTML(viz)
html_content <- map$html

test_that("Data consistency in iterations_df", {
  expect_true(nrow(iterations_df) > 0, info = "No iterations were performed.")
  expect_true(all(iterations_df$f_c != 0), info = "f(c) should not be exactly zero except at the root.")
})

test_that("Function visualization correctness", {
  expect_true(nrow(function_data_df) > 0, info = "Function data should not be empty.")
  expect_true(all(!is.na(function_data_df$y)), info = "Function values should not contain NA.")
})

test_that("HTML content contains the correct plot elements", {
  expect_true(grepl("Bisection Method - Root Finding", saveXML(html_content)), 
              info = "Plot title not found in the HTML content.")
})

test_that("Root approximation is within tolerance", {
  final_root <- iterations_df$c[nrow(iterations_df)]
  expect_true(abs(f(final_root)) < tolerance, 
              info = paste("Root approximation error exceeds tolerance. Approximated root:", final_root))
})

test_that("Animation control buttons exist", {
  expect_true(grepl("Show animation controls", saveXML(html_content)), 
              info = "Animation control buttons not found in the HTML content.")
})
