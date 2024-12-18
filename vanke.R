library(ggplot2)
library(animint2)

set.seed(42)

vanke1127 <- data.frame(
  price = round(runif(200, 11.2, 11.8), 2),
  time = as.POSIXct("2009-11-27 09:00:00") + sort(sample(1:(7 * 60 * 60), 200))
)

price.ani_ag <- function(price, time, time.begin = min(time), span = 15 * 60, 
                         xlab = 'price', ylab = 'frequency', xlim, ylim, main) {
  time1 <- time.begin
  miss.main <- missing(main)
  tab.max <- 0

  while (time1 < max(time)) {
    time2 <- time1 + span
    sub.price <- price[time >= time1 & time <= time2]
    if (length(sub.price) > 0) {
      tab.max <- max(tab.max, max(table(sub.price)))
    }
    time1 <- time2
  }

  if (missing(xlim)) {
    xlimit <- range(price)
  }
  if (missing(ylim)) {
    ylimit <- c(0, tab.max)
  }

  time1 <- time.begin
  i <- 1
  full_dat <- data.frame()
  time_data <- data.frame()
  while (time1 < max(time)) {
    time2 <- time1 + span
    sub.price <- price[time >= time1 & time <= time2]
    if (length(sub.price) > 0) {
      tab.price <- table(sub.price)
      count <- length(tab.price)
      sub.iteration <- vector(length = count, mode = "numeric")
      sub.iteration <- sub.iteration + 1
      temp_df <- data.frame(sub.price = as.numeric(names(tab.price)), 
                            Freq = as.numeric(tab.price), 
                            iteration = sub.iteration * i)
      full_dat <- rbind(full_dat, temp_df)
      if (miss.main) {
        main <- paste(time1, time2, sep = ' - ')
        temp_df_name <- data.frame(main, iteration = i)
        time_data <- rbind(time_data, temp_df_name)
      }
      i <- i + 1
    }
    time1 <- time2
  }
  invisible(list(full_data = full_dat, dur_data = time_data, xl = xlimit, yl = ylimit))
}

result <- price.ani_ag(vanke1127$price, vanke1127$time)
full_data <- result$full_data
time_data <- result$dur_data

xlimit <- c(11.2, 11.8)
ylimit <- c(0, 5)

mid_x <- mean(xlimit)

gg <- ggplot(full_data) +
  geom_point(aes(x = sub.price, y = Freq), color = "blue", size = 2, showSelected = "iteration") + 
  geom_segment(aes(x = sub.price, xend = sub.price, y = Freq, yend = 0), color = "blue", 
               size = 0.7, showSelected = "iteration") + 
  geom_text(data = time_data, aes(x = mid_x, y = 5.5, label = main), 
            size = 5, color = "darkred", fontface = "bold", showSelected = "iteration") + 
  scale_x_continuous(
    breaks = seq(11.2, 11.8, 0.1),
    limits = xlimit
  ) + 
  scale_y_continuous(
    breaks = seq(0, 5, 0.5),
    limits = c(0, 5)
  ) +
  labs(
    title = "Frequency of Stock Prices",
    x = "Price",
    y = "Frequency"
  )

viz <- list(
  plot = gg,
  time = list(variable = "iteration", ms = 1000),
  title = "Frequency of Stock Prices"
)

animint2dir(viz, out.dir = "Stock_price_of_vanke")
