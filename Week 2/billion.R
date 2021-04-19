library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)

# 1 billion
parts <- data.frame(
  names = "10K",
  vals = c(10000)
)

p <- waffle(parts, rows = 100, colors = "black")
ggsave("1bil.jpg",p, units = "cm", width = 30, height = 31)