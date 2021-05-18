library(tidyverse)
library(cowplot)
library(here)
library(ragg)

# Read data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

# Summary of departure reasons
depart_reasons <- departures  %>%
  group_by(departure_code) %>%
  count() %>%
  ungroup() %>%
  na.omit() %>%
  slice_head(n = 6) %>%
  mutate(departure_code = recode(departure_code, 
                       "1" = "DEATH",
                       "2" = "ILLNESS",
                       "3" = "POOR \n PERFORMANCE",
                       "4" = "LEGAL \n VIOLATIONS",
                       "5" = "RETIREMENT",
                       "6" = "NEW \n OPPORTUNITY"
                       )
         )

# Plot 

bg.col <- "#003b46"
blue.dark <- "#06575b"
blue.light <- "#b9dbde"


plot_dept_reasons <- 
  ggplot(depart_reasons, aes(x = departure_code, y = n)) +
  geom_col(aes(fill = n), show.legend = F) +
  scale_fill_gradient(low = blue.dark, high = blue.light) +
  scale_y_log10() +
  geom_hline(yintercept = 10000, linetype = 2, color = blue.light) +
  geom_text(
    aes(y = 10000, label = departure_code),
    family = "Ubuntu",
    size = 4,
    color = blue.light) +
  coord_polar() +
  theme_void() +
  labs(
    subtitle = "S&P 1500",
    title = "The most common cause of CEOs departure in",
    caption = "@SaintZainn | Source: Gentry et al. | #TidyTuesday") +
  theme(
    plot.subtitle = element_text(
      family = "Source Sans Pro",
      face = "bold", 
      hjust = 0.5,
      size = 60,
      color = blue.light, 
      margin = margin(t = 5, b = 10)
    ), 
    plot.title = element_text(
      family = "Source Sans Pro",
      face = , 
      hjust = 0.5,
      size = 13,
      color = "white",
      margin = margin(t = 20, b = 5)
    ),
    plot.caption = element_text(
      family = "Source Sans Pro",
      face = , 
      hjust = 0.5,
      size = 8,
      color = "white",
      margin = margin(b = 2)
    ),
    plot.background = element_rect(fill = bg.col, color = NA),
    plot.margin = margin(5,5,5,5)
  )


agg_png(filename = "test_1500_2.png", width = 5, height = 7, units = 'in', res = 120)
plot_dept_reasons
dev.off()