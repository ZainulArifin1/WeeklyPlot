# Libraries ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(ggthemes)
library(extrafont)
library(cowplot)
library(lubridate)
library(hms)

sysfonts::font_add_google('Delius')
showtext::showtext_auto()

# Data Input and Wrangling ------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-29')
tuesdata <- tidytuesdayR::tt_load(2021, week = 27)

animal_rescues <- tuesdata$animal_rescues

animal_rescues$date_time_of_call <- as.POSIXlt(animal_rescues$date_time_of_call,format="%d/%m/%Y %H:%M",tz=Sys.timezone())

animal_rescues <- animal_rescues %>%
  mutate(
    hour_minute = date_time_of_call
  )
animal_rescues$hour_minute <- as_hms(animal_rescues$hour_minute)


# viz ---------------------------------------------------------------------

pets <- animal_rescues %>%
  filter(animal_group_parent %in% c("Cat","Bird","Dog"))

pet_plot <-
  ggplot(data = pets, mapping = aes(x = hour_minute, fill = animal_group_parent)) +
  geom_density(adjust = 1/6, show.legend = F) +
  scale_fill_brewer(palette = "Set1") +
  facet_grid(rows = vars(animal_group_parent), switch = "y") +
  labs(
    x = "Time of the Day",
    y = "",
    title = "At What Time Of The Day You Can Find Animals In Trouble?",
    subtitle = paste(
      strwrap(
        "Our animal friends often get themselves into trouble throughout the day till night
    There is also time when they need us in the middle of the night as seen when the density peaks from 1 to 2 o'clock",
        width = 110), collapse = "\n" ),
    caption = "@SaintZainn | #TidyTuesday"
  ) +
  theme(
    plot.margin = margin(15,5,0,15),
    axis.text.x = element_text(family = "Delius", size = 30),
    axis.title.x = element_text(family = "Delius", size = 30),
    axis.text.y = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.spacing.y = unit(0, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(family = "Delius", size = 30),
    plot.title = element_text(family = "Delius", hjust = 0.5, size = 50, margin = margin(b = 20, t = 10)),
    plot.subtitle  = element_text(family = "Delius", hjust = 0.5, size = 25, margin = margin(b = 20), color = "grey10"),
    plot.caption = element_text(family = "Delius", hjust = 1, size = 22)
  ) +
  scale_x_time(breaks = breaks_width("4 hour"))

agg_png(filename = "result.png", width = 32, height = 17.5, units = 'cm', res = 200)
pet_plot
dev.off()