# Libraries and Data ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(Hmisc)
library(RColorBrewer)
library(ggdist)

sysfonts::font_add_google('Quicksand')
showtext::showtext_auto()

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


lemurs_unique <- lemurs %>%
  group_by(dlc_id) %>%
  slice(n()) %>%
  filter(sex != "ND")

lemurs_unique <- lemurs_unique %>% 
  mutate(sex = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female"
  ))

use_font <- "Quicksand"

raincloud_theme <- theme(
  text = element_text(size = 40),
  axis.title.x = element_text(size = 40, family = use_font, margin = margin(t = 20, b = 10)),
  axis.title.y = element_text(size = 40, family = use_font),
  axis.text = element_text(size = 40, family = use_font),
  axis.text.x = element_text(vjust = 0.5, family = use_font),
  axis.ticks = element_blank(),
  legend.title=element_text(size=40, family = use_font),
  legend.text=element_text(size=40, family = use_font),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 60, family = use_font, hjust = 0.5, margin = margin(b = 20)),
  plot.subtitle = element_text(lineheight=.8, face="bold", size = 33, family = use_font, hjust = 0.5),
  plot.caption = element_text(lineheight=.8, face="bold", size = 25, family = use_font),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank()
  )


# Viz ---------------------------------------------------------------------

res <-
  ggplot(data = lemurs_unique, aes(y = age_at_death_y, x = sex, fill = sex)) +
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA,
    show.legend = F
  ) + 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA,
    show.legend = F
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3,
    show.legend = F
  ) +
  coord_flip() +
  raincloud_theme +
  scale_fill_manual(values = c("#111820","#f3aa4e")) +
  labs(
    y = "Age of Lemurs at Death",
    x = "",
    title = "How long can a Lemur Live for?",
    subtitle = "Wild lemur are only expected to live on average 16 to 20 years but lemurs in captivity can live up to 30 years and even more!",
    caption = "@SaintZainn | #TidyTuesday | Source: Nature - Zehr et al, 2014"
  )


agg_png(filename = paste0("res.png"), width = 45, height = 25, units = 'cm', res = 180)
print(res)
dev.off()
