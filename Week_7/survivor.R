# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(ggthemes)
library("rnaturalearth")
library("rnaturalearthdata")


# Input Data -------------------------------------------------------------
summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
world <- ne_countries(scale = "medium", returnclass = "sf")

# Data Wrangling ----------------------------------------------------------

country_count <- summary %>%
  select(country) %>%
  group_by(country) %>%
  count() %>%
  ungroup() %>%
  rename(admin = country)

country_count[country_count$admin=="Polynesia", "admin"] <- "French Polynesia"  
country_count[country_count$admin=="Islands", "admin"] <- "Cook Islands"  

country_mapped <- left_join(world, country_count)
country_mapped$n <- as.factor(country_mapped$n)

# Draw Map ----------------------------------------------------------------

mytheme <- theme(text = element_text(family = 'mono')
                 ,panel.grid.major = element_line(color = '#cccccc' 
                                                  ,linetype = 'blank'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.background = element_rect(fill = 'aliceblue')
                 ,legend.background = element_rect(fill = 'aliceblue')
                 ,legend.key.size = unit(0.3, "cm")
                 ,legend.key.width = unit(0.3,"cm")
                 ,legend.position = "right"
                 ,plot.title = element_text(size = 32,hjust = 0.5)
                 ,plot.subtitle = element_text(size = 14, hjust = 0.5)
                 ,plot.caption = element_text(hjust = 0.1)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10)
)


map_plot <-
  ggplot(data = country_mapped) +
  geom_sf(aes(fill = n)) +
  scale_fill_brewer(palette = "Accent",na.value="#E8E8E8") +
  ggtitle("Survivor TV-Series Filming Location", subtitle = paste0(length(unique(summary$country)), " countries from 2000-2020")) +
  labs(
    fill = "Number of Films Shooting",
    caption = "@SaintZainn | TidyTuesday"
  ) +
  mytheme

agg_png(filename = "result.png", width = 32, height = 17.5, units = 'cm', res = 450)
map_plot
dev.off()
