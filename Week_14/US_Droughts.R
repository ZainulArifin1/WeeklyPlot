# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(urbnmapr)
library(lubridate)

sysfonts::font_add_google('Nunito')
showtext::showtext_auto()

# Input Data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tuesdata$drought

drought$map_date2 <- ymd(drought$map_date)
drought$year <- year(drought$map_date2)

drought_max <- drought %>%
  filter(drought_lvl %in% c("D3","D4")) %>%
  filter(area_pct > 0) %>%
  group_by(year) %>%
  count(state_abb) %>%
  rename(state_abbv = state_abb)

drought_joined <- left_join(drought_max, counties, by = "state_abbv")
drought_joined_2020 <- drought_joined %>%
  filter(year %in% "2020")


household_data <- left_join(countydata, counties, by = "county_fips") 

files_download <- vector(mode = "list", length = 21)

fam <- "Nunito"

for (i in 1:length(files_download)){
  temp <- 2000 + i
  temp_df <- drought_joined %>%
    filter(year == temp)
  
  plot_n <-
    temp_df %>%
    ggplot(aes(long, lat, group = group, fill = n)) +
    geom_polygon(color = NA) +
    geom_polygon(data = states, mapping = aes(long, lat, group = group),
                 fill = NA, color = "grey32") +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient(
      guide = guide_colorbar(title.position = "top"),
      low = "gold1",
      high = "red", 
      limits = c(0,106)
    ) +
    theme(legend.title = element_text(),
          legend.key.width = unit(.5, "in"),
          legend.position = "top",
          legend.title.align = 0.5,
          plot.title = element_text(size = 40, hjust = 0.5, family = fam), 
          plot.subtitle = element_text(size = 30, hjust = 0.5, family = fam, margin = margin(t = 10)),
          plot.caption = element_text(size = 20, hjust = 1, family = fam),
          legend.text = element_text(size = 30),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()
    ) +
    labs(title = "Heavy Drought in US States by Occurences each year",
         subtitle = paste(unique(temp_df$year)),
         caption = "@SaintZainn | Drought Monitor | #TidyTuesday",
         fill = "")
  files_download[[i]] <- plot_n
}

for (i in 1:length(files_download)){
  agg_png(filename = paste0("year_",i,".png"), width = 35, height = 25, units = 'cm', res = 180)
  print(files_download[[i]])
  dev.off()
}


