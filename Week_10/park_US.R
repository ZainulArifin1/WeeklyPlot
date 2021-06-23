# Libraries ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(cowplot)
library(gifski)
library(shadowtext)
library(gganimate)

sysfonts::font_add_google('Bebas Neue')
showtext::showtext_auto()

# Data Input and Wrangling ------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 26)
parks <- tuesdata$parks

parks$spend_per_resident_data <- as.numeric(gsub("\\$", "", parks$spend_per_resident_data))

top_20 <- parks %>%
  filter(city %in% c("New York","Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio",
                     "San Diego", "Dallas", "San Jose", "Austin", "Jacksonville", "Fort Worth", "San Francisco",
                     "Columbus","Charlotte", "Indianapolis", "Seattle", "Denver", "Washington, D.C."))

top_20 <- top_20[!(is.na(top_20$total_pct)), ]

# Viz ---------------------------------------------------------------------

parks_plot <- 
  top_20 %>%
  ggplot(aes(x=total_pct, y=spend_per_resident_data, color=city)) +
  geom_point(show.legend = F) +
  geom_text(aes(label=city),position = position_dodge(width = 3), show.legend = F, size = 5, hjust = 0.5, vjust = -0.5) +
  theme_solarized_2(light = FALSE) +
  labs(title = "RELATION BETWEEN RESIDENCES SPENDING AND PARK POINTS \nAWARDED IN TOP 20 MOST POPULOUS US CITIES (2012 - 2020)",
       caption = "@SaintZainn | #TidyTuesday",
       y = "Spending per Resident in USD",
       x = "City Parks Points (0 - 100)") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_shadowtext(colour = 'firebrick', family = 'Bebas Neue',
                                        size = 24, hjust = 0.5, face = "bold"),
        plot.subtitle = element_shadowtext(colour = 'firebrick', family = 'Bebas Neue',
                                           size = 20, hjust = 0, face = "bold"),
        plot.margin = margin(10,10,10,10)
  ) +
  scale_x_continuous(limits = c(0,100) )

  
graph2.animation <- parks_plot +
  transition_time(year) + 
  labs(subtitle = "Year:{round(frame_time, 0)}") +
  view_follow(fixed_y = T, fixed_x = T) 


animate(graph2.animation, height = 800, width = 1600, fps = 47, duration = 5,
        end_pause = 60, res = 100, renderer = gifski_renderer())
anim_save("parks3.gif")
