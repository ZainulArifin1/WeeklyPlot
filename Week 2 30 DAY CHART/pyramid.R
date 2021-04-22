library(tidyverse)
library(ggpol)
library(gganimate)
library(gifski)

options(scipen = 999)

#Data input and transformation
US <- read_csv("US-Pyramid.csv")

US_pop <- US %>% 
  na.omit %>% 
  mutate(Age = substr(`Series Name`, 17, 21),
         Age = as.factor(case_when(Age == '80 an' ~ '80+', TRUE ~ Age)),
         Gender = as.factor(ifelse(str_detect(`Series Name`, 'female$') == TRUE, 'Female', 'Male')),
         Gender = factor(Gender, levels = c('Male', 'Female'))) %>% 
  select(-`Country Name`, -`Series Name`) %>% 
  gather(Year, Pop, `1960`:`2019`) %>% 
  mutate(Year = as.integer(substr(Year, 1, 4)),
         Pop = ifelse(Gender == 'Male', as.integer(Pop * -1), as.integer(Pop)))

#Static Plot
PopPyramid <- US_pop %>%
  ggplot(aes(x = Age, y = Pop, fill = Gender)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('#ff7129', '#d3b4d0')) +
  scale_y_continuous(
    breaks = c(-3000000, -6000000, -9000000, 0, 3000000, 6000000, 9000000),
    label = c("3M", "6M", "9M", "0", "3M", "6M", "9M")
  ) +
  coord_flip() + 
  theme(
    legend.position = "bottom",
    plot.background = element_rect(fill = "#242422") ,
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    axis.text = element_text(size = 14),
    legend.key.size = unit(0.75, 'cm'),
    legend.background = element_blank(),
    legend.text = element_text(
      size = 15,
      face = 'bold',
      color = 'white'
    ),
    plot.title = element_text(
      size = 22,
      hjust = 0.5,
      face = 'bold',
      color = 'white'
    ),
    plot.subtitle = element_text(
      size = 8,
      hjust = 0,
      face = 'bold',
      color = 'white'
    ),
    axis.title.x = element_text(
      size = 16,
      face = 'bold',
      color = '#242422'
    ),
    axis.text.x = element_text(
      size = 16,
      face = 'bold',
      color = 'white'
    ),
    axis.text.y = element_text(
      size = 10,
      face = 'bold',
      color = 'white'
    ),
    plot.caption = element_text(
      size = 8,
      hjust = 1,
      face = 'bold',
      color = '#f5a11d'
    ),
    plot.margin = margin(10,10,10,10)
        )


PopPyramid <- PopPyramid + 
  labs(
    title = 'USA Population Change (1960-2020)\n\n{closest_state}',
    subtitle = '',
    y = '\n\nPopulation',
    caption = '@SaintZainn | #30DayChartChallenge\n\nData Source: https://databank.worldbank.org'
  )


PopPyramid <- PopPyramid + 
  transition_states(
    Year,
    transition_length = 1,
    state_length = 2
  ) + 
  enter_fade() +
  exit_fade() + 
  ease_aes('cubic-in-out')

animate(
  PopPyramid,
  fps = 40,
  duration = 7,
  width = 1200,
  height = 1400,
  res = 120,
  renderer = gifski_renderer('PopPyramid4.gif')
)
