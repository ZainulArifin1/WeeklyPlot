library(gganimate)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(tidyr)
library(gifski)
library(shadowtext)

sysfonts::font_add_google('Bebas Neue')
showtext::showtext.auto()

netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

netflix_filtered <- netflix_titles %>%
  filter(country %in% c("United States","India","United Kingdom","Japan","South Korea"))

netflix_final <- netflix_filtered %>%
  select(country, release_year)

netflix_to_plot <- netflix_final %>%
  group_by(release_year) %>%
  count(country) %>%
  ungroup()

netflix_to_plot <- netflix_to_plot %>%
  filter(release_year >= 2010 & release_year <= 2020)


netflix_to_plot$release_year <- as.integer(netflix_to_plot$release_year)
netflix_to_plot$country <- as.factor(netflix_to_plot$country, levels = c("United States","India", "United Kingdom", "Japan", "South Korea"))

graph2 <- netflix_to_plot %>%
  ggplot(aes(x=release_year, y=n, color=country)) +
  geom_line(size = 3, alpha = 0.5) +
  theme_solarized_2(light = FALSE) +
  labs(title = "NETFLIX MOVIE AND TV-SHOW PRODUCTION OVER THE YEAR",
       y = "") +
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_shadowtext(colour = 'firebrick', family = 'Bebas Neue',
                                        size = 20, hjust = 0.5, face = "bold"),
        plot.margin = margin(10,10,10,10)
        ) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(name="Year", limits=c(2010, 2020), breaks = c(2010:2020)) +
  geom_point()


graph2.animation <- graph2 +
  transition_reveal(release_year) + 
  view_follow(fixed_y = T, fixed_x = T) 


animate(graph2.animation, height = 750, width = 1400, fps = 40, duration = 7,
        end_pause = 60, res = 100, renderer = gifski_renderer())
anim_save("netflix2.gif")