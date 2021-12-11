# Libraries and Data input ------------------------------------------------
library(tidyverse)
library(ggstream)
library(scales)
library(ragg)

sysfonts::font_add_google('Bebas Neue')
showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-12-07')

spiders <- tuesdata$spiders

# Data cleansing ----------------------------------------------------------

spiders_cleaned <- spiders %>%
  tidyr::unite("Species",species,genus, sep = " ") %>%
  select(speciesId, family, Species, year) %>%
  distinct(Species, .keep_all = T)

top_7_family <- data.frame(table(spiders_cleaned$family)) %>%
  filter(Freq > 2000)

spiders_cleaned <- spiders_cleaned %>%
  filter(family %in% as.array(top_7_family$Var1)) 

to_plot <- spiders_cleaned %>%
  group_by(year) %>%
  count(family)


# Plotting ----------------------------------------------------------------
font <- "Bebas Neue"

res <- 
  ggplot(data = to_plot) +
  geom_stream(aes(x=year, y=n, fill=family), type="mirror") +
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "SPIDER DISCOVERY",
    subtitle = "Each color represent different spider family that was identified from 1800 to 2020",
    caption = "@SaintZainn | #TidyTuesday | Worlds Spiders Database"
  )+
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(family = font, size = 200, colour = "white", hjust = 0.5, margin = margin(t = 20, b= 10)),
    plot.subtitle = element_text(family = font, size = 50, colour = "white", hjust = 0.5, margin = margin(t = 20, b= 60)),
    plot.caption = element_text(family = font, size = 45, colour = "yellow", margin = margin(t = 60)),
    panel.grid = element_blank(),
    axis.text.y  = element_blank(),
    axis.text.x = element_text(family = font, size = 65, colour = "white"),
    legend.position = "top",
    legend.title = element_text(family = font, size = 5, colour = "black"),
    legend.text  = element_text(family = font, size = 55, colour = "white")
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_x_continuous(limits = c(1800, 2020))

agg_png(filename = paste0("res.png"), width = 65, height = 35, units = 'cm', res = 180)
print(res)
dev.off()
