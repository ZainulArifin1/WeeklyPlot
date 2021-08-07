# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)

sysfonts::font_add_google('Cairo')
showtext::showtext_auto()

# Data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-03')
tuesdata <- tidytuesdayR::tt_load(2021, week = 32)

athletes <- tuesdata$athletes

# EDA ---------------------------------------------------------------------
total_medals <- athletes %>%
  group_by(abb) %>%
  count(medal) %>%
  arrange(desc(n)) %>%
  ungroup() #Top countries are USA, China, and GBR

top_3 <- total_medals %>%
  filter(abb %in% c("USA","CHN","GBR")) 

top_3$abb <-
  dplyr::case_when(
    top_3$abb == "CHN" ~ "China",
    top_3$abb == "GBR" ~ "Great Britain",
    TRUE ~ "USA"
  )

top_3$abb  <-factor(top_3$abb, levels=c("USA","China","Great Britain"))
top_3$medal <- factor(top_3$medal, levels = c("Gold","Silver","Bronze"))

# Data Viz ----------------------------------------------------------------

res <-
  ggplot(data = top_3, aes(x = abb, y =n, fill = medal) ) +
  geom_bar(stat="identity", position=position_dodge(0.5), width = 0.5) +
  geom_text(
    aes(label = paste(n)),
    vjust = -1,
    fontface = "bold",
    family = "Cairo",
    position = position_dodge(0.5),
    size = 10
  ) +
  scale_fill_manual(values=c('gold','#c0c0c0','#cd7f32')) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = "Performance of Top Countries in Paralympics",
    caption = "@SaintZainn | #TidyTuesday"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Cairo", face = "bold", size = 33),
    legend.text = element_text(family = "Cairo", size = 33),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, family = "Cairo", size = 51),
    plot.caption = element_text(hjust = 1, family = "Cairo", size = 33)
  ) +
  ylim(0,860)
  


agg_png(filename = paste0("res.png"), width = 30, height = 35, units = 'cm', res = 180)
print(res)
dev.off()