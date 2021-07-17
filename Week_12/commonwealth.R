# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(ggtext)

sysfonts::font_add_google('Chivo')
showtext::showtext_auto()

# input -------------------------------------------------------------------
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

day_bar <- holidays %>%
  drop_na() %>%
  group_by(month) %>%
  count() %>%
  ungroup()

day_bar$month <- factor(day_bar$month, levels = rev(c("Jan","Feb","Mar", "Apr", "May", "Jun", 
                                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
'%ni%' <- Negate('%in%')

day_bar <- day_bar %>%
  mutate(
    color = case_when(
      month %in% c("Aug") ~ "brown2",
      month %in% c("Sep") ~ "mediumpurple1",
      month %in% c("Jul") ~ "gold1",
      month %ni% c("Jul", "Aug", "Sep") ~ "grey65"
    )
  )

month_ind <-
  ggplot(data = day_bar, mapping = aes(x = n, y = month, fill = color)) +
  geom_col() +
  geom_text(
    aes(label = paste(n,"COUNTRIES")),
    hjust = 1, 
    nudge_x = -0.2,
    fontface = "bold",
    family = "Chivo",
    size = 7
  ) +
  theme_minimal() +
  scale_x_continuous(expand = c(.01, .01), limits = c(0, 22)) +
  scale_fill_identity(guide = "none") +
  labs(
    caption = "@SaintZainn | #TidyTuesday"
  )+
  theme(axis.text.y = element_text(size = 30, hjust = 1, family = "Chivo", color = "black"),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 50, hjust = 1, family = "Chivo", color = "black", margin = margin(t = 20, b = -60)),
        plot.caption  = element_text(size = 20, hjust = 1, family = "Chivo", color = "#8c2414"),
        plot.margin = margin(rep(15, 4))) +
  ggtitle("COUNTRIES MONTH OF<br><span style ='color:#8c2414'>INDEPENDENCE</span>") 

agg_png(filename = "result.png", width = 32, height = 17.5, units = 'cm', res = 200)
month_ind
dev.off()