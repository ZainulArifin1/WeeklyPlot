# Libraries ---------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(ggthemes)
library(extrafont)
library(cowplot)
library(magick)

# Data Input and Wrangling ------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-15')
tweets <- tuesdata$tweets

tweets <- tweets %>%
  filter(!is.na(verified))

tweets <- tweets %>%
  mutate(verified = replace(verified, verified == "FALSE", "Not Verified"),
         verified = replace(verified, verified == "TRUE", "Verified")
  )


# DataViz -----------------------------------------------------------------

plot1 <-
  ggplot(data = tweets, mapping = aes(x = datetime, y = like_count, color = verified)) +
  geom_point(mapping = aes(size = followers), alpha = 0.9) +
  scale_color_manual(values = c("black","#bf111d")) +
  guides(size = F) +
  labs(
    x = "MONTH",
    y = "# OF LIKES",
    title = "#DuBoisChallenge Revisited",
    caption = "@SaintZainn | #TidyTuesday | Source: #DuBoisChallenge Tweets"
  ) +
  theme_economist() + 
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank() ,
        axis.text = element_text(size = 30, family = "serif"),
        axis.title = element_text(size = 30, family = "serif", margin = margin(r = 10)),
        plot.title = element_text(hjust = 0.5, family = "serif", size = 60, lineheight = unit(0.3, "mm")),
        plot.caption = element_text(family = "serif", size = 30, hjust = 1, lineheight = unit(0.5, "mm")),
        legend.position = "top",
        legend.title = element_blank() ,
        legend.text = element_text(size = 30, family = "serif")
  ) 

p_magick <- image_graph(width = 2200, height = 1200, res = 300, bg = "transparent")
plot1
dev.off()  

back_img <- image_read("paper_texture.png")
back_img <- image_scale(back_img, geometry = geometry_size_percent(400, 300))
p2 <- image_composite(back_img, p_magick, 
                      offset = geometry_point(210,190))

image_write(p2, "plot1.png")
