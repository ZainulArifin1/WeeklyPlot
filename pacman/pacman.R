library(tidyverse)


for (i in 0:20){
  df <- data.frame(
    variable = c("", "Pac-Man"),
    value = c(20-i, 80+i)
  )
  
  move <- i/50
  
  p <-
    ggplot(df, aes(x = "", y = value, fill = variable)) +
    geom_col(width = 1) +
    scale_fill_manual(values = c("#101920", "#ffe715")) +
    coord_polar("y", start = pi / (1.55+move) ) +
    labs(
    x = "",
    y = "",
    title = "Pac-Man?"
    ) +
    theme(
    plot.background = element_rect("#101920"),
      panel.background = element_rect("#101920"),
      legend.background = element_rect("#101920"),
      legend.key = element_rect("#101920"),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      text = element_text(colour = "#101920"),
      plot.margin = margin(rep(20,4)),
      legend.text = element_text(colour = "white", size = 20, family = "Architects Daughter"),
      plot.title = element_text(colour = "#ffe715", size = 40, hjust = 0.7, family = "Architects Daughter")
    )
  
  ggsave(paste0("pac",i,".png"), p, width = 30, height = 22, unit = 'cm', dpi = 100)
}

#Gifs are created https://gifmaker.me/ from the produced PNGS