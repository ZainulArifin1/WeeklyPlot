# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(treemap)

sysfonts::font_add_google('Gugi')
showtext::showtext_auto()

# Input -------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

scoobydoo$monster_type <- gsub(",.*", "",scoobydoo$monster_type)
monster_type <- scoobydoo %>%
  select(monster_type) %>%
  group_by(monster_type) %>%
  count()

monster_type <- monster_type[!grepl("NULL", monster_type$monster_type),]
monster_type$monster_type <- paste0(monster_type$monster_type, "\n",monster_type$n) 


agg_png(filename = "result.png", width = 35, height = 17.5, units = 'cm', res = 180)
treemap(
  monster_type,
  index = "monster_type",
  vSize = "n",        
  type="index",
  algorithm = "squarified",
  
  # Main
  title="THE MOST COMMON MONSTER TYPE IN SCOOBY DOO!",
  palette="BrBG",
  fontfamily.title = "Gugi",
  fontsize.labels = 24,
  fontsize.title = 45,
  fontfamily.labels = "Gugi",
  
  # Borders:
  border.col=c("black"),             
  border.lwds=1                         
)
dev.off()
