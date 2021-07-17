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

treemap(
  monster_type,
  index = "monster_type",
  vSize = "n",        
  type="index",
  
  # Main
  title="What is the ",
  palette="Dark2",
  
  # Borders:
  border.col=c("black"),             
  border.lwds=1                         
)

