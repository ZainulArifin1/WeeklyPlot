# Packages ----------------------------------------------------------------
library(tidyverse)
library(scales)
library(ragg)
library(RColorBrewer)
'%ni%' <- Negate('%in%')
options(scipen=999)

sysfonts::font_add_google('Cairo')
showtext::showtext_auto()


# Input Data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-08-10')
tuesdata <- tidytuesdayR::tt_load(2021, week = 33)
ipd <- tuesdata$ipd
chain_investment <- tuesdata$chain_investment
investment <- tuesdata$investment

investment_filt <- investment %>%
  filter(meta_cat %ni% c("Total infrastructure", "Total basic infrastructure"))


# Viz ---------------------------------------------------------------------

nb.cols <- length(unique(investment_filt$meta_cat))
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

ggplot(data = investment_filt, mapping = aes(x = year, y = gross_inv, fill = meta_cat)) +
  geom_col() +
  scale_fill_manual(values = mycolors)
