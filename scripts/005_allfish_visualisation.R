#SLM
#Created: 03042023

#--DESCRIPTION--#
#All the fish that I have tried in my life

#--LIBRARY--#
library(tidyverse)
library(ggbump)
library(ggthemes)
library(ggtext) #add and modify text to ggpplot
library(showtext) #fonts
font_add_google("Lato")
showtext_auto()

#--VISUALISE RESULTS--#
#FORMAT THE DATA IN THE GGBUMP FORMAT
plot <- read_csv('data/fish_data.csv') %>%
ggplot(aes(x = score, y = reorder(species, desc(score)))) + #TODO #6
  geom_segment(aes(xend = 0, yend = species), color = "#ef476f") +
  geom_point(size = 3, color = "#ef476f")


