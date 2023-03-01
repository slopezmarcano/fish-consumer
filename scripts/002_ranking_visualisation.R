#SLM
#Created: 01032023

#--DESCRIPTION--#
#my top 10 fish that I enjoy eating

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
fish_data_2 <- read_csv('data/fish_data.csv') %>%
        select(!c(spines, site,price, habitat, store_location, cooking_time)) %>%
        mutate(score = score-15) %>%
        filter(score >=1) %>%
        gather(key="characteristics", value ='measurement', -species) %>%
        mutate(characteristics=factor(characteristics)) %>%
        mutate(characteristics=fct_relevel(characteristics, c('flavour_ns', 'flavour_ws', 'texture', 'next_bite', 'score'))) %>%
        arrange(characteristics) %>%
        mutate(characteristics2 = as.numeric(characteristics))
#DEFINE ORDER FOR GGPLOT LEGEND
order_for_legend<- fish_data_2 %>%
      mutate(characteristics = as.character(characteristics)) %>%
      filter(characteristics == 'score') %>%
      group_by(species) %>%
      arrange(desc(measurement)) %>%
      pull(species)
#CREATE NEW COLUMN CALLED SPECIED_ORDERED
fish_data_2$species_ordered <- factor(fish_data_2$species, levels = order_for_legend)
#DEFINE PALETTE
palettini <- c("#293241", "#ee6c4d", "#98c1d9", "#ef476f",
              "#3d5a80", "#f6bd60", "#af1e78", "#4FB477")


#--DEFINE THEME--#
#--SETTING THEME AND FONTS--#
theme_set(theme_clean(base_family = "Lato"))

theme_update(
  # Set size 20 and colour for both y and x axes
  axis.title = element_text(color = "#404E4D", size = 20),
  # Axes labels are greyish
  axis.text = element_text(color = "#404E4D"),
  strip.text = element_text(colour = "#404E4D", size=20),
  # Set the size of the axes labels and margins.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 20, margin = margin(r = 5)),
  # Also, the ticks have a very dark grey color
  axis.ticks = element_line(color = "#333d3d", size = .5),
  # The length of the axis ticks is increased.
  #axis.ticks.length.x = unit(1.3, "lines"),
  #axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 15, 20, 15),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  strip.background = element_rect(fill='grey98', color = "grey98"),
  panel.border = element_rect(color='#e1dfdf', fill= NA, size=1),
  # Customize title appearence
  plot.title = element_text(
    color = "#404E4D", 
    size = 30, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "#656363", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "#656363", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40)), # Large margin on the top of the caption.
  # Remove legend
  legend.position = "none",
  legend.background = element_rect(fill = 'grey98', color = NA),
  legend.title = element_text(color = "#404E4D", size= 20),
  legend.text =  element_text(color = "#404E4D", size= 18))

#--PLOT--#
legend_title <- "Species"
plot<- ggplot(fish_data_2, aes(x=characteristics2, y=measurement, color=species_ordered))+
    geom_point(size = 5) +
    geom_bump() +
    scale_color_manual(legend_title, values = palettini)+
    theme(legend.position = c(0.5,0.20)) +
    labs(x= "Tasting Progression", y = "Ranking",
        title = "Fish that I like to eat",
        subtitle = paste("From the Caribbean to Australia, here are my top 10 fish that I like to eat.", "\n",
        "The current total amount of fish tasted are", nrow(fish_data), ". The last time this graph was updated was", Sys.Date(), ".",
        sep='\n'),
        caption = "Visualization by S Lopez Marcano")

ggsave("outputs/fish_that_i_like_to_eat.pdf", height=12)