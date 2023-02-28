#SLM
#Created: 28022023

#--DESCRIPTION--#


#--LIBRARY--#
library(tidyverse)
library(corrplot)
library(ggpubr)
library(ggbump)

#--DATASET--#
fish_data <- read_csv('data/fish_data.csv') %>%
    select(flavour_ns, flavour_ws, texture, spines, next_bite, score)

#--CHECK DISTRIBUTION OF VARIABLES--#
ggplot(gather(fish_data, cols, value), aes(x = value)) + 
       geom_histogram() + 
       facet_grid(.~cols)

#--CHECK NORMALITY--#
apply(fish_data,2,shapiro.test) #some variables are normal others not

#--MEAN CENTER AND SCALE DATASET--#
fish_data_sca <- as.data.frame(scale(fish_data, center=TRUE, scale=TRUE))
#--CHECK NORMALITY AGAIN --#
apply(fish_data_sca,2,shapiro.test) #some variables are not normal but visual representation shows good distribution

#--VISUAL REPRESENTATION OF NORMALITY--#
ggqqplot(fish_data_sca$flavour_ws)

#--CHECK CORRELATIONS BETWEEN VARIABLES--#
correlations <- cor(fish_data_sca)
corrplot(correlations, type="upper", method="circle", tl.col="black", tl.srt=45, tl.cex=0.8)

#--LINEAR MODEl--#
model <- lm(score ~ flavour_ns + flavour_ws + texture + spines + next_bite, data=fish_data_sca)
summary(model)

# Visualize results
ggplot(data=fish_data, aes(x=score, y=predict(model))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Overall score vs Predicted score") +
  theme_bw()

ggplot()

test <- fish_data <- 

level_order <- c('spines','flavour_ns', 'flavour_ws', 'texture', 'next_bite', 'score') 

ggplot(test, aes(x=factor(characteristics, level=level_order), y=measurement, color=species))+
    geom_point()+
    geom_path(group='species')

test2 <- read_csv('data/fish_data.csv') %>%
        select(!c(site,price, habitat, store_location, cooking_time)) %>%
        mutate(score = score-15) %>%
        filter(score >=1) %>%
        gather(key="characteristics", value ='measurement', -species) %>%
        mutate(characteristics=factor(characteristics)) %>%
        mutate(characteristics=fct_relevel(characteristics, c('spines','flavour_ns', 'flavour_ws', 'texture', 'next_bite', 'score'))) %>%
        arrange(characteristics) %>%
        mutate(characteristics2 = as.numeric(characteristics))


ggplot(test2, aes(x=characteristics2, y=measurement, color=species))+
    geom_bump(size = 1.5)+
  geom_text(data = test2 %>% filter(characteristics2 == max(characteristics2)),
            aes(x = characteristics2 + 0.1, label = species),
            size = 10, hjust = "inward", vjust="inward") +
  scale_color_brewer(palette = "Pastel1") +
  theme_void() +
  theme(legend.position = "none")

ggsave()
# Calculate effect sizes
library(jtools)
summ(model, scale=T)
