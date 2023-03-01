#SLM
#Created: 28022023

#--DESCRIPTION--#
#linear model to understand the relationhsip between my fish score and all the tasting characteristics

#--LIBRARY--#
library(tidyverse)
library(corrplot)
library(ggpubr)

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

#the variables are good predictions of the score - as expected. 
