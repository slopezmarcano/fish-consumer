#SLM
#Created: 13032023

#--DESCRIPTION--#
#linear model to understand the relationhsip between my fish score and all the tasting characteristics

#--LIBRARY--#
library(tidyverse)

#Load the new data set
new_data <- read.csv("data/new_fish_tasted.csv") %>%
    select(flavour_no_seasoning, flavour_with_seasoning, texture, spines, next_bite)


new_data_sca <- as.data.frame(scale(new_data, center=TRUE, scale=TRUE))

#Use the predict() function to generate predicted scores for the new data
predicted_scores <- predict(model, newdata=new_data_sca)

#View the predicted scores
predicted_scores


