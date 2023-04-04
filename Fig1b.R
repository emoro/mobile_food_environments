# code to reproduce the results in Figure 2 of the paper 
#"You are what you eat: Effect of mobile food environments on fast food visits"
# by Bernardo Garcia-Bulle et al. 
# Date: March 9, 2023
# Author: Esteban Moro

## libraries used
library(tidyverse)
library(data.table)
library(ggthemes)
library(latex2exp)
library(patchwork)
library(jtools)
library(sf)

## visualization settings
source("viz_settings.R")


## load data
density.df <- fread("./data/density_distance_to_places_all_MSA.csv")

## load census tracts in CA from tigris (census)
require(tigris)
trs <- tracts(state="CA")

# keep only those census tracts in the LA metro area
trs <- trs %>% 
  filter(substr(GEOID,1,5) %in% c("06037","06059"))

#plot
density.df.together <- density.df[exp(x)>100,.(dens=mean(dens)),.(x,type)]
gfig1b <- ggplot() + 
  geom_line(data=density.df.together[type=="all"],
            aes(x=exp(x)/1000,y=dens),color="black",lwd=1) +
  geom_line(data=density.df.together[type=="food"],
            aes(x=exp(x)/1000,y=dens),color=food_color,lwd=1,
            linetype=1) +
  geom_line(data=density.df.together[type=="fast food"],
            aes(x=exp(x)/1000,y=dens),color=fast_food_color,lwd=1) +
  geom_line(data=density.df.together[type=="supermarket"],
            aes(x=exp(x)/1000,y=dens),color="darkgrey",lwd=1) +  
  scale_x_log10() + labs(x="Distance from home (in km)",y="Density") +
  geom_vline(xintercept = as.numeric(sqrt(mean(st_area(trs))/1000^2)),linetype=3,lwd=1) + 
  theme_hc()


gfig1b