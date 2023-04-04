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
poi_LA <- fread("./data/poi_31080.csv.gz")

# load census tracts in CA from tigris (census)
require(tigris)
trs <- tracts(state="CA")
# keep only those census tracts in the LA metro area
trs <- trs %>% 
  filter(substr(GEOID,1,5) %in% c("06037","06059"))

# load counties for the map visualization and remove water
counties <- tigris::counties(state="CA")
state <- st_union(counties)
state <- st_crop(state,st_bbox(trs))
get_water <- function(county_GEOID){
  area_water("CA", county_GEOID, class = "sf")
}
water <- do.call(rbind, 
                 lapply(counties$COUNTYFP,get_water))
water <- st_crop(water,st_bbox(trs))
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}
sf::sf_use_s2(FALSE)
state <- st_erase(state,water)


# Make hexagonal tesselation of the LA area
hex <- st_make_grid(st_as_sf(trs),cellsize=0.0106,square=F)
hex <- st_transform(hex,4326)
hex <- st_as_sf(hex)


# Calculate the fraction of FFO by hexagon
poi_coords <- st_as_sf(poi_LA,coords=c("lon","lat"),crs=4326)
w1 <- st_within(poi_coords,hex)
poi_vis <- poi_LA
poi_vis$hex_id <- as.numeric(w1)
poi_df <- poi_vis[,.(npoi=.N,nfood=sum(is.food),nff=sum(is.fast.food)),.(hex_id)]
hex$hex_id <- 1:nrow(hex)
hex <- merge(hex,poi_df)


# Map
new_bbox <- st_bbox(c(xmin =-118.6377, xmax = -117.7174, ymax = 33.5615, ymin = 34.3101), crs = st_crs(4326))
hex_new <- st_crop(hex,new_bbox)
hex_new$`Fraction of FF places` = hex_new$nff/hex_new$nfood
state_new <- st_crop(st_transform(state,4326),new_bbox) %>%
  sf::st_collection_extract("POLYGON")


require(tmap)
col_land <- rgb(0.8,0.8,0.8,0.8)
hex_new <- hex_new %>% 
  rename(`Ratio of FF places`="Fraction of FF places")
map_LA <- 
  tm_shape(state_new) + 
  tm_fill(col_land) + tm_borders("black",lwd=.2) +
  tm_shape(hex_new[hex_new$nfood>4,]) + 
  tm_polygons(col="Ratio of FF places",border.col="transparent") +
  tm_scale_bar(position = c("left", "bottom"))+
  tm_legend(position=c("left","bottom"))

map_LA

