# This is a set of functions which uses rworldmap to generate a gif of
# World Bank population data on a world map.

library(sp)
library(rworldmap)
library(dplyr)
library(ggplot2)

map_worldBank_popDensity<-function(yrs){
  
  # Get density for all countries for all years -----------------------------------------------
  fullHist <- get_worldBank_popDensity_history(1961,2015);
  
  
  # Pull in and set up world map --------------------------------------------------------------
  wmap <- getMap(resolution="low");
  
  # Interacting with rworldmap:
    # names(wpmap) # what's in the object
    # levels(factor(wmap$NAME)) # Lists countries on map
    # subset(wmap,!(NAME=='Australia')) # removes certain countries
  
  wmap <- spTransform(wmap, CRS("+proj=robin"));
  wmap <- subset(wmap,!(NAME=='Antarctica'));
  
  # Apply density data to map -----------------------------------------------------------------
  wmapTbl <- fortify(wmap,region="ISO3");
  #wmapTbl <- left_join(wmapTbl,fullHist, by=c('id'='countryCode'));
  
   
  plot(wmap);
  
}