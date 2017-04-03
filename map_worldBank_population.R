# This is a set of functions which uses rworldmap to generate a gif of
# World Bank population data on a world map.

library(sp)
library(rworldmap)


map_worldBank_popDensity<-function(yrs){
  wmap <- getMap(resolution="low");
  
  # names(wpmap) # what's in the object
  # levels(factor(wmap$NAME)) # Lists countries on map
  # subset(wmap,!(NAME=='Australia')) # removes certain countries
  
  # wmap <- spTransform(wmap, CRS("+proj=robin"));
   wmap <- subset(wmap,!(NAME=='Antarctica'))
  
  plot(wmap);
  
}