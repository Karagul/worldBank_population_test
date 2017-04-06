# This is a set of functions which uses rworldmap to generate a gif of
# World Bank population data on a world map.

library(sp)
library(rworldmap)
library(dplyr)
library(ggplot2)
library(gganimate)
library(viridis)

map_worldBank_popDensity<-function(yrs){
  
  # Get density for all countries for all years -----------------------------------------------
  fullHist <- get_worldBank_popDensity_history(yrs[1],tail(yrs,1));
  
  #dens_min <-min(fullHist$density);
  #dens_max <-max(fullHist$density);
  
  
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
  wmapTbl <- left_join(wmapTbl,fullHist, by=c('id'='countryCode')); # this can't be right (4mm rows!)
  
  
  o <- ggplot(data=wmapTbl) +
    geom_polygon(aes(x = long, y = lat, group = group, fill=density, frame = year), color="gray90") +
    scale_fill_viridis(name="Density", begin = 0, end = 1, limits = c(0,600), na.value="gray99") +
    theme_void() +
    guides(fill = guide_colorbar(title.position = "top")) +
    labs(title = "Density") +
    #labs(caption = "Map by n=30 (www.nequals30.com), @nequals30") +
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.05, size=25)) +
    theme(plot.caption = element_text(hjust = 0, color="gray40", size=15)) +
    coord_cartesian(xlim = c(-11807982, 14807978)) +
    theme( legend.position = c(.5, .08), 
           legend.direction = "horizontal", 
           legend.title.align = 0,
           legend.key.size = unit(1.3, "cm"),
           legend.title=element_text(size=17), 
           legend.text=element_text(size=13) );
  
  gg_animate(o, "images/outgif.gif", title_frame =T,ani.width=1600, ani.height=820, dpi=800, interval = .4)
  
}