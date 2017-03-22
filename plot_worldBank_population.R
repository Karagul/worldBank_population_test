# This is a set of functions which use ggplot2 to plot World Bank population data stored in
# a local database. the get_worldBank_functions file contains the functions needed to get the
# data.

library(ggplot2)

# Plots a pie graph of world population, by region, for a certain year ----------------------
plot_pie_region<-function(regionType,year){
  
  popTbl <- get_worldBank_region_population(regionType,year);
  
  p <- ggplot(popTbl, aes(x=1, y=population, fill=region)) +
    geom_bar(stat='identity', color='black') +
    guides(fill=guide_legend(override.aes=list(color=NA))) + 
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank());
  
  print(p)
}