# This is a set of functions which use ggplot2 to plot World Bank population data stored in
# a local database. the get_worldBank_functions file contains the functions needed to get the
# data.

library(ggplot2)

# Plots a pie graph of world population, by region, for a certain year ----------------------
plot_pie_region<-function(regionType,year){
  
  popTbl <- get_worldBank_region_population(regionType,year);
  # popTbl <- get_worldBank_region_area(regionType,year);
  
  p <- ggplot(popTbl, aes(x=1, y=population, fill=region)) +
    geom_bar(stat='identity', color='black') +
    guides(fill=guide_legend(title="Regions",override.aes=list(color=NA))) + 
    coord_polar(theta='y') +
    theme(axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank()) +
    ggtitle(paste("World Population by ",regionType," Regions, ",year,sep='')) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"));
  
  print(p)
}

# Plots a stacked area graph of all the regions populations through history ----------------
plot_stackArea_histByRegion<-function(regionType,yrs){
  
  popTbl <- get_worldBank_region_population(regionType,tail(yrs,n=1));
  
  regionHistTbl <- data.frame(region=character(),year=integer(),population=numeric());
  
  for (i in 1:nrow(popTbl)){
    thisHist <- get_worldBank_region_popHist(popTbl[i,"region"]);
    thisHist <- cbind(rep(popTbl[i,"region"],nrow(thisHist)),thisHist);
    colnames(thisHist)[1] = 'region';
    regionHistTbl <- rbind(regionHistTbl,thisHist);
  }
  
  regionHistTbl[,"population"] <- regionHistTbl[,"population"]/1000000
  
  p <- ggplot(regionHistTbl, aes(x=year, y=population, fill=region)) + geom_area() +
    guides(fill=guide_legend(title="Regions",override.aes=list(color=NA))) +
    ggtitle(paste("World Population by ",regionType," Regions, ",yrs[1]," to ",tail(yrs,n=1),sep='')) + 
    theme(plot.title = element_text(lineheight=.8, face="bold"));
  
  print(p)
}