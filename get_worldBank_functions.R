# This is a set of functions which pulls data from the worldBank tables on your local SQL server.
# For details on how that table is populated, see the function load_worldBank_populationData

library(DBI)
dbCn <- dbConnect(RMySQL::MySQL(),group='testConnect');

# Get countries in region -----------------------------------------------------------
get_worldBank_region_countries <- function(regionName){

     q <- paste("select countryName,countryCode from worldBank.regions ",
             "right join worldBank.countryRegion on (regions.region_ID=countryRegion.region_ID) ",
             "right join worldBank.countryLookup on (countryRegion.country_ID=countryLookup.country_ID)",
             "where region='",regionName,"'",
             sep='');
  
  dbGetQuery(dbCn,q);
}

# Get countries in region -----------------------------------------------------------
get_worldBank_region_popHist <- function(regionName){

  # Construct query
  q <- paste("select popYear As year,cast(sum(population) As Integer) As population from worldBank.regions ",
             "right join worldBank.countryRegion on (regions.region_ID = countryRegion.region_ID) ",
             "right join worldBank.popByYear on (countryRegion.country_ID = popByYear.country_ID)",
             "where region='",regionName,"' ",
             "group by popYear order by popYear;",
             sep='');
  
  dbGetQuery(dbCn,q);
}
