load_worldBank_populationData <- function() {
  # Loads World Bank population data (country population by year 1960-present) into a database.
  # Population data is sourced from CSV files in the working directory.
  
  
  # Download ZIP containing CSVs and unzip ---------------------------------------------
  # The ZIP file from this URL: http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=csv
  
  # (Not Implemented)
  
  # Open CSV files ---------------------------------------------------------------------
  
  # Main CSV file
  mainFile = readLines('API_SP.POP.TOTL_DS2_en_csv_v2.csv',-1);
  writeLines(mainFile[-(1:4)],'worldBank_population_main.csv');
  wbPopTbl <- read.csv('worldBank_population_main.csv',stringsAsFactors = FALSE);
  
  # Metadata CSV file
  metaFile = readLines('Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2.csv',-1);
  writeLines(metaFile,'worldBank_population_meta.csv');
  wbMetaTbl <- read.csv('worldBank_population_meta.csv',stringsAsFactors = FALSE);
  
  # Filter out things that aren't countries (e.g. regions) -----------------------------
  
  goodCodes <- wbMetaTbl[wbMetaTbl[,"Region"]!="","Country.Code"];
  isCountry <- is.element(wbPopTbl[,"Country.Code"],goodCodes);
  wbPopTbl <- wbPopTbl[isCountry,];
  wbMetaTbl <- wbMetaTbl[wbMetaTbl[,"Region"]!="",];
  
  cat(c('World Population as of 2015:',sum(wbPopTbl[!is.na(wbPopTbl[,"X2015"]),"X2015"])),'\n');
  
  # Connect to the (local) database ---------------------------------------------------
  
  library(DBI)
  cn<-dbConnect(RMySQL::MySQL(),group='testConnect');
  
  # Add regions (geography,income) as specified in WB meta file -----------------------
  
  dbGetQuery(cn,'delete from worldBank.regions');
  dbGetQuery(cn,'alter table worldBank.regions auto_increment=1');
  
  # Geographic
  wb_geoRegions <- unique(wbMetaTbl[,"Region"]);
  for (i in 1:length(wb_geoRegions)){
    thisRegionName <- gsub("\\'","",trimws(wb_geoRegions[i],which="both"));
    q <- paste("insert into worldBank.regions (region,regionType) values('",thisRegionName,"','GEO')",sep='');
    dbGetQuery(cn,q);
  }
  
  # Income
  wb_inRegions <- unique(wbMetaTbl[,"IncomeGroup"]);
  for (i in 1:length(wb_inRegions)){
    thisRegionName <- gsub("\\'","",trimws(wb_inRegions[i],which="both"));
    q <- paste("insert into worldBank.regions (region,regionType) values('",thisRegionName,"','INC')",sep='');
    dbGetQuery(cn,q);
  }
  
  # Load country data into lookup table -----------------------------------------------
  
  dbGetQuery(cn,'delete from worldBank.countryLookup');
  
  for (i in 1:nrow(wbPopTbl)){
    thisCountryName <- gsub("\\'","",trimws(wbPopTbl[i,"Country.Name"],which="both"));
    thisCountryCode <- trimws(wbPopTbl[i,"Country.Code"],which="both");
    q <- paste("insert into worldBank.countryLookup (country_ID,countryName,countryCode) values(",i,",'",thisCountryName,"','",thisCountryCode,"'); ",sep="");
    dbGetQuery(cn,q);
  }
  
  # Pull down the lookup tables we just populated ------------------------------------
  
  countryID_lookup <- dbGetQuery(cn,'select countryCode,country_ID from worldBank.countryLookup;');
  regionID_lookup <- dbGetQuery(cn,'select region,region_ID,regionType from worldBank.regions;');
  
  # Load regions for each country into countryRegion table -------------------------
  
  dbGetQuery(cn,'delete from worldBank.countryRegion');
  for (i in 1:nrow(wbMetaTbl)){
    thisGeo <- regionID_lookup[regionID_lookup[,"region"]==wbMetaTbl[i,"Region"],"region_ID"];
    thisInc <- regionID_lookup[regionID_lookup[,"region"]==wbMetaTbl[i,"IncomeGroup"],"region_ID"];
    thisCountryID <- countryID_lookup[countryID_lookup[,"countryCode"]==wbMetaTbl[i,"Country.Code"],"country_ID"];
    q1 <- paste("insert into worldBank.countryRegion (country_ID,region_ID) values(",thisCountryID,",",thisGeo,")",sep='');
    q2 <- paste("insert into worldBank.countryRegion (country_ID,region_ID) values(",thisCountryID,",",thisInc,")",sep='');
    dbGetQuery(cn,q1);
    dbGetQuery(cn,q2);
    # This is where you could add custom tags (e.g. Soviet Union)
  }
  
  # Load populations into populationByYear table -------------------------------------
  
  dbGetQuery(cn,'delete from worldBank.popByYear');
  for (i in 1:nrow(wbPopTbl)){
    thisCountryID <- countryID_lookup[countryID_lookup[,"countryCode"]==wbPopTbl[i,"Country.Code"],"country_ID"];
    for (y in 1960:2015){
      thisPop <- wbPopTbl[i,paste('X',y,sep="")];
      thisPop[is.na(thisPop)] <- 'NULL';
      q <- paste("insert into worldBank.popByYear (country_ID,popYear,population) values(",thisCountryID,",",y,",",thisPop,"); ",sep="");
      dbGetQuery(cn,q);
    }
  }
  
  # Sample pull-down:
  # select * from worldBank.countryLookup 
  # left join worldBank.popByYear on (worldBank.countryLookup.country_ID=worldBank.popByYear.country_ID)
  # where countryCode='USD' order by popYear;
  
}