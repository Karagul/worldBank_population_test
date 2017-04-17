library(DBI)
dbCn <- dbConnect(RMySQL::MySQL(),group='testConnect');

load_worldBank_area <- function(){
  
  # Download ZIP containing CSVs and unzip ---------------------------------------------
  # The ZIP file from this URL: http://api.worldbank.org/v2/en/indicator/AG.LND.TOTL.K2?downloadformat=csv
  
  # (Not Implemented)
  
  # Open CSV files ---------------------------------------------------------------------
  mainFile = readLines('API_AG.LND.TOTL.K2_DS2_en_csv_v2.csv',-1);
  writeLines(mainFile[-(1:4)],'worldBank_area_main.csv');
  mainTbl <- read.csv('worldBank_area_main.csv',stringsAsFactors = FALSE);
  
  # Get a list of the country codes in our current database ----------------------------
  countryID_lookup <- dbGetQuery(dbCn,'select countryCode,country_ID from worldBank.countryLookup;');
  
  # Load populations into populationByYear table -------------------------------------
  dbGetQuery(dbCn,'delete from worldBank.areaByYear');
  for (i in 1:nrow(mainTbl)){
    thisCountryID <- countryID_lookup[countryID_lookup[,"countryCode"]==mainTbl[i,"Country.Code"],"country_ID"];
    if (length(thisCountryID)==1){
      for (y in 1961:2015){
        thisArea <- mainTbl[i,paste('X',y,sep="")];

        # World Bank is missing areas for some countries before 2000. This assumes the area is the same as 2015
        if (is.na(thisArea)) {
          if (!is.na(mainTbl[i,'X2015'])){
            thisArea = mainTbl[i,'X2015'];
          }
        }
        thisArea[is.na(thisArea)] <- 'NULL';
        
        q <- paste("insert into worldBank.areaByYear (country_ID,areaYear,landArea) values(",thisCountryID,",",y,",",thisArea,"); ",sep="");
        dbGetQuery(dbCn,q);
      }
    }
  }
  
}