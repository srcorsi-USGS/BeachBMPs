#Add POSIXct date to beach dataframes

addPdate <- function(df,dateCol,timeCol = NA, dateFormat = "Y-m-d", siteCol){
  
  #define date and time column
  if(is.na(timeCol)) {dates <- df[,dateCol]
  }else{dates <- paste(df[,dateCol],df[,timeCol])
  }
  
  df$pdate <- as.POSIXct(dates ,tz="GMT",format=dateFormat)
  
  #Define date, time, and site ID columns
  firstColumns <- c(siteCol,"pdate",dateCol)
  if(!is.na(timeCol))firstColumns <- c(firstColumns,timeCol)
  firstColumnNums <- which(names(df) %in% firstColumns)
  
  dfNames <- c(firstColumns,names(df)[-firstColumnNums])
  df <- df[,dfNames]
  
  return(df)
}