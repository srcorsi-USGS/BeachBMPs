# Generate summary of beach data columns



get_data_info <- function(df,pdate,startCol,endCol){
  
  #  initDate <- as.POSIXct('1900-01-01',tz='UTC',origin ='1970-01-01 00:00.00 UTC')
  dfSummary <- data.frame(parameter=names(df)[startCol:endCol],minDate=NA,maxDate=NA,
                          min=NA,quant1=NA,median=NA,mean=NA,quant3=NA,max=NA,NAs=NA)
  
  for(i in startCol:endCol){
    notNAs <- which(!is.na(df[,i]))
    minDate <- min(df[notNAs,pdateCol])
    maxDate <- max(df[notNAs,pdateCol])
    dfSummary[(i-startCol+1),2:3] <- as.character(c(minDate,maxDate))
    summaryDat <- as.numeric(summary(df[,i]))
    if(length(summaryDat)==6) summaryDat <- c(summaryDat,0)
    dfSummary[(i-startCol+1),4:dim(dfSummary)[2]] <- summaryDat
    dfSummary$minDate <- as.POSIXct(dfSummary$minDate)
    dfSummary$maxDate <- as.POSIXct(dfSummary$maxDate)
  }
  dfSummary$numObs <- dim(df)[1]-dfSummary$NAs
  return(dfSummary)
}
###

convertNAs <- function(df,NAs) {
  for(i in 1:dim(df)[2]){
    df[which(df[,i] %in% NAs),i] <- NA
  }
  return(df)
}