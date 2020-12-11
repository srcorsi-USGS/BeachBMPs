# Read data into the system

library(readxl)
library(dplyr)
library(googlesheets)

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

plotVarsTS <- function(df,variables,dateCol,beach){
  x <- df[,dateCol]
  
  for(i in 1:length(variables)) {
    y <- df[,variables[i]]
    plot(x,y,ylab=variables[i],log="y",main = paste(variables[i],"at",beach))
  }
}
    
    
    ## Summarize 63rd St data
raw_directory <- "./10_load_data/raw/63rd/"
filenm <- "63rd E coli and variables.xlsx"

df <- as.data.frame(read_excel(paste(raw_directory,filenm,sep="") ,sheet = "E coli and variables"))

dateCol <- "Date"
pdateCol <- "pdate"
siteCol <- "ClientID"
df <- addPdate(df,dateCol,siteCol=siteCol)

startCol <- 4
endCol <- dim(df)[2]

dfSummary63 <- get_data_info(df,pdateCol,startCol,endCol)
df63 <- df

variables <- names(df63)[4:dim(df63)[2]]
beach = "63rd Street"

filenm <- "varPlots63rd.pdf"
pdf(filenm)
plotVarsTS(df63,variables,"pdate",beach=beach)
dev.off()
shell.exec(filenm)

################################################################################
## Summarize Jeorse data
# raw_directory <- "./10_load_data/raw/Jeorse/"
# filenm <- "E coli 2010_2017-variables.xlsx"
# 
# dfJeorse <- as.data.frame(read_excel(paste(raw_directory,filenm,sep="") ,sheet = "E coli 2010_2017-variables"))
# 
# dateCol <- "Date"
# pdateCol <- "pdate"
# siteCol <- "Location"
# dfJeorse <- addPdate(dfJeorse,dateCol,,siteCol=siteCol)
# 
# startCol <- 4
# endCol <- dim(dfJeorse)[2]
# df <- dfJeorse
# 
# dfSummaryJeorse <- get_data_info(dfJeorse,pdateCol,startCol,endCol)
# 
# variables <- names(dfJeorse)[4:dim(df63)[2]]
# beach = "Jeorse"
# 
# 
# filenm <- "varPlotsJeorse.pdf"
# pdf(filenm)
# plotVarsTS(dfJeorse,variables,"pdate",beach=beach)
# dev.off()
# shell.exec(filenm)

# 
# 
# ## Summarize Racine data
# raw_directory <- "./10_load_data/raw/Racine/"
# filenm <- "North Beach Historical Database 1995-2016, COMBINED DATA.xlsx"
# filenm.csv <- "North Beach Historical Database 1995-2016, COMBINED DATA.csv"
# 
# dfRacine <- read_excel(paste(raw_directory,filenm,sep="") ,sheet = "CombinedData94-16")
# dfRacine <- read.csv(paste(raw_directory,filenm.csv,sep=""),stringsAsFactors = FALSE)
# 
# dfRacine <- convertNAs(dfRacine,NAs = c("NR","#N/A"))
# dateCol <- "Date"
# timeCol <- "Time.Collected"
# pdateCol <- "pdate"
# siteCol <- "Sample.Dilution"
# dateFormat <- "%m/%d/%Y %H:%M:S"
# dfRacine <- addPdate(dfRacine,dateCol,timeCol,siteCol, format=dateFormat)
# 
# 
# 
# 
# startCol <- 5
# endCol <- dim(dfRacine)[2]
# df <- dfRacine
# 
# dfSummaryRacine <- get_data_info(dfRacine,pdateCol,startCol,endCol)
# 
# 
