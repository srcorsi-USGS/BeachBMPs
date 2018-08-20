# get_Racine_data.R
# Read in data from Racine North Shore beach and format for further processing
# 

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


get_Racine <- function(){
dfNB <- read.csv("10_load_data/raw/Racine/North Beach Historical Database 1995-2016, COMBINED EDIT.csv",stringsAsFactors = FALSE, na.strings = c("NR","#N/A", "NC", "No Data","N/A"))
#write.csv(dfNB[c(which(names(dfNB)=="Air.Temperature...F."):which(names(dfNB)=="Sigma...NOAA.Milwaukee..WI...Station.ID..9087057"))],file="IVs.csv")
dfNB$Site <- "NorthBeach"

dfNBIVs <- read.csv("10_load_data/raw/Racine/IVs.csv",stringsAsFactors = FALSE)
IVs <- dfNBIVs[which(dfNBIVs$Keep==1),"IV"]

dateCol <- "Date"
pdateCol <- "pdate"
siteCol <- "Site"
dfNB <- addPdate(dfNB,dateCol,dateFormat = "%m/%d/%Y", siteCol=siteCol)






#---------------------------------------------------------------------------------------

names(dfNB)[which(names(dfNB) %in% c("E.coli..CFU.or.MPN.per.100mL."))] <- "Ecoli"

response <- "Ecoli"
responseRmk <- "EcoliRmk"

dfNB$EcoliRmk <- "NA"
dfNB[grep(">",dfNB$Ecoli),"EcoliRmk"] <- ">"
dfNB$Ecoli <- as.numeric(sub("<","", dfNB$Ecoli))

df <- dfNB[,c(pdateCol,response,IVs)]

# Adjustments needed on these variables:
# Adjutsed.Wind.Speed..mph. (remove "variable"), 

df[grep("iable",df$Adjutsed.Wind.Speed..mph.),"Adjutsed.Wind.Speed..mph."] <- NA
df$Adjutsed.Wind.Speed..mph. <- as.numeric(df$Adjutsed.Wind.Speed..mph.)


dfCheck <- df[,-which(sapply(df, typeof)=="character" )]
startCol <- 2
endCol <- dim(dfCheck)[2]

#dfSummary <- get_data_info(dfCheck,pdateCol,startCol,endCol)



variables <- names(dfCheck)[2:dim(dfCheck)[2]]
beach = "North Beach"

# filenm <- "NorthBeach.pdf"
# pdf(filenm)
# plotVarsTS(dfCheck,variables,"pdate",beach=beach)
# dev.off()
# shell.exec(filenm)
# 
# plot(df$Air.Temperature...F.~df$Water.Temperature...F.)
# abline(0,1)
# 
# plot(df$Air.Temperature...F.~df$Air.Temperature.Celsius.)
# abline(0,1)
# 
# racineRain144 <- df$Racine.WWTP...144hr.Precip..in..*25.4
# plot(df$Total.precip..mm....1.hr..556.723..Sum.144.hr ~racineRain144)
# abline(0,1)
# 
# 
# #plotColor <- ifelse(df$pdate < as.POSIXct("2001-01-01"),"red","black") #no GLCFS data before 2006
# plot(df$Adjutsed.Wind.Speed..mph.* 0.44704 ~ df$Air.Velocity.m.s.,col="blue" )
# abline(0,1)
# 
# plot(df$Wind.A.Component..BO....338.55ø.)

return(df)
}
#saveRDS(df,file="NorthBeach.rds")
