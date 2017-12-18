# Read data into the system
library(readxl)
library(dplyr)

get_data <- function(raw_directory,filenm,siteCol,dateCol="Date",...) {

  df <- as.data.frame(read_excel(paste(raw_directory,filenm,sep=""),...))
  
  pdateCol <- "pdate"
  df <- addPdate(df,dateCol,siteCol=siteCol)

    return(df)
}

  # startCol <- 4
  # endCol <- dim(df)[2]
# dfSummary63 <- get_data_info(df,pdateCol,startCol,endCol)
# df63 <- df
# 
# variables <- names(df63)[4:dim(df63)[2]]
# beach = "63rd Street"

################################################################################
## Summarize Jeorse data
# get_Jeorse_data <- function(raw_directory,filenm) {
# raw_directory <- "./10_load_data/raw/Jeorse/"
# filenm <- "E coli 2010_2017-variables.xlsx"
# 
# df <- as.data.frame(read_excel(paste(raw_directory,filenm,sep="") ,sheet = "E coli 2010_2017-variables"))
# 
# dateCol <- "Date"
# pdateCol <- "pdate"
# siteCol <- "Location"
# df <- addPdate(df,dateCol,,siteCol=siteCol)
# 
# 
# return(df)
# }

# startCol <- 4
# endCol <- dim(df)[2]

#dfSummaryJeorse <- get_data_info(dfJeorse,pdateCol,startCol,endCol)

# variables <- names(dfJeorse)[4:dim(df63)[2]]
# beach = "Jeorse"


# filenm <- "varPlotsJeorse.pdf"
# pdf(filenm)
# plotVarsTS(dfJeorse,variables,"pdate",beach=beach)
# dev.off()
# shell.exec(filenm)
# 
# }
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
