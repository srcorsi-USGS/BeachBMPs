#Examine Illinois BeachGuard data
#Extract South Calumet data for paired analysis
library(tidyverse)

data_loc <- "C:/Users/srcorsi/OneDrive - DOI/GLRI Chicago Cresent/Data/Jeorse/Raw/"
data_loc_git <- file.path("20_process_data","out")
ibg <-read.csv(file = paste0(data_loc,"BeachGuardDataExport Indiana.csv"), stringsAsFactors = FALSE)

ibg$pdate <- as.POSIXct(ibg$SAMPLE_DATE,format = "%m/%d/%Y %I:%M:%S %p",tz="CST6CDT") 
ibg$Date <- as.Date(ibg$pdate)

# ibg$year <- as.POSIXlt(ibg$pdate)$year + 1900
# ibg <- subset(ibg,year > 2006)

#boxplot(RESULT_VALUE+1~year,data = ibg, log="y")

cal <- ibg[grep("calumet",ibg$BEACH_NAME,ignore.case = TRUE),]

#boxplot(RESULT_VALUE+1~year,data = cal, log="y")

