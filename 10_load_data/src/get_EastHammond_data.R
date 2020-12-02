#Examine Illinois BeachGuard data
#Extract South Calumet data for paired analysis
library(tidyverse)

data_loc <- "C:/Users/srcorsi/OneDrive - DOI/GLRI Chicago Cresent/Data/Jeorse"
data_loc_git <- file.path("20_process_data","out")
ibg <-read.csv(file = paste0(data_loc,"/BeachGuardDataExport Indiana.csv"), stringsAsFactors = FALSE)

ibg$pdate <- as.POSIXct(ibg$SAMPLE_DATE,format = "%m/%d/%Y %I:%M:%S %p",tz="CST6CDT") 
ibg$Date <- as.Date(ibg$pdate)

# ibg$year <- as.POSIXlt(ibg$pdate)$year + 1900
# ibg <- subset(ibg,year > 2006)

#boxplot(RESULT_VALUE+1~year,data = ibg, log="y")

eh <- ibg[grep("hammond",ibg$BEACH_NAME,ignore.case = TRUE),]

#boxplot(RESULT_VALUE+1~year,data = eh, log="y")

par(mar=c(11,4,3,2))
ncex <- 0.6
bp <- boxplot(RESULT_VALUE + 1~ANALYSIS_METHOD_TYPE_TEXT ,data = eh,log="y",las=2,xlab = "")
mtext("n = ", line = 0.1, side = 3,adj=0, cex=ncex)
mtext(paste(bp$n, sep = ""), at = seq_along(bp$n), line = 0.1, side = 3,cex=ncex)

eh <- eh[which(eh$ANALYSIS_METHOD_TYPE_TEXT %in% unique(eh$ANALYSIS_METHOD_TYPE_TEXT)[1:2]),]

par(mar=c(11,4,3,2))
ncex <- 0.6
bp <- boxplot(RESULT_VALUE + 1~ANALYSIS_METHOD_TYPE_TEXT ,data = eh,log="y",las=2,xlab = "")
mtext("n = ", line = 0.1, side = 3,adj=0, cex=ncex)
mtext(paste(bp$n, sep = ""), at = seq_along(bp$n), line = 0.1, side = 3,cex=ncex)
anal.code <- ifelse(eh$ANALYSIS_METHOD_TYPE_TEXT == "",1,2)

plot(eh$Date,anal.code)
year <- as.POSIXlt(eh$Date)$year + 1900

boxplot(eh$RESULT_VALUE+1~year,log="y")

