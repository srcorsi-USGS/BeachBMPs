#Examine Illinois BeachGuard data
#Extract South Calumet data for paired analysis
library(tidyverse)

data_loc1 <- "C:/Users/srcorsi/OneDrive - DOI/GLRI Chicago Cresent/Data/Jeorse"
data_loc2 <- "C:/Users/srcorsi/OneDrive - DOI/GLRI Chicago Cresent/Data/63rd/Raw"
data_loc_git <- file.path("20_process_data","out")
ibg1 <-read.csv(file = paste0(data_loc1,"/BeachGuardDataExport Indiana.csv"), stringsAsFactors = FALSE)
ibg2 <-read.csv(file = paste0(data_loc2,"/BeachGuardDataExport Illinois.csv"), stringsAsFactors = FALSE)

ibg <- rbind(ibg1,ibg2)

ibg$pdate <- as.POSIXct(ibg$SAMPLE_DATE,format = "%m/%d/%Y %I:%M:%S %p",tz="CST6CDT") 
ibg$Date <- as.Date(ibg$pdate)

bg63rd <- filter(ibg,BEACH_NAME == "63rd Street Beach")
unique(bg63rd$ANALYSIS_METHOD_TYPE_TEXT)

# ibg$year <- as.POSIXlt(ibg$pdate)$year + 1900
# ibg <- subset(ibg,year > 2006)

#boxplot(RESULT_VALUE+1~year,data = ibg, log="y")

j63 <- ibg[grep(c("jeorse|63rd"),ibg$BEACH_NAME,ignore.case = TRUE),]

#boxplot(RESULT_VALUE+1~year,data = j63, log="y")

par(mar=c(11,4,3,2))
ncex <- 0.6

n_fun <- function(x){
  return(data.frame(y = 0.95*log10(200000),
                    label = length(x)))
}
analysis_boxplots <- ggplot(j63,aes(x= ANALYSIS_METHOD_TYPE_TEXT, y = RESULT_VALUE + 1)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  facet_wrap(~BEACH_NAME,nrow = 3) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  stat_summary(fun.data = n_fun, geom = "text",hjust = 0.5,
               #aes(group=season),
               position = position_dodge(0.9))
  
  
analysis_boxplots

bp <- boxplot(RESULT_VALUE + 1~ANALYSIS_METHOD_TYPE_TEXT + BEACH_NAME,data = j63,log="y",las=2,xlab = "")
mtext("n = ", line = 0.1, side = 3,adj=0, cex=ncex)
mtext(paste(bp$n, sep = ""), at = seq_along(bp$n), line = 0.1, side = 3,cex=ncex)

j63 <- j63[which(j63$ANALYSIS_METHOD_TYPE_TEXT %in% unique(j63$ANALYSIS_METHOD_TYPE_TEXT)[1:2]),]

par(mar=c(11,4,3,2))
ncex <- 0.6
bp <- boxplot(RESULT_VALUE + 1~ANALYSIS_METHOD_TYPE_TEXT ,data = j63,log="y",las=2,xlab = "")
mtext("n = ", line = 0.1, side = 3,adj=0, cex=ncex)
mtext(paste(bp$n, sep = ""), at = seq_along(bp$n), line = 0.1, side = 3,cex=ncex)
anal.code <- ifelse(j63$ANALYSIS_METHOD_TYPE_TEXT == "",1,2)

plot(j63$Date,anal.code)
year <- as.POSIXlt(j63$Date)$year + 1900

boxplot(j63$RESULT_VALUE+1~year,log="y")


jeorse <- ibg[grep("Jeorse", ibg$BEACH_NAME),]

plot(jeorse$pdate,jeorse$RESULT_VALUE,cex=0.6,ylab =  "E. coli",xlab = "",main = "Jeorse 1 & 2 E. coli and BMP evaluation periods")
dates <- as.POSIXct(c("2013-05-01","2014-10-01","2016-05-01","2017-10-01"))

abline(v=dates,col = c("blue","blue","forestgreen","forestgreen"))
text(x=mean(dates[1:2]),y=3500,labels = "pre",col="blue")
text(x=mean(dates[3:4]),y=3500,labels = "post",col = "forestgreen")

jeorse_eval <- subset(jeorse,pdate > dates[1] & pdate < dates[4])
jeorse_eval$year <- as.POSIXlt(jeorse_eval$pdate)$year + 1900
jeorse$year <- as.POSIXlt(jeorse$pdate)$year + 1900

method_counts <- jeorse %>% 
  group_by(year, ANALYSIS_METHOD_TYPE_TEXT) %>%
  summarize(n = length(RESULT_VALUE))

write.csv(method_counts,file = "Jeorse_metho_counts_by_year.csv",row.names = FALSE)

plot(JP1$pdate,JP1$Ecoli,cex=0.6,ylab =  "E. coli",xlab = "",main = "Jeorse 1 E. coli and BMP evaluation periods")
dates <- as.POSIXct(c("2013-05-01","2014-10-01","2016-05-01","2017-10-01"))

abline(v=dates,col = c("blue","blue","forestgreen","forestgreen"))
text(x=mean(dates[1:2]),y=4500,labels = "pre",col="blue")
text(x=mean(dates[3:4]),y=4500,labels = "post",col = "forestgreen")

plot(JP2$pdate,JP2$Ecoli,cex=0.6,ylab =  "E. coli",xlab = "",main = "Jeorse 2 E. coli and BMP evaluation periods")
dates <- as.POSIXct(c("2013-05-01","2014-10-01","2016-05-01","2017-10-01"))

abline(v=dates,col = c("blue","blue","forestgreen","forestgreen"))
text(x=mean(dates[1:2]),y=4500,labels = "pre",col="blue")
text(x=mean(dates[3:4]),y=4500,labels = "post",col = "forestgreen")

JP1_eval <- subset(JP1,pdate > dates[1] & pdate < dates[4])


