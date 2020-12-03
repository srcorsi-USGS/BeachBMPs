
library(tidyverse)

methods <- unique(j63$BEACH_NAME)

gantt <- j63 %>%
  group_by(BEACH_NAME,ANALYSIS_METHOD_TYPE_TEXT) %>%
  summarize(start = max(pdate),end = min(pdate)) %>%
  rename(method = ANALYSIS_METHOD_TYPE_TEXT,beach = BEACH_NAME)

gantt <- pivot_longer(gantt,cols = c("start","end"),names_to = "time", values_to = "date") %>%
  mutate(date = as.Date(date), method = factor(method))

#gantt <- filter(gantt,beach == "63rd Street Beach")

beaches <- unique(gantt$beach)

bmps <- data.frame(beach = "63rd Street Beach", method = "Pre BMP 1", time = "start", date = "2006-05-01")
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Pre BMP 1", time = "end", date = "2009-10-01"))
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Post BMP 1", time = "start", date = "2010-05-01"))
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Post BMP 1", time = "end", date = "2016-10-01"))

bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Pre BMP 2", time = "start", date = "2008-05-01"))
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Pre BMP 2", time = "end", date = "2009-10-02"))
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Post BMP 2", time = "start", date = "2014-05-01"))
bmps <- rbind(bmps,data.frame(beach = "63rd Street Beach", method = "Post BMP 2", time = "end", date = "2016-10-01"))


bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach I", method = "Pre BMP", time = "start", date = "2013-05-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach I", method = "Pre BMP", time = "end", date = "2014-10-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach I", method = "Post BMP", time = "start", date = "2016-05-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach I", method = "Post BMP", time = "end", date = "2017-10-01"))

bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach II", method = "Pre BMP", time = "start", date = "2013-05-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach II", method = "Pre BMP", time = "end", date = "2014-10-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach II", method = "Post BMP", time = "start", date = "2016-05-01"))
bmps <- rbind(bmps,data.frame(beach = "Jeorse Park Beach II", method = "Post BMP", time = "end", date = "2017-10-01"))



bmps$date <- as.Date(bmps$date)

gantt <- rbind(gantt,bmps)

gantt$method <- ifelse(gantt$method == "","Unspecified",gantt$method)

unique(gantt$method)

methods <- c("Coliform/E. coli Enzyme substrate test; ONPG-MUG test",
             "Colilert-18 hour",
             "mTEC",
             "QPCRenterococci - Rapid Method",
             "Unspecified",
             "Colilert-18 hour, 2000",
             "Colilert 2000",
             "Pre BMP 1",
             "Post BMP 1",
             "Pre BMP 2",
             "Post BMP 2",
             "Pre BMP",
             "Post BMP")                                        

method_order <- c(13,12,11,10,9,8,7,5,4,3,2,1,6)
gantt$method <- factor(gantt$method, levels = methods[method_order])


ggplot(gantt, aes(date, method,colour = method)) + 
  geom_line(size = 6) +
  xlab(NULL) + 
  ylab(NULL)+
  facet_wrap(~beach,nrow=3, scales="free") 



# 
# DataTestPeriod <- list(as.POSIXct(c("2006-01-02","2010-01-02","2010-01-02","2016-01-02")),
#                        as.POSIXct(c("2008-01-02","2010-01-02","2014-01-02","2016-01-02")))
# 
# 
# "63rd Street Beach Post BMP"
# "Jeorse Park Beach Pre BMP"
# "Jeorse Park Beach Post BMP"
# 
# ggplot(gantt,aes(date,method,colour=beach,group = item)) +
#   geom_line(size = 10)
# 
# 
# 
# 
# #Example
# library(reshape2)
# library(ggplot2)
# 
# tasks <- c("Review literature", "Mung data", "Stats analysis", "Write Report")
# dfr <- data.frame(
#   name        = factor(tasks, levels = tasks),
#   start.date  = as.Date(c("2010-08-24", "2010-10-01", "2010-11-01", "2011-02-14")),
#   end.date    = as.Date(c("2010-10-31", "2010-12-14", "2011-02-28", "2011-04-30")),
#   is.critical = c(TRUE, FALSE, FALSE, TRUE)
# )
# mdfr <- melt(dfr, measure.vars = c("start.date", "end.date"))
