# Figure xx-model_boxplot

library(tidyverse)
library(gridExtra)
library(ggtext)
library(ggpubr)
library(scales)


# Read data
JP1_model <- readRDS(file = file.path("40_modeling","out","Jeorse1_model_df.rds"))
JP2_model <- readRDS(file = file.path("40_modeling","out","Jeorse2_model_df.rds"))
Racine_model <- readRDS(file = file.path("40_modeling","out","Racine_model_df.rds"))

#Process data 
JP1_ec <- JP1_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Jeorse 1")
JP1_ec$year <- as.POSIXlt(JP1_ec$pdate)$year + 1900
JP1_ec$year <- factor(JP1_ec$year,levels=sort(unique(JP1_ec$year)))
JP1_ec$bmp <- ifelse(JP1_ec$year %in% c(2012,2014),"pre","post")
JP1_ec$bmp <- ifelse(JP1_ec$year %in% c(2015),"transition",JP1_ec$bmp)

JP2_ec <- JP2_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Jeorse 2")
JP2_ec$year <- as.POSIXlt(JP2_ec$pdate)$year + 1900
JP2_ec$year <- factor(JP2_ec$year,levels=sort(unique(JP2_ec$year)))
JP2_ec$bmp <- ifelse(JP2_ec$year %in% c(2012,2014),"pre","post")
JP2_ec$bmp <- ifelse(JP2_ec$year %in% c(2015),"transition",JP2_ec$bmp)

Racine_ec <- Racine_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Racine")
Racine_ec$year <- as.POSIXlt(Racine_ec$pdate)$year + 1900
Racine_ec$year <- factor(Racine_ec$year,levels=sort(unique(Racine_ec$year)))


df_ec <- full_join(JP1_ec,JP2_ec) %>%
  full_join(Racine_ec)


# ggplot(df_ec,aes(y=Ecoli,x=year,group = year)) +
#   geom_boxplot() +
#   scale_y_continuous(trans="log10")+
#   facet_wrap(~beach,scales = "free",nrow=3)+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Make boxplots

  # Define number of observations
n_fun <- function(x){
  return(data.frame(y = 0.95*log10(10^4),
                    label = length(x)))
  
}

j1_boxplot <- ggplot(JP1_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_log10("y",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  annotate(geom = "text",label = "Jeorse 1",fontface = "bold",x= 3,y=10^4.3) + 
  annotate(geom = "text",label = "n = ",x= 0.7, y=10^3.8,size = 3) + 
  theme(axis.title = element_blank()) +
  stat_summary(fun.data = n_fun, geom = "text",hjust = 0.5,size = 3,
               #aes(group=season),
               position = position_dodge(0.9))

  # labs(
  #   x = "",
  #   y = "*E. coli*") +
  # theme(axis.title.y = ggtext::element_markdown())

j2_boxplot <- ggplot(JP2_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_log10("y",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  annotate(geom = "text",label = "Jeorse 2",fontface = "bold",x= 3,y=10^4.3) + 
  annotate(geom = "text",label = "n = ",x= 0.7, y=10^3.8,size = 3) + 
  theme(axis.title = element_blank()) +
  stat_summary(fun.data = n_fun, geom = "text",hjust = 0.5,size = 3,
               #aes(group=season),
               position = position_dodge(0.9))


# Define number of observations
n_fun <- function(x){
  return(data.frame(y = log10(10^4.9),
                    label = length(x)))
  
}

racine_boxplot <- ggplot(Racine_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_log10("y",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  annotate(geom = "text",label = "Racine North",fontface = "bold",x= 12,y=10^5.5) + 
  annotate(geom = "text",label = "n = ",x= 0.8, y=10^5.3,size = 3) + 
  theme(axis.title = element_blank()) +
  stat_summary(fun.data = n_fun, geom = "text",hjust = 0.5, size = 3,
               #aes(group=season),
               position = position_dodge(0.9))

#Combine plots into one
yaxislabel=text_grob("E. coli", size = 15, face = "italic",rot = 90) 
xaxislabel=text_grob("Year", size = 15) 
grid_out <- grid.arrange(grobs = list(j1_boxplot, j2_boxplot,racine_boxplot), 
                          widths = c(1,1), heights = c(1,1), 
                          layout_matrix = rbind(c(1,2),
                                                c(3,3)),
                          left = yaxislabel, bottom = xaxislabel)

#Save final figure

ggsave(filename = file.path("50_Visualize","out",filenm),plot = grid_out)


jp2015 <- JP1_model %>% filter(pdate < as.POSIXct("2015-10-06") & pdate > as.POSIXct("2015-05-01"))
jp2015 <- jp2015 %>% mutate(bmp = ifelse(pdate > as.POSIXct("2015-07-06") & pdate < as.POSIXct("2015-08-01"),"Dogs","No dogs"))
boxplot(Ecoli~bmp, data = jp2015,main = "Jeorse 1 2015")

