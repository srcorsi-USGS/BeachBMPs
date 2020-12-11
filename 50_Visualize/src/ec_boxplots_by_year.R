# Figure xx-model_boxplot

library(tidyverse)
library(gridExtra)
library(ggtext)
library(ggpubr)

JP1_model <- readRDS(file = file.path("40_modeling","out","Jeorse1_model_df.rds"))
JP2_model <- readRDS(file = file.path("40_modeling","out","Jeorse2_model_df.rds"))
Racine_model <- readRDS(file = file.path("40_modeling","out","Racine_model_df.rds"))

JP1_ec <- JP1_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Jeorse 1")
JP1_ec$year <- as.POSIXlt(JP1_ec$pdate)$year + 1900
JP1_ec$year <- factor(JP1_ec$year,levels=sort(unique(JP1_ec$year)))

JP2_ec <- JP2_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Jeorse 2")
JP2_ec$year <- as.POSIXlt(JP2_ec$pdate)$year + 1900
JP2_ec$year <- factor(JP2_ec$year,levels=sort(unique(JP2_ec$year)))

Racine_ec <- Racine_model %>% select(pdate,Ecoli) %>%
  mutate(beach = "Racine")
Racine_ec$year <- as.POSIXlt(Racine_ec$pdate)$year + 1900
Racine_ec$year <- factor(Racine_ec$year,levels=sort(unique(Racine_ec$year)))


df_ec <- full_join(JP1_ec,JP2_ec) %>%
  full_join(Racine_ec)


ggplot(df_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")+
  facet_wrap(~beach,scales = "free",nrow=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

j1_boxplot <- ggplot(JP1_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom = "text",label = "Jeorse 1",x= 3,y=10000) + 
  theme(axis.title = element_blank())

  # labs(
  #   x = "",
  #   y = "*E. coli*") +
  # theme(axis.title.y = ggtext::element_markdown())

j2_boxplot <- ggplot(JP1_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom = "text",label = "Jeorse 2",x= 3,y=10000)+ 
  theme(axis.title = element_blank())
# 
#   labs(
#     x = "",y="") +
#   theme(axis.title.y = ggtext::element_markdown())

racine_boxplot <- ggplot(Racine_ec,aes(y=Ecoli,x=year,group = year)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(geom = "text",label = "Racine North",x= 12,y=20000) +
  theme(axis.title = element_blank())
# 
#   labs(
#     x = "",
#     y = "*E. coli*") +
#   theme(axis.title.y = ggtext::element_markdown())

yaxislabel=text_grob("E. coli", size = 15, face = "italic") 

grid_out2 <- grid.arrange(grobs = list(j1_boxplot, j2_boxplot,racine_boxplot), 
                          widths = c(1,1), heights = c(1,1), 
                          layout_matrix = rbind(c(1,2),
                                                c(3,3)),
                          left = yaxislabel, bottom = "Year")

                          

