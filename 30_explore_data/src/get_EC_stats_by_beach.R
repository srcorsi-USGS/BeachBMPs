# Figure xx-model_boxplot

library(tidyverse)


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


df_JP <- full_join(JP1_ec,JP2_ec)

JP_medians <- df_JP %>%
  group_by(beach,bmp) %>%
  summarize(median_EC = median(Ecoli))
 
PrePost <- c(1997,2000,2004,2010)
Racine_ec$year <- as.POSIXlt(Racine_ec$pdate)$year + 1900
Racine_ec$year <- factor(Racine_ec$year,levels=sort(unique(Racine_ec$year)))
Racine_ec$bmp <- ifelse(Racine_ec$year %in% PrePost[1:2],"pre","post")
Racine_ec$bmp <- ifelse(Racine_ec$year %in% c((PrePost[2]+1):(PrePost[3]-1)),"transition",Racine_ec$bmp)

test <
