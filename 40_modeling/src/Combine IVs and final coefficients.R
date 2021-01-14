# summarize variables for all models

library(tidyverse)

#Read models
models <- list()
IVs <- list()

models[[1]] <- readRDS(file = file.path("40_modeling","out","Jeorse1_model.rds"))
IVs[[1]] <- readRDS(file = file.path("40_modeling","out","Jeorse1_IVs.rds"))
length(models)

models[[2]] <- readRDS(file = file.path("40_modeling","out","Jeorse2_model.rds"))
length(models)

racine_models <- readRDS(file = file.path("40_modeling","out","Racine_model.rds"))
IVs[[2]] <- readRDS(file = file.path("40_modeling","out","Racine_IVs.rds"))

for(i in 3:6){models[[i]] <- racine_models[[i-2]]}
length(models)

names(IVs) <- c("Jeorse","Racine")

names(models) <- c("Jeorse 1","Jeorse 2", "Racine 1","Racine 2", "Racine 3", "Racine 4")

IVs[[1]]$Beach <- "Jeorse Beach"
IVs[[2]]$Beach <- "North Beach"

# Combine model coefficients

beaches <- c("Jeorse 1","Jeorse 2","Racine","Racine","Racine","Racine")
model_num <- c(1,1,1:4)

DataTestPeriod <- list(as.POSIXlt(c("2012-05-01","2014-10-01","2016-05-01","2017-10-01"))$year + 1900,
                       as.POSIXlt(c("2012-05-01","2014-10-01","2016-05-01","2017-10-01"))$year + 1900,
                       as.POSIXlt(c("1997-01-01","2000-12-31","2016-01-01","2019-12-31"))$year + 1900,
                       as.POSIXlt(c("1997-01-01","2000-09-30","2005-01-01","2007-12-31"))$year + 1900,
                       as.POSIXlt(c("2005-01-01","2007-12-31","2011-01-01","2013-12-31"))$year + 1900,
                       as.POSIXlt(c("2010-01-01","2013-12-31","2014-01-01","2017-12-31"))$year + 1900)

df_coefficents <- data.frame(Coefficient = NULL,Beach = NULL,pre = NULL, post = NULL)

for(i in 1:length(models)) {
  coef(models[[i]])[-1]
  dftemp <- as.data.frame(round(coef(models[[i]])[-1],2))
  names(dftemp) <- "Coefficient"
  dftemp$Beach <- beaches[i]
  dftemp$pre <- paste(DataTestPeriod[[i]][1], "-" , DataTestPeriod[[i]][2])
  dftemp$post <- paste(DataTestPeriod[[i]][3], "-" , DataTestPeriod[[i]][4])
  
    df_coefficents <- rbind(df_coefficents,dftemp)
}
  
Jeorse1_coef <- coef(models[[1]])[-1]
Jeorse2_coef <- coef(models[[2]])[-1]


write.csv(df_coefficents, file = file.path("40_modeling","out","final_model_coefficients.csv"))

IVs_all <- rbind(IVs[[1]],IVs[[2]])

write.csv(IVs_all, file = file.path("40_modeling","out","IVs_all.csv"), row.names = FALSE)

#combine Jeorse model data

JP1 <- readRDS(file.path("40_modeling","out","Jeorse1_model_df.rds"))
JP2 <- readRDS(file.path("40_modeling","out","Jeorse2_model_df.rds"))
names(JP1)[4] <- "EColi1"
names(JP2)[4] <- "EColi2"

JP <- full_join(JP1[,-c(1,2)],JP2[,-c(1,2)])

write.csv(JP,file.path("40_modeling","out","Jeorse_1_2_model_df.csv"),row.names = FALSE)               
