library(tools)

source("10_load_data/src/get_Racine_data.R")
source("20_process_data/src/fxn_cart2polar_polar2cart.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_Racine.R")

dfNB <- read.csv("10_load_data/raw/Racine/North Beach Historical Database 1995-2016, COMBINED EDIT.csv",
                 stringsAsFactors = FALSE, 
                 na.strings = c('NR','#N/A', 'NC', 'No Data','N/A','NA'))

dfNBIVs <- read.csv("10_load_data/raw/Racine/IVs.csv",stringsAsFactors = FALSE)

dfRacine <- get_Racine(df=dfNB,dfIVs=dfNBIVs)

dfModel <- process_Racine(dfRacine)



