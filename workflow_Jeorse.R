

source("10_load_data/src/get_data.R")
source("10_load_data/src/addPdate.R")

source("20_process_data/src/processBeachData.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_Jeorse.R")

dfJeorse <- get_data(raw_directory = "./10_load_data/raw/Jeorse/",
         filenm= "E coli 2010_2017-variables.xlsx",
         sheet = "E coli 2010_2017-variables",
         siteCol = "Location")

dfJeorse_processed <- process_Jeorse(dfJeorse)
JP1 <- split_Jeorse(dfJeorse_processed,"JP1")
JP2 <- split_Jeorse(dfJeorse_processed,"JP2")

# plot_variable_availability(df63,"pdate","_unprocessed")
# plot_variable_availability(df,"pdate","_processed")

