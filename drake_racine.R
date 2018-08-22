library(drake)
library(tools)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

source("10_load_data/src/get_Racine_data.R")
source("20_process_data/src/fxn_cart2polar_polar2cart.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_Racine.R")

plan <- drake_plan(
  dfNB = read.csv(file_in("10_load_data/raw/Racine/North Beach Historical Database 1995-2016, COMBINED EDIT.csv"),
                  stringsAsFactors = FALSE, 
                  na.strings = c('NR','#N/A', 'NC', 'No Data','N/A','NA')),
  
  dfNBIVs = read.csv(file_in("10_load_data/raw/Racine/IVs.csv"),stringsAsFactors = FALSE),
  
  dfRacine = get_Racine(df=dfNB,dfIVs=dfNBIVs),
  
  dfModel = process_Racine(dfRacine)
  
)
#plan

make(plan)

dfRacine <- readd(dfRacine)
dfIVs <- readd(dfNBIVs)
df.orig <- readd(dfModel)




config <- drake_config(plan)
vis_drake_graph(config)


dfRacine <- get_Racine()



