

source("10_load_data/src/get_data.R")
source("10_load_data/src/addPdate.R")
source("10_load_data/src/get_63rd_data.R")

source("20_process_data/src/processBeachData.R")
source("20_process_data/src/fxn_cart2polar_polar2cart_df.R")
source("20_process_data/src/process_63rd.R")


df63rd <- process_63rd(df63)
plot_variable_availability(df63,"pdate","63rd_unprocessed")
plot_variable_availability(df63rd,"pdate","63rd_processed")

