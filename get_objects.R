library(remake)
make()

df63rd <- make("df63rd_processed")
dfJeorse1 <- make("dfJeorse1_processed")
dfJeorse2 <- make("dfJeorse2_processed")

#plotVarsTS(junk,"test")

make("20_process_data/figures/data_available_63rd.pdf")
make("20_process_data/figures/data_available_63rd_processed.pdf")


df <- make("dfJeorse1_processed")
testModels <- make("models_Jeorse1")
model <- testModels[[1]]
make("40_modeling/figures/Jeorse1_model_graphs.pdf")
