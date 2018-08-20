library(drake)

source("10_load_data/src/get_Racine_data.R")

plan <- drake_plan(
  Racine_data = get_Racine()
)
plan

make(plan)

dfRacine <- readd(Racine_data)



config <- drake_config(plan)
vis_drake_graph(config)
