## Data pre-processing 
rm(list = ls())
n_trees = 100
out <- readRDS(file = paste("data/simulations/DDD-", n_trees, ".rds", sep = ""))
trees = out$trees
param = out$param
num_nodes = sapply(trees, function(list) list$Nnode)
max_n_taxa = max(num_nodes)

df.ltt <- NNemesis:::generate_ltt_dataframe(trees = trees, 
                                            n_taxa = max_n_taxa)

#df.rates <- as.data.frame(do.call(cbind, param))
#df.ltt_full <- list("ltt" = df.ltt, "rates" = df.rates)

#ds.ltt <- NNemesis:::convert_ltt_dataframe_to_dataset_full(ltt_full = df.ltt_full)

ds.ltt <- NNemesis:::convert_ltt_dataframe_to_dataset_orig(df.ltt,param)


