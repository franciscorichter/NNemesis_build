n_trees = 100
lambda_interval <- c(0, 50) # speciation rate
K_interval <- c(15,400)

out = NNemesis.Build:::generatePhyloDDD(n_trees = n_trees,
                 lambda_interval = lambda_interval,
                 K_interval = K_interval)

fi = paste0(n_trees,"_",lambda_interval[1],"_",lambda_interval[2],"_",K_interval[1],"_",K_interval[2],".RData",sep="")
save.image(file=fi)
#saveRDS(out, file = paste("data/simulations/DDD-", n_trees, ".rds", sep = ""))




