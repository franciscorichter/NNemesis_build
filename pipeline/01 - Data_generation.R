n_trees = 100
lambda_interval <- c(0, 50) # speciation rate
K_interval <- c(15,400)

out = NNemesis:::generatePhyloDDD(n_trees = n_trees,
                 lambda_interval = lambda_interval,
                 K_interval = K_interval)

#saveRDS(out, file = paste("data/simulations/DDD-", n_trees, ".rds", sep = ""))




