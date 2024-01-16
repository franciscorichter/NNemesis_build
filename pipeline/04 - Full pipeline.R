# Parameters for generatePhyloPD
time0 = proc.time()
n_trees <- 100000

mu_interval <- c(0.1, 20)
lambda_interval <- c(2, 120)
betaN_interval <- c(-5, 0)
betaP_interval <- c(-5, 5)

# # Generate data
out = generatePhyloPD(
  n_trees = n_trees,
  mu_interval = mu_interval,
  lambda_interval = lambda_interval,
  betaN_interval = betaN_interval,
  betaP_interval = betaP_interval
)
time1 = proc.time()
trees = out$trees
param = out$param
num_nodes = (unlist(sapply(trees, function(list) list$Nnode)))
max_n_taxa = max(num_nodes)
summary(num_nodes)

null_trees = NULL
for(i in 1:length(trees)){
  tr = trees[[i]]
  if(is.null(tr)) null_trees = c(null_trees,i)
}

null_trees
non_null_indices = c() # Create an empty vector to store indices of non-null trees
for(i in 1:length(trees)){
  tr = trees[[i]]
  if(!is.null(tr)) non_null_indices = c(non_null_indices,i) # Collect indices of non-null values
}

length(trees)
# Subset trees and param using non_null_indices
trees <- trees[non_null_indices]
n_trees = length(trees)


#pars <- data.frame(mu=param$mu[non_null_indices],
 #                  lambda=param$lambda[non_null_indices],
  #                 betaN=param$betaN[non_null_indices],
   #                betaP = param$betaP[non_null_indices])
#qplot(null_pars$mu,null_pars$lambda)
#qplot(null_pars$betaN,null_pars$betaP)

#> summary(num_nodes)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.0    17.0    29.0   122.3    68.0 10723.0 

# if we don't need all those numbers of species, we can process this checking the parameters of importance
#library(ggplot2)
#qplot(x=param$mu,y=param$lambda,size = num_nodes)

df.ltt <- generate_ltt_dataframe(trees = trees, n_taxa = max_n_taxa)

# Convert to dataset
ds.ltt <- convert_ltt_dataframe_to_dataset_orig(df.ltt, param)

  # Train the neural network
#library(torch)
tr = train_NN(df.ltt = df.ltt, 
                  param = param, 
                  n_trees = n_trees, 
                  n_input = max_n_taxa, 
                  seed = 1234,n_out = 4)


plot_loss(tr$train_losses, tr$valid_losses)

#ev = evaluate_and_plot(model = cnn_ltt, test_dl = test_dl)
