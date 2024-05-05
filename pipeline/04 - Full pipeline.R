devtools:::install_github("franciscorichter/NNemesis_Build")
library("NNemesis")
# Parameters for generatePhyloPD
time0 = proc.time()
n_trees <- 10000

mu_interval <- c(0.1, 10)
lambda_interval <- c(2, 30)
betaN_interval <- c(-5, 0)
betaP_interval <- c(-5, 5)

# # Generate data
out = generatePhyloPD(
  n_trees = n_trees,
  mu_interval = mu_interval,
  lambda_interval = lambda_interval,
  betaN_interval = betaN_interval,
  betaP_interval = betaP_interval,
  max_lin = 10000
)
time1 = proc.time()
print(time1[3]-time0[3])

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
simulated_data_filename <- paste0("simulated_data_", n_trees, "_", mu_interval[1], "_", lambda_interval[1], "_", betaN_interval[1], "_", betaP_interval[1], "_", timestamp, ".RData")


log_filename <- paste0("simulation_log_", timestamp, ".txt")
log_info <- paste("Simulation Time: ", time1[3] - time0[3], 
                  "\nParameters: n_trees = ", n_trees, 
                  ", mu_interval = ", toString(mu_interval), 
                  ", lambda_interval = ", toString(lambda_interval), 
                  ", betaN_interval = ", toString(betaN_interval), 
                  ", betaP_interval = ", toString(betaP_interval),
                  "\nOutput File: ", simulated_data_filename,
                  "\n\n", sep = "")

out$log_info = log_info
save(out, file = simulated_data_filename)

trees = out$trees
param = out$param
num_nodes = (unlist(sapply(trees, function(list) list$Nnode)))
max_n_taxa = max(num_nodes)
summary(num_nodes)


curateTreeData <- function(trees, params, min_nodes = 7, max_percentile = 99.9) {
  # Calculate the number of nodes for each tree
  num_nodes <- sapply(trees, function(tree) tree$Nnode)
  
  # Determine the upper threshold for the number of nodes
  upper_threshold <- quantile(num_nodes, probs = max_percentile / 100)
  
  # Filter indices based on node count criteria
  valid_indices <- which(num_nodes >= min_nodes & num_nodes <= upper_threshold)
  
  # Filter trees and corresponding parameters
  curated_trees <- trees[valid_indices]
  curated_params <- lapply(params, function(param) param[valid_indices])
  
  return(list(trees = curated_trees, params = curated_params))
}

#pars <- data.frame(mu=param$m,
#                   lambda=param$lambda,
#                   betaN=param$betaN,
#                   betaP = param$betaP,
#                   num_nodes = num_nodes)

#sum(num_nodes>50)

#ggplot(pars[num_nodes>50,]) + geom_point(aes(x=mu,y=lambda,colour=num_nodes))
#ggplot(pars) + geom_point(aes(x=betaN,y=betaP))

#qplot(null_pars$mu,null_pars$lambda)
#qplot(null_pars$betaN,null_pars$betaP)

#> summary(num_nodes)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.0    17.0    29.0   122.3    68.0 10723.0 

# if we don't need all those numbers of species, we can process this checking the parameters of importance
#library(ggplot2)
#qplot(x=param$mu,y=param$lambda,size = num_nodes)

ct = curateTreeData(trees,param)

trees = ct$trees
param = ct$params

num_nodes = (unlist(sapply(ct$trees, function(list) list$Nnode)))
max_n_taxa = max(num_nodes)
n_trees = length(trees)

df.ltt <- generate_ltt_dataframe(trees = trees, n_taxa = max_n_taxa)

# Convert to dataset
ds.ltt <- convert_ltt_dataframe_to_dataset_orig(df.ltt, param)

  # Train the neural network
#library(torch)
neural_net = train_NN(df.ltt = df.ltt, 
                  param = param, 
                  n_trees = length(trees), 
                  n_input = max_n_taxa, 
                  seed = 12353,
                  n_out = 4,patience = 30)


data_filename <- paste0("NN_simulation_estimationn_", n_trees, "_", mu_interval[1], "_", lambda_interval[1], "_", betaN_interval[1], "_", betaP_interval[1], "_", timestamp, ".RData")
save.image(file = data_filename)



#plot_loss(neural_net$train_losses, neural_net$valid_losses)

#ev = evaluate_and_plot(model = cnn_ltt, test_dl = test_dl)

ep = evaluate_and_plot(model = neural_net$model, 
                       test_dl = neural_net$test_dl, 
                       names(param), 
                       4, "cpu") 
results$ep = ep

load("~/Library/CloudStorage/Dropbox/github/other_emphasis_versions/emphasisLD/data/FamilyBirdTrees.Rdata")
tree = FamilyBirdTrees$Parulidae$tree

trees = list(tree)
df.ltt <- generate_ltt_dataframe(trees = trees, n_taxa = max_n_taxa)

# Convert to dataset
ds.ltt <- convert_ltt_dataframe_to_dataset_orig(df.ltt, param)

# Convert your data to a torch tensor
new_data_tensor <- torch::torch_tensor(ds.ltt, device = "cpu")
# Set the model to evaluation mode
model$eval()

# Pass the data through the model
predictions <- model(new_data_tensor)

# If your output is a tensor, convert it to a more user-friendly format
# For example, convert it to a numeric vector or a dataframe
predictions <- as.numeric(predictions$to(device = "cpu"))

