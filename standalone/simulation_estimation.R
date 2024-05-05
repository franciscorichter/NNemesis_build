source("functionsNN.R")

# Parameters for generatePhyloPD
time0 = proc.time()
n_trees <- 1000

mu_interval <- c(0, 30)
lambda_interval <- c(0, 80)
betaN_interval <- c(-3, 0)
betaP_interval <- c(0, 0)

# # Generate data
out = generatePhyloPD(
  n_trees = n_trees,
  mu_interval = mu_interval,
  lambda_interval = lambda_interval,
  betaN_interval = betaN_interval,
  betaP_interval = betaP_interval,
  max_lin = 100000
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

ct = curateTreeData(trees,param)

trees = ct$trees
param = ct$params

num_nodes = (unlist(sapply(ct$trees, function(list) list$Nnode)))
max_n_taxa = max(num_nodes)
n_trees = length(trees)

df.ltt <- generate_ltt_dataframe(trees = trees, n_taxa = max_n_taxa)

neural_net = train_NN(df.ltt = df.ltt, 
                      param = param, 
                      n_trees = length(trees), 
                      n_input = max_n_taxa, 
                      seed = 1234,n_out = 4)


data_filename <- paste0("NN_simulation_estimation_", n_trees, "_", mu_interval[1], "_", lambda_interval[1], "_", betaN_interval[1], "_", betaP_interval[1], "_", timestamp, ".RData")
save.image(file = data_filename)
