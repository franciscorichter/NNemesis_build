# Set parameters
n_trees = 1000
lambda_interval <- c(0, 50)
K_interval <- c(15,400)

# Generate data
out = NNemesis.Build:::generatePhyloDDD(n_trees = n_trees,
                                        lambda_interval = lambda_interval,
                                        K_interval = K_interval)

#fi = paste0(n_trees,"_",lambda_interval[1],"_",lambda_interval[2],"_",K_interval[1],"_",K_interval[2],".RData",sep="")
trees = out$trees
param = out$param
num_nodes = sapply(trees, function(list) list$Nnode)
max_n_taxa = max(num_nodes)

df.ltt <- NNemesis:::generate_ltt_dataframe(trees = trees, n_taxa = max_n_taxa)

# Convert to dataset
ds.ltt <- convert_ltt_dataframe_to_dataset_orig(df.ltt, param)

# Train the neural network
tr = train_NN(df.ltt = df.ltt, param = param, n_trees = n_trees, n_input = max_n_taxa, seed = 1234)

plot_loss(train_losses, valid_losses)

evaluate_and_plot(model = cnn_ltt, test_dl = test_dl)
