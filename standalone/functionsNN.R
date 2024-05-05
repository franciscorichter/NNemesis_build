library(torch)

train_NN <- function(df.ltt, param, n_trees, nn_type = "cnn-ltt", n_hidden = 8, n_layer = 3, 
                     ker_size = 5, p_dropout = 0.01, n_input, 
                     n_out, n_epochs = 1000, patience = 50, seed = 1234, device="cpu") {
  
  compute_flattened_size <- function(n_input, n_layer, ker_size) {
    for (i in 1:n_layer) {
      n_input <- n_input - ker_size + 1
      n_input <- floor(n_input / 2)  # assuming stride of 2 for avg_pool1d
    }
    return(n_input)
  }
  
  set.seed(seed)
  torch_manual_seed(seed)
  
  n_train    <- floor(n_trees * .9)
  n_valid    <- floor(n_trees * .05)
  n_test     <- n_trees - n_train - n_valid
  batch_size <- 10
  
  subset_indices <- sample(1:n_trees, n_trees)
  train_indices <- subset_indices[1:n_train]
  valid_indices <- subset_indices[(n_train + 1):(n_train + n_valid)]
  test_indices  <- subset_indices[(n_train + n_valid + 1):n_trees]
  
  ds.ltt <- convert_ltt_dataframe_to_dataset_orig(df.ltt, param)
  
  train_ds <- ds.ltt(df.ltt[, train_indices], as.list(do.call(cbind, param)[train_indices,] %>% as.data.frame()))
  valid_ds <- ds.ltt(df.ltt[, valid_indices], as.list(do.call(cbind, param)[valid_indices,] %>% as.data.frame()))
  test_ds  <- ds.ltt(df.ltt[, test_indices], as.list(do.call(cbind, param)[test_indices,] %>% as.data.frame()))
  
  train_dl <- train_ds %>% dataloader(batch_size=batch_size, shuffle=TRUE)
  valid_dl <- valid_ds %>% dataloader(batch_size=batch_size, shuffle=FALSE)
  test_dl  <- test_ds  %>% dataloader(batch_size=1, shuffle=FALSE)
  
  
  cnn.net <- nn_module(
    
    "corr-cnn",
    
    initialize = function(n_input, n_out, n_hidden, n_layer, ker_size, p_dropout) {
      self$conv1 <- nn_conv1d(in_channels = 1, out_channels = n_hidden, kernel_size = ker_size)
      self$conv2 <- nn_conv1d(in_channels = n_hidden, out_channels = 2*n_hidden, kernel_size = ker_size)
      self$conv3 <- nn_conv1d(in_channels = 2*n_hidden, out_channels = 2*2*n_hidden, kernel_size = ker_size)
      n_flatten <- compute_dim_ouput_flatten_cnn(n_input, n_layer, ker_size)
      self$fc1 <- nn_linear(in_features = n_flatten * (2*2*n_hidden), out_features = 100)
      self$fc2 <- nn_linear(in_features = 100, out_features = n_out)
    },
    
    forward = function(x) {
      x %>% 
        self$conv1() %>%
        nnf_relu() %>%
        nnf_dropout(p = p_dropout) %>%
        nnf_avg_pool1d(2) %>%
        
        self$conv2() %>%
        nnf_relu() %>%
        nnf_dropout(p = p_dropout) %>%
        nnf_avg_pool1d(2) %>%
        
        self$conv3() %>%
        nnf_relu() %>%
        nnf_dropout(p = p_dropout) %>%
        nnf_avg_pool1d(2) %>%
        
        torch_flatten(start_dim = 2) %>%
        self$fc1() %>%
        nnf_relu() %>%
        nnf_dropout(p = p_dropout) %>%
        
        self$fc2()
    }
  )
  
  cnn_ltt <- cnn.net(n_input, n_out, n_hidden, n_layer, ker_size, p_dropout)
  cnn_ltt$to(device = device)
  
  opt <- optim_adam(params = cnn_ltt$parameters)
  
  train_batch <- function(b) {
    opt$zero_grad()
    output <- cnn_ltt(b$x$to(device = device))
    target <- b$y$to(device = device)
    
    loss <- nnf_mse_loss(output, target)
    loss$backward()
    opt$step()
    loss$item()
  }
  
  valid_batch <- function(b) {
    output <- cnn_ltt(b$x$to(device = device))
    target <- b$y$to(device = device)
    loss <- nnf_mse_loss(output, target)
    loss$item()
  }
  
  epoch <- 1
  trigger <- 0 
  last_loss <- 100
  train_losses <- list()
  valid_losses <- list()
  
  while (epoch < n_epochs & trigger < patience) {
    cnn_ltt$train()
    train_loss <- c()
    coro::loop(for (b in train_dl) {
      loss <- train_batch(b)
      train_loss <- c(train_loss, loss)
    })
    cat(sprintf("epoch %0.3d/%0.3d - train - loss: %3.5f", epoch, n_epochs, mean(train_loss)))
    train_losses <- append(train_losses, mean(train_loss))
    cnn_ltt$eval()
    valid_loss <- c()
    coro::loop(for (b in valid_dl) {
      loss <- valid_batch(b)
      valid_loss <- c(valid_loss, loss)
    })
    cat(sprintf(" - valid - loss: %3.5f", mean(valid_loss)))
    valid_losses <- append(valid_losses, mean(valid_loss))
    epsilon <- 0.001
    if (last_loss - mean(valid_loss) <= epsilon) {
      trigger <- trigger + 1
    } else {
      trigger <- 0
      last_loss <- mean(valid_loss)
    }
    
    cat(sprintf(" - patience: %0.1d/%0.1d", trigger, patience))
    cat("\n")
    epoch <- epoch + 1
    if (trigger == patience) {
      print("Early stopping triggered")
      break
    }
  }
  cnn_ltt$eval()
  test_loss <- c()
  coro::loop(for (b in test_dl) {
    loss <- valid_batch(b)
    test_loss <- c(test_loss, loss)
  })
  cat(sprintf("test - loss: %3.5f", mean(test_loss)))
  
  cat("\nSaving model... Done.")
  results <- list(
    model = cnn_ltt,
    train_losses = train_losses,
    valid_losses = valid_losses,
    test_loss = test_loss,
    test_dl = test_dl
  )
  
  ###Compute predictions on test
  return(results)
}

generatePhyloPD <- function(n_trees,
                            mu_interval,
                            lambda_interval,
                            betaN_interval,
                            betaP_interval,
                            max_lin = 1e+5,
                            max_tries = 10) {
  
  # Dynamically allocate lists
  trees <- list()
  extrees <- list()
  Lmats <- list()
  brds_s <- list()
  
  name.param <- c("mu", "lambda", "betaN", "betaP")
  true.param <- vector(mode='list', length=4)
  names(true.param) <- name.param
  
  for(j in 1:n_trees) {
    
    # Randomly sample parameter values
    lambda_sample <- runif(1, lambda_interval[1], lambda_interval[2])
    mu_sample <- runif(1, mu_interval[1], mu_interval[2])
    betaN_sample <- runif(1, betaN_interval[1], betaN_interval[2])
    betaP_sample <- runif(1, betaP_interval[1], betaP_interval[2])
    sim.param <- c(mu_sample, lambda_sample, betaN_sample, betaP_sample)
    
    outputs <- tryCatch({
      emphasis::sim_tree_pd_cpp(pars = sim.param, 
                                max_t = 1, 
                                max_lin = max_lin, 
                                max_tries = max_tries,
                                useDDD = TRUE)
    }, error = function(e) NULL)
    
    if (is.list(outputs) && max(outputs$brts) == 1) {
      trees[[length(trees) + 1]] <- outputs[[1]]
      extrees[[length(extrees) + 1]] <- outputs[[2]]
      Lmats[[length(Lmats) + 1]] <- outputs[[3]]
      brds_s[[length(brds_s) + 1]] <- outputs[[4]]
      for (i in 1:4) {
        true.param[[i]] <- c(true.param[[i]], sim.param[i])
      }
    }
    
    # Print progress
    svMisc::progress(j, n_trees, progress.bar = TRUE, init = (j == 1))
  }
  
  # Package and return results
  list(trees = trees, param = true.param, tas = extrees, L = Lmats, brts = brds_s)
}



compute_dim_ouput_flatten_cnn <- function(n_input, n_layer, kernel_size = 2){
  
  k <- kernel_size - 1 
  
  for (i in 1:n_layer){
    n_input <- as.integer((n_input - k)/2)
  }
  
  return(n_input)
  
}


convert_ltt_dataframe_to_dataset_orig<-function(df.ltt, true.param, nn_type= "cnn-ltt" ){
  
  if (nn_type == "cnn-ltt"){
    ds.ltt <- torch::dataset(
      name <- "ltt_dataset", 
      initialize = function(df.ltt, true.param){
        
        # input
        df.ltt[is.na(df.ltt)] <- 0
        
        array.ltt <- df.ltt %>% 
          as.matrix() %>% 
          torch_tensor()
        self$x <- array.ltt
        
        # target 
        self$y <- torch_tensor(do.call(cbind, true.param)) # target
      }, 
      
      .getitem = function(i) {list(x = self$x[,i]$unsqueeze(1), y = self$y[i, ])},
      
      .length = function() {self$y$size()[[1]]}
    )
  }
  
  else{
    ds.ltt <- torch::dataset(
      name <- "ltt_dataset", 
      initialize = function(df.ltt, true.param){
        
        # input
        df.ltt[is.na(df.ltt)] <- 0
        
        array.ltt <- df.ltt %>% 
          as.matrix() %>% 
          torch_tensor()
        self$x <- array.ltt
        
        # target 
        self$y <- torch_tensor(do.call(cbind, true.param)) # target
      }, 
      
      .getitem = function(i) {list(x = self$x[,i], y = self$y[i, ])},
      
      .length = function() {self$y$size()[[1]]}
    )
  }
  
  return(ds.ltt)
}


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



generate_ltt_dataframe <- function(trees, n_taxa){
  # Check if input arguments are valid
  if(!is.list(trees)){
    stop("Input trees is not a valid list.")
  }
  if(!is.numeric(n_taxa) || n_taxa <= 0){
    stop("Input n_taxa is not a positive integer.")
  }
  
  n_trees  <- length(trees) # number of trees 
  n_row <- ifelse(length(n_taxa) == 1, n_taxa, n_taxa[2])
  df.ltt <- data.frame("tree1" = rep(0, n_row))
  
  cat("Creating LTT dataframe...\n")
  
  for (i in 1:n_trees){
    tree <- trees[[i]] # get tree 
    ltt.coord <- ape::ltt.plot.coords(tree) # get ltt coordinates 
    ltt.coord <- as.data.frame(ltt.coord)
    ltt.coord.time <- ltt.coord$time
    n <- length(ltt.coord.time)
    df.ltt[1:n,paste("tree", i, sep = "")] <- ltt.coord$time
    svMisc:::progress(i, n_trees, progress.bar = TRUE, init = (i==1))
  }
  
  cat("\nCreating LTT dataframe... Done.")
  
  return(df.ltt) # function output
}


