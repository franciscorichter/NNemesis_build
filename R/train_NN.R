train_NN <- function(df.ltt, param, n_trees, nn_type = "cnn-ltt", n_hidden = 8, n_layer = 3, 
                     ker_size = 5, p_dropout = 0.01, n_input = max_n_taxa, 
                     n_out = 3, n_epochs = 1000, patience = 100, seed = 1234) {
  
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
      n_flatten <- compute_dim_ouput_flatten_cnn(n_input,n_layer,ker_size)
      self$fc1 <- nn_linear(in_features = n_flatten*(2*2*n_hidden) , out_features = 100)
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
  device = "cpu"
  cnn_ltt$to(device = device)
  
  opt <- optim_adam(params = cnn_ltt$parameters)
  
  train_batch <- function(b){
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
    if (last_loss <= mean(valid_loss)){
      trigger <- trigger + 1
    }else{
      trigger <- 0
      last_loss <- mean(valid_loss)
    }
    cat(sprintf(" - patience: %0.1d/%0.1d", trigger, patience))
    cat("\n")
    epoch <- epoch + 1
    if (trigger == patience){
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
  
  model_path <- paste("NNs/DDD-", n_trees, ".pt", sep = "")
  torch::torch_save(cnn_ltt, model_path)
  save.image(file=paste("NNs/DDD-", n_trees, ".RData", sep = ""))
  cat(paste("\n Model cnn ltt saved at", model_path))
  cat("\nSaving model... Done.")
  results <- list(
    model = cnn_ltt,
    train_losses = train_losses,
    valid_losses = valid_losses,
    test_loss = mean(test_loss)
  )
  return(results)
}
