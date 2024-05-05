evaluate_and_plot <- function(model, test_dl,names,n_out,device) {
  # Create empty lists to store the outputs and the true labels
  
  outputs <- vector(mode = "list", length = n_out)
  names(outputs) <- names
  
  true_labels <- vector(mode = "list", length = n_out)
  names(true_labels) <- names
  
  # Set the model to evaluation mode
  model$eval()
  
    # Compute predictions 
    coro::loop(for (b in test_dl) {
      out <- model(b$x$to(device = device))
      pred <- as.numeric(out$to(device = "cpu")) # move the tensor to CPU 
      true <- as.numeric(b$y)
      for (i in 1:n_out){
        outputs[[i]] <- c(outputs[[i]], pred[i])
        true_labels[[i]] <- c(true_labels[[i]], true[i])
        
        }
    })
  
  # Plot true vs predicted for each output
  par(mfrow = c(1, length(outputs))) # Arrange the plots in 1 row and as many columns as there are outputs
  for (i in seq_len(length(outputs))) {
    plot(true_labels[[ i]], outputs[[ i]], main = paste("True vs Predicted for output ", names[i]), 
         xlab = "True values", ylab = "Predicted values")
    abline(0, 1) # Adds a line representing a perfect prediction
  }
}




evaluate_and_plot <- function(model, test_dl,names,n_out,device) {
  # Create empty lists to store the outputs and the true labels
  
  outputs <- vector(mode = "list", length = n_out)
  names(outputs) <- names
  
  true_labels <- vector(mode = "list", length = n_out)
  names(true_labels) <- names
  
  # Set the model to evaluation mode
  model$eval()
  
  # Compute predictions 
  coro::loop(for (b in test_dl) {
    out <- model(b$x$to(device = device))
    pred <- as.numeric(out$to(device = "cpu")) # move the tensor to CPU 
    true <- as.numeric(b$y)
    for (i in 1:n_out){
      outputs[[i]] <- c(outputs[[i]], pred[i])
      true_labels[[i]] <- c(true_labels[[i]], true[i])
      
    }
  })
  
  # Plot true vs predicted for each output
  par(mfrow = c(1, length(outputs))) # Arrange the plots in 1 row and as many columns as there are outputs
  for (i in seq_len(length(outputs))) {
    plot(true_labels[[ i]], outputs[[ i]], main = paste("True vs Predicted for output ", names[i]), 
         xlab = "True values", ylab = "Predicted values")
    abline(0, 1) # Adds a line representing a perfect prediction
  }
}



evaluate_and_plot <- function(model, test_dl,names,n_out=4,device = "cpu") {

  outputs <- NULL
  
  true_labels <- NULL
  
  outputs <- vector(mode = "list", length = n_out)
  names(outputs) <- names
  
  true_labels <- vector(mode = "list", length = n_out)
  names(true_labels) <- names
  
  # Set the model to evaluation mode
  model$eval()
  
  # Compute predictions 
  coro::loop(for (b in test_dl) {
    out <- model(b$x$to(device = device))
    pred <- as.numeric(out$to(device = "cpu")) # move the tensor to CPU 
    true <- as.numeric(b$y)
    for (i in 1:n_out){
      outputs[[ i]] <- c(outputs[[ i]], pred[i])
      true_labels[[ i]] <- c(true_labels[[ i]], true[i])
      
    }
  })
  
  
  
  # Plot true vs predicted for each output
  par(mfrow = c(1, length(outputs))) # Arrange the plots in 1 row and as many columns as there are outputs
  for (i in seq_len(length(outputs))) {
    hist((true_labels[[ i]]-outputs[[ i]])/true_labels[[ i]], main = paste("relative error for output ", names[i]), 
         xlab = "Error",breaks=100)
  }
  
  
}



evaluate_and_plot <- function(model, test_dl,names,n_out,device) {
  # Create empty lists to store the outputs and the true labels
  
  outputs <- vector(mode = "list", length = n_out)
  names(outputs) <- names
  
  true_labels <- vector(mode = "list", length = n_out)
  names(true_labels) <- names
  
  
  # Set the model to evaluation mode
  model$eval()
  
  
  
  
  
  # Compute predictions 
  coro::loop(for (b in test_dl) {
    out <- model(b$x$to(device = device))
    pred <- as.numeric(out$to(device = "cpu")) # move the tensor to CPU 
    true <- as.numeric(b$y)
    for (i in 1:n_out){
      outputs[[i]] <- c(outputs[[i]], pred[i])
      true_labels[[i]] <- c(true_labels[[i]], true[i])
      
    }
  })
  
  
  
  # Plot true vs predicted for each output
  par(mfrow = c(1, length(outputs))) # Arrange the plots in 1 row and as many columns as there are outputs
  for (i in seq_len(length(outputs))) {
    plot(true_labels[[ i]], outputs[[ i]], main = paste("True vs Predicted for output ", names[i]), 
         xlab = "True values", ylab = "Predicted values")
    abline(0, 1) # Adds a line representing a perfect prediction
  }
  
  
}



plot_loss <- function(train_losses, valid_losses) {
  plot(1:length(train_losses), train_losses, type = "l", col = "blue",
       xlab = "Epoch", ylab = "Loss", main = "Training and Validation Loss",
       ylim = range(c(train_losses, valid_losses)))
  lines(1:length(valid_losses), valid_losses, type = "l", col = "red")
  legend("topright", legend = c("Training Loss", "Validation Loss"),
         col = c("blue", "red"), lty = 1)
}
