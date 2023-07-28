evaluate_and_plot <- function(model, test_dl) {
  # Create empty lists to store the outputs and the true labels
  outputs <- list()
  true_labels <- list()
  
  # Set the model to evaluation mode
  model$eval()
  
  # Iterate over the test DataLoader
  coro::loop(for (b in test_dl) {
    # Get the data and labels from the current batch
    data <- b$x
    labels <- b$y
    
    # Use the model to get the output for the current batch
    output <- model(data)
    
    # Append the output and the labels to their respective lists
    outputs <- c(outputs, as.array(output$detach()))
    true_labels <- c(true_labels, as.array(labels))
  })
  
  # Convert the lists to vectors
  outputs <- do.call(rbind, outputs)
  true_labels <- do.call(rbind, true_labels)
  
  # Plot true vs predicted for each output
  par(mfrow = c(1, ncol(outputs))) # Arrange the plots in 1 row and as many columns as there are outputs
  for (i in seq_len(ncol(outputs))) {
    plot(true_labels[, i], outputs[, i], main = paste("True vs Predicted for output", i), 
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
