#' @import magrittr

# Improved DDD_par_est function
DDD_est <- function(tree, cnn_ltt, max_nodes_rounded = 550, device = "cpu"){
  # Check if tree is a valid phylogenetic tree
  if(!inherits(tree, "phylo")){
    stop("Input tree is not a valid phylogenetic tree.")
  }
  cnn_ltt$eval()
  # Check if cnn_ltt is a valid model
  if(!inherits(cnn_ltt, "nn_module")){
    stop("Input cnn_ltt is not a valid torch model.")
  }
  
  ltt.coord <- ape::ltt.plot.coords(tree) # get ltt coordinates 
  ltt.coord <- as.data.frame(ltt.coord)
  ltt.coord.time <- ltt.coord$time
  n <- length(ltt.coord.time)
  
  df.ltt <- data.frame("tree1" = rep(NA, max_nodes_rounded))
  df.ltt[1:n,1] <- ltt.coord$time
  
  crown_time = max(abs(df.ltt[!is.na(df.ltt)]))
  df.ltt = df.ltt / crown_time
  
  ds.ltt_new <- convert_ltt_dataframe_to_dataset(df.ltt)

  ds_eval  <- ds.ltt_new(df.ltt)
  
  data_loader_ltt <- ds_eval  %>% torch::dataloader(batch_size=1, shuffle=FALSE)
  
  cnn_ltt$eval()
  
  coro::loop(for (b in data_loader_ltt) {
    out <- cnn_ltt(b$x$to(device = device))
    pred <- as.numeric(out$to(device = "cpu")) # move the tensor to CPU 
  })

  
  par_estim = pred/crown_time
  
  return(par_estim)
}
