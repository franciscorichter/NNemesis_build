convert_trees_to_ltt_dataframe <- function(trees, n_taxa){
  # Check if input arguments are valid
  if(!is.list(trees)){
    stop("Input trees is not a valid list.")
  }
  if(!is.numeric(n_taxa) || n_taxa <= 0){
    stop("Input n_taxa is not a positive integer.")
  }
  
  n_trees  <- length(trees) # number of trees 
  n_row <- ifelse(length(n_taxa) == 1, n_taxa, n_taxa[2])
  df.ltt <- data.frame("tree1" = rep(NA, n_row))
  
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


convert_ltt_dataframe_to_dataset <- function(df.ltt, nn_type = "cnn-ltt"){
  # Check if input arguments are valid
  if(!is.data.frame(df.ltt)){
    stop("Input df.ltt is not a valid data frame.")
  }
  if(!is.character(nn_type) || !(nn_type %in% c("cnn-ltt", "other"))){
    stop("Input nn_type is not a valid string ('cnn-ltt' or 'other').")
  }
  
  if (nn_type == "cnn-ltt"){
    ds.ltt <- torch::dataset(
      name <- "ltt_dataset", 
      initialize = function(df.ltt){
        # input
        df.ltt[is.na(df.ltt)] <- 0
        array.ltt <- df.ltt %>% 
          as.matrix() %>% 
          torch_tensor()
        self$x <- array.ltt
      }, 
      
      .getitem = function(i) {list(x = self$x[,i]$unsqueeze(1))},
      
      .length = function() {self$x$size()[[2]]}
    )
  }
  
  else{
    ds.ltt <- torch::dataset(
      name <- "ltt_dataset", 
      initialize = function(df.ltt){
        # input
        df.ltt[is.na(df.ltt)] <- 0
        array.ltt <- df.ltt %>% 
          as.matrix() %>% 
          torch_tensor()
        self$x <- array.ltt
      }, 
      
      .getitem = function(i) {list(x = self$x[,i])},
      
      .length = function() {self$x$size()[[2]]}
    )
  }
  
  return(ds.ltt)
}

convert_ltt_dataframe_to_dataset_full <- function(ltt_full, nn_type = "cnn-ltt"){
  ltt_full = ltt_full$df.ltt
  param = ltt_full$param
  # Check if input arguments are valid
  if(!is.data.frame(df.ltt)){
    stop("Input df.ltt is not a valid data frame.")
  }

    ds.ltt <- torch::dataset(
      name <- "ltt_dataset", 
      initialize = function(df.ltt, param){
        
        # input
        df.ltt[is.na(df.ltt)] <- 0
        
        array.ltt <- df.ltt %>% 
        as.matrix() %>% 
        torch_tensor()
        self$x <- array.ltt
        
        # target 
        self$y <- torch_tensor(do.call(cbind, param)) # target
      }, 
      
      .getitem = function(i) {list(x = self$x[,i], y = self$y[i, ])},
      
      .length = function() {self$y$size()[[1]]}
    )
  
  
  
  return(ds.ltt)
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


extract_elements <- function(list_of_vectors, indices_to_extract){
  l <- as.list(do.call(cbind, list_of_vectors)[indices_to_extract,] 
               %>% as.data.frame())
  return(l)
}


compute_dim_ouput_flatten_cnn <- function(n_input, n_layer, kernel_size = 2){
  
  k <- kernel_size - 1 
  
  for (i in 1:n_layer){
    n_input <- as.integer((n_input - k)/2)
  }
  
  return(n_input)
  
}
