.onLoad <- function(libname, pkgname) {
  model_path <- system.file("data", "NNtest.pt", package = "emphasis")
  assign("cnn_ltt", torch::torch_load(model_path), envir = .GlobalEnv)
  

}

compute_dim_ouput_flatten_cnn <- function(n_input, n_layer, kernel_size = 2){
  
  k <- kernel_size - 1 
  
  for (i in 1:n_layer){
    n_input <- as.integer((n_input - k)/2)
  }
  
  return(n_input)
  
}