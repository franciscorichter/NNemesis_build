generatePhyloDDD_minimal <- function(n_trees,
                             lambda_interval,
                             K_interval){
  
  simulations <- list()
  
  name.param <- c("lambda", "mu","K") 
  true.param <- vector(mode='list', length=3)
  names(true.param) <- name.param
  j=0
  while(length(trees) < n_trees){
    j=j+1
    
    lambda0_sample <- runif(1, min = lambda_interval[1], max = lambda_interval[2])
    mu_sample <- runif(1, min = 0, max = lambda0_sample)
    k_sample <- runif(1, min = K_interval[1], max = K_interval[2])
    
    beta_sample = (mu_sample-lambda0_sample)/k_sample
    
    vec.param <- c(lambda0_sample,mu_sample,k_sample)
    sim.param <- c(mu_sample,lambda0_sample,beta_sample,0)
    

    outputs <- emphasis:::sim_tree_pd_cpp(pars = sim.param,
                                            max_t = 1,
                                            max_lin = 1e+6,
                                            max_tries = 10)

    simulations <- append(simulations, outputs)                    # save tree                   #
    
    for (i in 1:3){
      true.param[[i]] <- c(true.param[[i]], vec.param[i]) # save param.
    }
    
    svMisc:::progress(length(trees), n_trees, progress.bar = TRUE, # print
                      init = (length(trees)==1))                   # progression
    
  }
  
  out <- list("trees"    = simulations, "param"    = true.param)
  
  return(out)
}
