# Load required packages
library(ape)
library(diversitree)

### PARAMETERS ###


bd <- TRUE
bisse <- TRUE
musse <- FALSE
geosse <- TRUE
bisseness <- FALSE
classe <- TRUE
quasse <- FALSE

step <- 0.1
lambda_range <- c(0.1, 0.73)
mu_range <- c(0, 0.75)
q_range <- c(0.1, 0.9)
dispersal_range <- c(0.1, 0.9)
r_range <- c(0.5, 1.0)
p_range <- c(0.1, 0.9) # bisseness

num_trees <- 10**4
name <- "1k"
T_max <- Inf
max_taxa <- 1000

set.seed(1)


### BD ###

tree_bd <- function(lambda, mu, num_trees) {
  
  trees <- list()
  lambdas <- list()
  mus <- list()
  n_sp <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/bd.params", "a")
  sizes_conn <- file("trees/bd.sizes", "a")
  for (i in 1:num_trees) {
    l <- runif(1, lambda)
    m <- runif(1, mu)
    cat("BD [", j, "/", num_trees, "], λ = ", l, "μ = ", m)
    if(m < l) {
      max_taxa_i <- runif(1, 10, max_taxa)
      phy <- tree.bd(c(l, m), max.taxa=max_taxa_i)
      if (length(phy$tip.label) > 0) {
        trees[[length(trees) + 1]] <- phy
        # Save the parameters of the tree
        params[[length(params) + 1]] <- c(l, m)
        
        ape::write.tree(phy, file = "trees/bd.nwk", append = TRUE)
        writeLines(paste(length(phy$tip.label)), sizes_conn)
        
        par <- params[[length(params)]]
        param_lines <- c(
          paste("lambda ", par[1]),
          paste("mu ", par[2])
        )
        writeLines(param_lines, param_conn)
        
        # print number of trees in the two files
        cat("Length of nwk", length(trees))
        cat("Length of params", length(params))
        
      }
      cat(" Number of extant species: ", length(phy$tip.label))
      
      lambdas[[length(lambdas) + 1]] <- l
      mus[[length(mus) + 1]] <- m
      n_sp[[length(n_sp) + 1]] <- length(phy$tip.label)
      
    }
    j <- j+1
    cat("\n")
    
    
  }
  # plot the couples lambda, mu, and mark the point red if they produce a tree with 0 species, with full dot
  plot(lambdas, mus, col = ifelse(n_sp == 0, "red", "black"), pch = ifelse(n_sp == 0, 1, 19), xlab = "lambda", ylab = "mu")
  # count the number of trees with 0 species with respect to the total number of trees
  cat("Number of trees with 0 species: ", sum(n_sp == 0), "/", num_trees, " ", 100*sum(n_sp == 0)/num_trees, "%\n")
  # The same using trees$tip.label
  cat("Number of trees with 0 species: ", sum(sapply(trees, function(x) length(x$tip.label)) == 0), "/", num_trees, " ", 100*sum(sapply(trees, function(x) length(x$tip.label)) == 0)/num_trees, "%\n")
  hist(sapply(trees, function(x) length(x$tip.label)), main = "Number of extant species in BD trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  
  close(param_conn)
  close(sizes_conn)
  
  cat("Number of trees: ", length(trees), "\n")
  cat("Number of parameters: ", length(params), "\n")
  return(list(trees=trees, params=params))
}

### BiSSE ###


tree_bisse <- function(lambda_range, mu_range, q_range, num_trees) {
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/bisse.params", "a")
  sizes_conn <- file("trees/bisse.sizes", "a")
  for (i in 1:num_trees) {
    lambdas <- runif(2,lambda_range[1], lambda_range[2])
    mus <- runif(2,mu_range[1], mu_range[2])
    qs <- runif(2,q_range[1], q_range[2])
    cat("BiSSE [", j, "/", num_trees, "] λ [", lambdas, "] μ [", mus, "] q [", qs, "], ")
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.bisse(c(lambdas, mus, qs), max.taxa=max_taxa_i, x0=0)
    cat("Number of extant species: ", length(phy$tip.label), "\n")
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # Save the parameters of the tree
      params[[length(params) + 1]] <- c(lambdas, mus, qs)
      
      ape::write.tree(phy, file = "trees/bisse.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("lambda1 ", par[1]),
        paste("lambda2 ", par[2]),
        paste("mu1 ", par[3]),
        paste("mu2 ", par[4]),
        paste("q12 ", par[5]),
        paste("q21 ", par[6])
      )
      writeLines(param_lines, param_conn)
    }
    j <- j+1
    
  }
  close(param_conn)
  close(sizes_conn)
  return(list(trees=trees, params=params))
}

### MUSSE ###

tree_musse <- function(lambda_range, mu_range, q_range, num_states, num_trees) {
  
  
  #pars <- c(.1, .15, .2, # lambda 1, 2, 3
  #        .03, .045, .06, # mu 1, 2, 3
  #        .05, 0, # q12, q13
  #        .05, .05, # q21, q23
  #        0, .05) # q31, q32
  
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/musse_500k.params", "a")
  sizes_conn <- file("trees/musse_500k.sizes", "a")
  for(i in 1:num_trees) {
    lambdas <- runif(num_states, lambda_range[1], lambda_range[2])
    # sample mu from a uniform distribution between the mu_range[1] and the corresponding lambda
    mus <- rep(0, num_states)
    for (k in 1:num_states) {
      mus[k] <- runif(1, mu_range[1], lambdas[k])
    }
    qs <- runif(num_states * (num_states - 1), q_range[1], q_range[2])
    pars <- c(lambdas, mus, qs)
    cat("MuSSE [", j, "/", num_trees, "]\n") # λ [", lambdas, "] μ [", paste(mus), "] q [", qs, "], ")
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.musse(pars, max.taxa=max_taxa_i, x0=1)
    
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # save the parameters of the tree
      params[[length(params) + 1]] <- c(lambdas, mus, qs)
      
      ape::write.tree(phy, file = "trees/musse_500k.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("num_states", num_states),
        paste("lambda1 ", par[1]),
        paste("lambda2 ", par[2]),
        paste("lambda3 ", par[3]),
        paste("mu1 ", par[4]),
        paste("mu2 ", par[5]),
        paste("mu3 ", par[6]),
        paste("q12 ", par[7]),
        paste("q13 ", par[8]),
        paste("q21 ", par[9]),
        paste("q23 ", par[10]),
        paste("q31 ", par[11]),
        paste("q32 ", par[12])
      )
      writeLines(param_lines, param_conn)
      
    }
    
    cat("Number of extant species: ", length(phy$tip.label), "\n")
    j <- j+1
    
    
  }
  
  close(param_conn)
  close(sizes_conn)
  
  return(list(trees=trees, params=params))
  
}


### GeoSSE ###

tree_geosse <- function(lambda_range, mu_range, dispersal_range, num_trees) {
  
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/geosse.params", "a")
  sizes_conn <- file("trees/geosse.sizes", "a")
  for(i in 1:num_trees) {
    cat("GeoSSE [", j, "/", num_trees, "]")
    sp_a <- runif(1,lambda_range)
    sp_b <- runif(1,lambda_range)
    s_ab <- runif(1,lambda_range)
    mu_a <- runif(1,mu_range)
    mu_b <- runif(1,mu_range)
    dispersal_a_to_b <- runif(1,dispersal_range)
    dispersal_b_to_a <- runif(1,dispersal_range)
    pars <- c(sp_a, sp_b, s_ab, mu_a, mu_b, dispersal_a_to_b, dispersal_b_to_a)
    cat(" sp_a = ", sp_a, " sp_b = ", sp_b, " s_ab = ", s_ab, " mu_a = ", mu_a, " mu_b = ", mu_b, " dispersal_a_to_b = ", dispersal_a_to_b, " dispersal_b_to_a = ", dispersal_b_to_a)
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.geosse(pars, max.taxa=max_taxa_i, x0=0)
    cat(" Number of extant species: ", length(phy$tip.label), "\n")
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # Save the parameters of the tree
      params[[length(params) + 1]] <- c(sp_a, sp_b, s_ab, mu_a, mu_b, dispersal_a_to_b, dispersal_b_to_a)
      
      ape::write.tree(phy, file = "trees/geosse.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("sp_a ", par[1]),
        paste("sp_b ", par[2]),
        paste("s_ab ", par[3]),
        paste("mu_a ", par[4]),
        paste("mu_b ", par[5]),
        paste("dispersal_a_to_b ", par[6]),
        paste("dispersal_b_to_a ", par[7])
      )
      writeLines(param_lines, param_conn)
    }
    j <- j+1
  }
  close(param_conn)
  close(sizes_conn)
  return(list(trees=trees, params=params))
  
}


### BiSSEness ###

#Parameters:
#• λ0, λ1: Base speciation rates irrespective of character states.
#• μ0, μ1: Baseline extinction rates regardless of character states.
#• p0c, p0a, p1c, p1a: Conditional probabilities of shifting character states dur-
#ing speciation or lineage development


tree_bisseness <- function(lambda_range, mu_range, q_range, p_range, num_trees){
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/bisseness.params", "a")
  sizes_conn <- file("trees/bisseness.sizes", "a")
  for(i in 1:num_trees) {
    cat("BiSSEness [", j, "/", num_trees, "]\n")
    lambdas <- runif(2, min = lambda_range[1], max = lambda_range[2])
    mus <- runif(2,mu_range[1], mu_range[2])
    qs <- runif(2,q_range[1], q_range[2])
    ps <- runif(4,p_range[1], p_range[2])
    pars <- c(lambdas, mus, qs, ps)
    cat("λ [", lambdas, "] μ [", mus, "] q [", qs, "] p [", ps, "], ")
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.bisseness(pars, max.taxa=max_taxa_i, x0=0)
    cat("Number of extant species: ", length(phy$tip.label), "\n")
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # Save the parameters of the tree
      params[[length(params) + 1]] <- c(lambdas, mus, qs, ps)
      
      ape::write.tree(phy, file = "trees/bisseness.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("lambda1 ", par[1]),
        paste("lambda2 ", par[2]),
        paste("mu1 ", par[3]),
        paste("mu2 ", par[4]),
        paste("q1 ", par[5]),
        paste("q2 ", par[6]),
        paste("p1c ", par[7]),
        paste("p1a ", par[8]),
        paste("p2c ", par[9]),
        paste("p2a ", par[10])
      )
      writeLines(param_lines, param_conn)
    }
    j <- j+1
  }
  close(param_conn)
  close(sizes_conn)
  return(list(trees=trees, params=params))
}

### CLASSE ###
#diversitree:::default.argnames.classe(2)
#[1] "lambda111" "lambda112" "lambda122" "lambda211" "lambda212" "lambda222" "mu1"       "mu2"       "q12"       "q21"
tree_classe_2 <- function(lambda_range, mu_range, q_range, num_trees) {
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/classe_2.params", "a")
  sizes_conn <- file("trees/classe_2.sizes", "a")
  for(i in 1:num_trees) {
    lambdas <- runif(6,lambda_range[1], lambda_range[2])
    mus <- runif(2,mu_range[1], mu_range[2])
    qs <- runif(2,q_range[1], q_range[2])
    pars <- c(lambdas, mus, qs)
    cat("CLASSE [", j, "/", num_trees, "] λ [", lambdas, "] μ [", mus, "] q [", qs, "], ")
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.classe(pars, max.taxa=max_taxa_i, x0=1)
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # Save the parameters of the tree
      params[[length(params) + 1]] <- c(lambdas, mus, qs)
      
      ape::write.tree(phy, file = "trees/classe_2.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("lambda111 ", par[1]),
        paste("lambda112 ", par[2]),
        paste("lambda122 ", par[3]),
        paste("lambda211 ", par[4]),
        paste("lambda212 ", par[5]),
        paste("lambda222 ", par[6]),
        paste("mu1 ", par[7]),
        paste("mu2 ", par[8]),
        paste("q12 ", par[9]),
        paste("q21 ", par[10])
      )
      writeLines(param_lines, param_conn)
    }
    cat("Number of extant species: ", length(phy$tip.label), "\n")
    j <- j+1
  }
  close(param_conn)
  close(sizes_conn)
  return(list(trees=trees, params=params))
}


### QuaSSE ###
# LAMBDAS AND MUS ARE ARBITRARY FUNCTIONS
#lambda <- function(x) sigmoid.x(x, 0.1, 0.2, 0, 2.5)
#mu <- function(x) constant.x(x, 0.03)
#phy <- tree.quasse(c(lambda, mu), max
tree_quasse <- function(lambda_range, mu_range, T_max, num_trees) {
  trees <- list()
  j <- 0
  params <- list()
  param_conn <- file("trees/quasse.params", "a")
  sizes_conn <- file("trees/quasse.sizes", "a")
  for(i in 1:num_trees) {
    # randomly select lambda and mu functions from a list of possible functions: sigmoid, constant, linear, etc.
    functions <- c( "sigmoid", 
                    "constant",
                    "stepf",
                    "noroptimal")
    lambda <- sample(functions, 1)
    mu <- sample(functions, 1)
    cat("QuaSSE [", j, "/", num_trees, "]\n")
    if (lambda == "sigmoid") {
      cat(" λ <- sigmoid.x(x, ")
      # y0: y value at very small 'x' (limit as 'x' tends to negative infinity)
      # y1: y value at very large 'x' (limit as 'x' tends to infinity). For 'noroptimal.x', this is the y value at 'xmid'.
      # xmid: Midpoint (inflection point) of sigmoid or step function
      # r: Rate at which exponential decay occurs or sigmoid changes - higher values are steeper
      y <- runif(2, min = lambda_range[1], max = lambda_range[2])
      # I take xmid to be between 1/4 and 3/4 of the maximum value of T_max
      xmid <- runif(1, T_max/4, 3* T_max/4)
      r <- runif(1,0,50)
      cat("y0 = ", y[1], ", y1 = ", y[2], ", xmid = ", xmid, ", r = ", r, ")")
      lambda <- function(x) sigmoid.x(x, y[1], y[2], xmid, r)
    } else if (lambda == "constant") {
      cat(" λ <- constant(x, ")
      c <- runif(1,lambda_range, 1)
      cat("c = ", c, ")")
      lambda <- function(x) constant.x(x, c)
    } else if (lambda == "noroptimal") {
      cat(" λ <- noroptimal(x, ")
      y <- runif(2, min = lambda_range[1], max = lambda_range[2])
      xmid <- runif(1, T_max/4, 3* T_max/4)
      # s2 is the standard deviation of the normal distribution
      s2 <- runif(0.1, 0.5)
      cat("y0 = ", min(y), "y1 = ", max(y), ", xmid = ", xmid, ")")
      lambda <- function(x) noroptimal.x(x, min(y), max(y), xmid, s2)
    } else if (lambda == "stepf"){
      cat(" λ <- stepf(x, ")
      y <- runif(2, min = lambda_range[1], max = lambda_range[2])
      xmid <- runif(1, T_max/4, 3* T_max/4)
      cat("y0 = ", y[1], "y1 = ", y[2], ", xmid = ", xmid, ")")
      lambda <- function(x) stepf.x(x, y[1], y[2], xmid)
    }
    
    cat("\n")
    if (mu == "sigmoid") {
      cat(" μ <- sigmoid.x(x, ")
      y <- runif(2,mu_range[1], mu_range[2])
      xmid <- runif(1,  T_max/4, 3* T_max/4)
      r <- runif(1,0.5,1.0)
      cat("y0 = ", y[1], ", y1 = ", y[2], ", xmid = ", xmid, ", r = ", r, ")")
      mu <- function(x) sigmoid.x(x, y[1], y[2], xmid, r)
    } else if (mu == "constant") {
      cat(" μ <- constant(x, ")
      c <- runif(1,mu_range)
      cat("c = ", c, ")")
      mu <- function(x) constant.x(x, c)
    } else if (mu == "noroptimal"){
      cat(" μ <- noroptimal(x, ")
      y <- runif(2,mu_range[1], mu_range[2])
      xmid <- runif(1,  T_max/4, 3* T_max/4)
      s2 <- runif(0.1, 0.5)
      cat("y0 = ", min(y), "y1 = ", max(y), ", xmid = ", xmid, ")")
      mu <- function(x) noroptimal.x(x, min(y), max(y), xmid, s2)
    } else if (mu == "stepf"){
      cat(" μ <- stepf(x, ")
      y <- runif(2,mu_range[1], mu_range[2])
      xmid <- runif(1,  T_max/4, 3* T_max/4)
      cat("y0 = ", y[1], "y1 = ", y[2], ", xmid = ", xmid, ")")
      mu <- function(x) stepf.x(x, y[1], y[2], xmid)
    }
    
    cat("\n")
    
    # char: Character state evolution function
    # make.brownian.with.drift(drift, diffusion)
    # drift: Rate of drift
    # diffusion: Rate of diffusion (positive)
    # char <- make.brownian.with.drift(0, 0.025)
    drift <- runif(1, 0, 0.1)
    diffusion <- runif(1, 0.01, 0.1)
    #cat(" char = brownian with drift(", drift, ",", diffusion, ")")
    #char <- make.brownian.with.drift(drift, diffusion)
    char <- make.brownian.with.drift(0, 0.025)
    cat(" char <- make.brownian.with.drift(", 0, ",", 0.025, ")")
    cat("\n")
    
    pars <- c(lambda, mu, char)
    max_taxa_i <- runif(1, 10, max_taxa)
    phy <- tree.quasse(pars, max.taxa=max_taxa_i, x0=0)
    if (length(phy$tip.label) > 0) {
      trees[[length(trees) + 1]] <- phy
      # Save the parameters of the tree
      params[[length(params) + 1]] <- c(lambda, mu, char)
      
      ape::write.tree(phy, file = "trees/quasse.nwk", append = TRUE)
      writeLines(paste(length(phy$tip.label)), sizes_conn)
      
      par <- params[[length(params)]]
      param_lines <- c(
        paste("lambda ", lambda),
        paste("mu ", mu),
        paste("char ", char)
      )
      writeLines(param_lines, param_conn)
    }
    j <- j+1
    cat(" Number of extant species: ", length(phy$tip.label), "\n")
    cat(" Species: ", phy$tip.label, "\n")
  }
  close(param_conn)
  close(sizes_conn)
  return(list(trees=trees, params=params))
}

## define a get_ranges function that loads the complete trees from "data/completeTrees.Rdata"
# and for each model do inference to get the data. So it loops on each model, and it runs the 
# diversitree function make.bd or make.bisse or make.quasse etc. to infer the parameters. It 
# also checks the likelihood and based on that it retrieves the ranges of the parameters (lambda, mu,...)

get_ranges <- function() {
  load("data/completeTrees.Rdata")
  pars <- list()
  treshold <- 0
  cat("Running BD model\n")
  pars <- list()
  for (tree in completeTrees) {
    t <- tree[[1]]
    # if the tree is not ultrametric, we need to make it ultrametric
    if (!is.ultrametric(t)) {
      t <- chronos(t)
    }
    tryCatch(
      {
        lik <- make.bd(t)
        p <- starting.point.bd(t)
        fit <- find.mle(lik, p)
        fit$lnLik
        if (fit$lnLik > threshold) {
          pars[[length(pars) + 1]] <- fit$par
        }
      },
      error = function(e) {
        cat("Error in tree\n")
      }
    )
    
  }
  for (tree in completeTrees) {
    t <- tree[[1]]
    # if the tree is not ultrametric, we need to make it ultrametric
    if (!is.ultrametric(t)) {
      t <- chronos(t)
    }
    tryCatch(
      {
        lik <- make.bd(t)
        p <- starting.point.bd(t)
        fit <- find.mle(lik, p)
        fit$lnLik
        if (fit$lnLik > threshold) {
          pars[[length(pars) + 1]] <- fit$par
        }
      },
      error = function(e) {
        cat("Error in tree\n")
      }
    )
    
  }
  lambda_bd_range <- c(min(sapply(pars, function(x) x[1])), max(sapply(pars, function(x) x[1])))
  mu_bd_range <- c(min(sapply(pars, function(x) x[2])), max(sapply(pars, function(x) x[2])))
  lambda_bd_range
  mu_bd_range
}





### BD ###

if (bd) {
  start_time <- Sys.time()
  tree_bd_simulations <- tree_bd(lambda_range, mu_range, num_trees)
  end_time <- Sys.time()
  bd_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for TreeBD: ", bd_time, "\n")
  num_extant_bd <- mean(sapply(tree_bd_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in TreeBD trees: ", num_extant_bd, "\n")
  cat("Number of extant species in TreeBD trees: ", num_extant_bd, "\n")
  hist(sapply(tree_bd_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in TreeBD trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_bd.png")  
  
}

### BiSSE ###

if (bisse){
  start_time <- Sys.time()
  tree_bisse_simulations <- tree_bisse(lambda_range, mu_range, q_range, num_trees)
  end_time <- Sys.time()
  
  bisse_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for BiSSE: ", bisse_time, "\n")
  num_extant_bisse <- mean(sapply(tree_bisse_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in BiSSE trees: ", num_extant_bisse, "\n")
  cat("Number of extant species in BiSSE trees: ", num_extant_bisse, "\n")
  hist(sapply(tree_bisse_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in BiSSE trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_bisse.png")
}


### MUSSE ###

if(musse){
  num_states <- 3
  start_time <- Sys.time()
  tree_musse_simulations <- tree_musse(lambda_range, mu_range, q_range, num_states, num_trees)
  end_time <- Sys.time()
  
  cat("Number of trees: ", length(tree_musse_simulations$trees), "\n")
  cat("Number of parameters: ", length(tree_musse_simulations$params), "\n")
  
  
  # Plot the parameters lambda1, mu1, lambda2, mu2, with red dots for trees with 0 species all in one plot
  par(mfrow=c(2,2))
  plot(sapply(tree_musse_simulations$params, function(x) x[1]), sapply(tree_musse_simulations$params, function(x) x[4]), col = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, "red", "black"), pch = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, 1, 19), xlab = "lambda1", ylab = "mu1")
  plot(sapply(tree_musse_simulations$params, function(x) x[2]), sapply(tree_musse_simulations$params, function(x) x[5]), col = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, "red", "black"), pch = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, 1, 19), xlab = "lambda2", ylab = "mu2")
  plot(sapply(tree_musse_simulations$params, function(x) x[3]), sapply(tree_musse_simulations$params, function(x) x[6]), col = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, "red", "black"), pch = ifelse(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0, 1, 19), xlab = "lambda3", ylab = "mu3")
  # count the number of trees with 0 species with respect to the total number of trees
  cat("Number of trees with 0 species: ", sum(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0), "/", num_trees, " ", 100*sum(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)) == 0)/num_trees, "%\n")
  length(tree_musse_simulations$trees)
  num_trees
  par(mfrow=c(1,1))
  
  musse_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for MuSSE: ", musse_time, "\n")
  num_extant_musse <- mean(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in MuSSE trees: ", num_extant_musse, "\n")
  hist(sapply(tree_musse_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in MuSSE trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  #png("hist/tree_musse.png")
}

# if only musse and other false stop the script
if(musse & !bd & !bisse & !geosse & !bisseness & !classe & !quasse){
  stop("Only MuSSE is selected, stopping the script")
  return()
}


### GeoSSE ###
if(geosse){
  start_time <- Sys.time()
  tree_geosse_simulations <- tree_geosse(lambda_range, mu_range, dispersal_range, num_trees)
  end_time <- Sys.time()
  
  geosse_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for GeoSSE: ", geosse_time, "\n")
  num_extant_geosse <- mean(sapply(tree_geosse_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in GeoSSE trees: ", num_extant_geosse, "\n")
  hist(sapply(tree_geosse_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in GeoSSE trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_geosse.png")
}
### BiSSEness ###
if(bisseness){
  start_time <- Sys.time()
  tree_bisseness_simulations <- tree_bisseness(lambda_range, mu_range, q_range, p_range, num_trees)
  end_time <- Sys.time()
  
  bisseness_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for BiSSEness: ", bisseness_time, "\n")
  num_extant_bisseness <- mean(sapply(tree_bisseness_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in BiSSEness trees: ", num_extant_bisseness, "\n")
  hist(sapply(tree_bisseness_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in BiSSEness trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_bisseness.png")
}

### ClaSSE ###
if(classe){
  start_time <- Sys.time()
  tree_classe_simulations <- tree_classe_2(lambda_range, mu_range, q_range, num_trees)
  end_time <- Sys.time()
  
  classe_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for ClaSSE: ", quasse_time, "\n")
  num_extant_classe <- mean(sapply(tree_classe_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in ClaSSE trees: ", num_extant_classe, "\n")
  hist(sapply(tree_classe_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in ClaSSE trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_classe.png")
}

### QuaSSE ###
if(quasse){
  start_time <- Sys.time()
  tree_quasse_simulations <- tree_quasse(lambda_range, mu_range, T_max, num_trees)
  end_time <- Sys.time()
  
  # save time, specifying if seconds, minutes, hours, etc.
  quasse_time <- as.numeric(end_time - start_time, units = "mins")
  cat("Time for QuaSSE: ", quasse_time, "\n")
  num_extant_quasse <- mean(sapply(tree_quasse_simulations$trees, function(x) length(x$tip.label)))
  cat("Average number of extant species in QuaSSE trees: ", num_extant_quasse, "\n")
  hist(sapply(tree_quasse_simulations$trees, function(x) length(x$tip.label)), main = "Number of extant species in QuaSSE trees", xlab = "Number of extant species", ylab = "Frequency", plot=TRUE)
  png("hist/tree_quasse.png")
}

if (bd & bisse & musse & geosse & bisseness & classe & quasse) {
  results <- rbind(c("TreeBD", num_extant_bd, bd_time),
                   c("BiSSE", num_extant_bisse, bisse_time),
                   c("MuSSE", num_extant_musse, musse_time),
                   c("GeoSSE", num_extant_geosse, geosse_time),
                   c("BiSSEness", num_extant_bisseness, bisseness_time),
                   c("ClaSSE", num_extant_classe, classe_time),
                   c("QuaSSE", num_extant_quasse, quasse_time))
  
  colnames(results) <- c("Model", "Average number of extant species", paste("Time to simulate ", num_trees, " trees (mins)"))
  knitr::kable(as.data.frame(results))
}