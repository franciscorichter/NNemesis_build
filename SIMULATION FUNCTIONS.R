## SIMULATION FUNCTIONS 

tau <- 10
b <- 1e6
d <- b-0.5
nu <- 0.6
tree <- RPANDA::sim_sgd(tau,b,d,nu)
plot(tree)
