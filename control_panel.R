remove(list = ls())
options(mc.cores = parallel::detectCores())

# Set parameters

matrix_types <- c("posterior")
iterations <- 10000
herd_length <- 200
eta <- 0
alpha <- 0.8
beta <- 0.8
pi <- 0.7
states <- c("a", "b") 
pis <- c(0.49, 0.5, 0.51)

# Warn if alpha and beta don't satisfy condition that: 
# P(signal = a | state = A) >= P(signal = b | state = A) and 
# P(signal = b | state = B) >= P(signal = a | state = B)

if (alpha < 0.5) {
  print(paste0("Caution: an alpha value of ", alpha, " makes a b signal more common than an a signal when state = A"))
}

if (beta < 0.5) {
  print(paste0("Caution: a beta value of ", beta, " makes an a signal more common than a b signal when state = B"))
}

# if FALSE, agents observe all actions before them 
# if an integer, each agent only observes the last n actions

memory_length <- 4

# Should order be considered?

order <- FALSE

# Parameter ranges

alpha_range <- c(seq(from = 0.5, to = 0.9, by = 0.05), seq(from = 0.91, to = 0.99, by = 0.01))

beta_range <- c(seq(from = 0.5, to = 0.9, by = 0.05), seq(from = 0.91, to = 0.99, by = 0.01))

# Load functions 

source("functions.R", local = TRUE)

# for (pi in pis){

# Run simulations 

source("simulations.R", local = TRUE)

# Output results 

source("output_simulations.R", local = TRUE)
# }