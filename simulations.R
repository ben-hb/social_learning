# Definitions

# There are two states, A and B 
# Exactly one is true/right 

# Agents receive a discrete signal
# a correlates with state A
# b correlates with state B 
# o is uncorrelated with state
# eta is the frequency of the null signal 
# P(signal = 0 | state = A) = P(signal = o | state = B) = eta
# alpha and beta are the respective signal frequences of a and b
# alpha = P(signal = a | state = A) = alpha * (1 - eta)
# beta = P(signal = b | state = B) = beta * (1 - eta)

# pi is the prior that state = A

# herd_length is the maximum length of the single file line 

# iterations is the number of simulated herds that an agent runs to determine
# the relative frequency of the action sequence before them in either state

# Agents have two choices of actions: a and b 

# They take action a iff P(state = A | action sequence, private signal) > 0.5

# They take action b iff P(state = A | action sequence, private signal) < 0.5

# They randomize with symmetric probability between actions a and b iff: 
# P(state = A | action sequence, private signal) = 0.5

temp_NAs <- rep(NA, times = length(alpha_range) * length(beta_range))

correct_herd_rate <- matrix(data = NA, nrow = length(beta_range), ncol = length(alpha_range))

correct_herd_rate_a <- data.frame(correct_herd_rate)
correct_herd_rate_b <- data.frame(correct_herd_rate)

row.names(correct_herd_rate_a) <- beta_range
row.names(correct_herd_rate_b) <- beta_range

colnames(correct_herd_rate_a) <- alpha_range
colnames(correct_herd_rate_b) <- alpha_range

# for (beta in beta_range) {
# 
# for (alpha in alpha_range) {

# Generates vectors of names of matrices for loops

for (matrix_type in matrix_types){
  assign(paste(matrix_type, "matrices", sep = "_"), paste(matrix_type, "matrix", states, sep = "_"))
}

# Initialize empty matrices

for (state in states){
  for (matrix_type in matrix_types){
    assign(paste(matrix_type, "matrix", state, sep = "_"), init_matrix(iterations, herd_length + 1))
  }
}

signal_matrix_a <- init_matrix(iterations, herd_length + 1)
signal_matrix_b <- init_matrix(iterations, herd_length + 1)

decision_matrix_a <- init_matrix(iterations, 1)
decision_matrix_b <- init_matrix(iterations, 1)

# Assign signals 

for (state in states){
  assign(paste("signal_matrix", state, sep = "_"), gen_signals(signal_matrix_a, eta, alpha, beta, state))
}

# Initialize list of decision summary data frames

decision_summary_a_dfs <- list()
decision_summary_b_dfs <- list()

for (generation in 1:herd_length) {
  
  decision_summary_a_dfs[[paste("decision_summary_a_df", generation, sep = "_")]] <- data.frame(NULL)
  decision_summary_b_dfs[[paste("decision_summary_b_df", generation, sep = "_")]] <- data.frame(NULL)
  
}

# Initialize a list of action posterior matrices

action_posterior_matrices <- list()

for (generation in 1:herd_length) {
  
  action_posterior_matrices[[paste("action_posterior_matrix", generation, sep = "_")]] <- data.frame(NULL)
  
}

# Initialize a vector indicating which iterations have herded
# TRUE indicates that the generation has herded, FALSE indicates that it has not

herd_tracker_a_vectors <- list()
herd_tracker_b_vectors <- list()

for (generation in 1:herd_length) {
  herd_tracker_a_vectors[[paste("herd_tracker_a_vector", generation, sep = "_")]] <- c(rep(NA, times = iterations))
  herd_tracker_b_vectors[[paste("herd_tracker_b_vector", generation, sep = "_")]] <- c(rep(NA, times = iterations))
}

# For the special case in which there are no prior actions to consider
# Generating posterior for generation 1 solely after observing their own signal

# Calculate beliefs conditional on receiving signal a or b 

posterior_with_signal_a_a <- infer_if_a(pi)
posterior_with_signal_b_a <- infer_if_b(pi)

posterior_with_signal_a_b <- infer_if_a(pi)
posterior_with_signal_b_b <- infer_if_b(pi)

# Warn if posterior out of bounds 

if (posterior_with_signal_a_a > 1 || posterior_with_signal_a_b > 1 || posterior_with_signal_b_a > 1|| posterior_with_signal_b_b > 1) {
  print("Terminal error: posterior belief greater than 1")
}

if (posterior_with_signal_a_a < 0 || posterior_with_signal_a_b < 0 || posterior_with_signal_b_a < 0 || posterior_with_signal_b_b < 0) {
  print("Terminal error: posterior belief less than 0")
}

# Generate posteriors from action posterior matrix with signal

posterior_matrix_a[, 1] <- infer_on_signal(signal_matrix_a, pi, posterior_with_signal_a_a,
                                           posterior_with_signal_b_a, 1)

posterior_matrix_b[, 1] <- infer_on_signal(signal_matrix_b, pi, posterior_with_signal_a_b,
                                           posterior_with_signal_b_b, 1)

# Check which iterations have herded

herd_tracker_a_vectors[[1]] <- track_herd(posterior_with_signal_a_a, posterior_with_signal_b_a, 1)
herd_tracker_b_vectors[[1]] <- track_herd(posterior_with_signal_a_b, posterior_with_signal_b_b, 1)

start_time <- Sys.time()

for (generation in 1:herd_length) {
  
  # Status report
  print(paste0("Simulating generation #", generation))

  # Decide
  
  temp_decision_a <- decide(posterior_matrix_a[, generation], signal_matrix_a[, generation])
  temp_decision_b <- decide(posterior_matrix_b[, generation], signal_matrix_b[, generation])
  
  if (generation == 1) {
    decision_matrix_a <- as.matrix(temp_decision_a)
    decision_matrix_b <- as.matrix(temp_decision_b)
  }
  
  else {
    decision_matrix_a <- cbind(decision_matrix_a, temp_decision_a)
    decision_matrix_b <- cbind(decision_matrix_b, temp_decision_b)
  }
  
  # Starting with generation 2, concatenate decisions into permutation string
  
  # if (generation > 1) {
  
  cat_decision_matrix_a <- matrix(apply(format(decision_matrix_a), 1, paste, collapse = ""), iterations, 1)
  cat_decision_matrix_b <- matrix(apply(format(decision_matrix_b), 1, paste, collapse = ""), iterations, 1)
  
  if (order == FALSE) {
    cat_decision_matrix_a <- vapply(strsplit(cat_decision_matrix_a, NULL), function(x) paste(sort(x), collapse = ''), '')
    cat_decision_matrix_b <- vapply(strsplit(cat_decision_matrix_b, NULL), function(x) paste(sort(x), collapse = ''), '')
    
    cat_decision_matrix_a <- matrix(cat_decision_matrix_a)
    cat_decision_matrix_b <- matrix(cat_decision_matrix_b)
  }
  
  if (memory_length != FALSE) {
    if(memory_length < generation) {
      cat_decision_matrix_a <- substr(cat_decision_matrix_a, generation - memory_length + 1, generation)
      cat_decision_matrix_b <- substr(cat_decision_matrix_b, generation - memory_length + 1, generation) 
    }
  }
  
  # Generate decision summary data frames
  
  decision_summary_a_dfs[[generation]] <- as.data.frame(table(cat_decision_matrix_a))
  
  decision_summary_a_dfs[[generation]]$cat_decision_matrix_a <- as.character(decision_summary_a_dfs[[generation]]$cat_decision_matrix_a)
  
  decision_summary_b_dfs[[generation]] <- as.data.frame(table(cat_decision_matrix_b))
  
  decision_summary_b_dfs[[generation]]$cat_decision_matrix_b <- as.character(decision_summary_b_dfs[[generation]]$cat_decision_matrix_b)
  
  # Infer posteriors from decision summary tables without signal
  
  action_posterior_matrices[[generation]] <- infer_on_actions(decision_summary_a_dfs[[generation]], decision_summary_b_dfs[[generation]],
                                                              pi, generation)
  
  # Calculate belief that state = A before observing new signal 
  
  prior_a <- infer_prior(decision_matrix_a, action_posterior_matrices[[generation]], generation)
  prior_b <- infer_prior(decision_matrix_b, action_posterior_matrices[[generation]], generation)
  
  # Calculate beliefs conditional on receiving signal a or b 
  
  posterior_with_signal_a_a <- infer_if_a(prior_a)
  posterior_with_signal_b_a <- infer_if_b(prior_a)
  
  posterior_with_signal_a_b <- infer_if_a(prior_b)
  posterior_with_signal_b_b <- infer_if_b(prior_b)
  
  # Generate posteriors from action posterior matrix with signal
  
  posterior_matrix_a[, generation + 1] <- infer_on_signal(signal_matrix_a, prior_a, posterior_with_signal_a_a,
                                                          posterior_with_signal_b_a, generation + 1)
  
  posterior_matrix_b[, generation + 1] <- infer_on_signal(signal_matrix_b, prior_b, posterior_with_signal_a_b,
                                                          posterior_with_signal_b_b, generation + 1)
  
  # Check which iterations have herded
  
  herd_tracker_a_vectors[[generation + 1]] <- track_herd(posterior_with_signal_a_a, posterior_with_signal_b_a, generation)
  herd_tracker_b_vectors[[generation + 1]] <- track_herd(posterior_with_signal_a_b, posterior_with_signal_b_b, generation)
  
  # Check if 99% of iterations have herded
  # If so, move on to next generation 
  
  if (sum(herd_tracker_a_vectors[[generation + 1]]) + sum(herd_tracker_b_vectors[[generation + 1]]) >= 2 * 99 * iterations / 100) {
    print(paste("All iterations have herded after ", generation, " generations. Ending simulation."))
    correct_herd_rate_a[as.character(beta), as.character(alpha)] <- sum(decision_matrix_a[, generation] == "A") / iterations
    correct_herd_rate_b[as.character(beta), as.character(alpha)] <- sum(decision_matrix_b[, generation] == "B") / iterations
    break
  }
}

# }
# 
# }

end_time <- Sys.time()
print(paste0("Run time for ", iterations, " iterations and a herd length of ", herd_length, ": ", round(end_time - start_time, 3)))