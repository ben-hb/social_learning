# Generate Starting Matrices

init_matrix <- function(iterations, herd_length) {
  temp_matrix <- matrix(0, iterations, herd_length)
  return(temp_matrix)
}

# Loop over state 

gen_signals <- function(signal_matrix, eta, alpha, beta, state) {
  signals <- sample(c("a", "b", "o"), size = length(signal_matrix), replace = TRUE, 
# P(signal = b | state = A) = 1 - P(signal = 0 | state = A) - P(signal = a | state = A)
                    prob = c(ifelse(state == "a", alpha * (1 - eta), 1 - eta - beta * (1 - eta)),
# P(signal = a | state = B) = 1 - P(signal = o | state = B) - P(signal = b | state = B)
                             ifelse(state == "b", beta * (1 - eta), 1 - eta - alpha * (1 - eta)),
                             eta))
  return(matrix(signals, nrow(signal_matrix), ncol(signal_matrix)))
}

# Infer belief state = A before observing new signal 

infer_prior <- function(decision_matrix, prior, generation, decision_summary_a, decision_summary_b, pi) {
  decision_matrix <- matrix(apply(format(decision_matrix), 1, paste, collapse = ""), iterations, 1)
  
  if (order == FALSE) {
    decision_matrix <- vapply(strsplit(decision_matrix, NULL), function(x) paste(sort(x), collapse = ''), '')
    decision_matrix <- matrix(decision_matrix)
  }
  
  if (memory_length != FALSE) {
    if(memory_length < generation) {
    decision_matrix <- substr(decision_matrix, generation - memory_length + 1, generation)
    }
  }
  
  if (prop_naive > 0) {
    
    num_rational <- ceiling(iterations * prop_rational)
    num_naive <- floor(iterations * prop_naive)
    
    if (prop_rational > 0) {
      naive_decision_matrix <- decision_matrix[1:num_naive]
      rational_decision_matrix <- decision_matrix[(num_naive + 1):iterations]
    }
    
    else {
      naive_decision_matrix <- decision_matrix
    }
    
    # P(first agent takes action a | state = A)
    paa <- decision_summary_a[[1, 2]] / iterations
    pba <- decision_summary_a[[2, 2]] / iterations
    pbb <- decision_summary_b[[2, 2]] / iterations
    pab <- decision_summary_b[[1, 2]] / iterations
    
    # Count number of a and b in each action sequence 
    num_a <- lengths(regmatches(naive_decision_matrix, gregexpr("A", naive_decision_matrix)))
    num_b <- lengths(regmatches(naive_decision_matrix, gregexpr("B", naive_decision_matrix)))
    
    naive_prior <- paa^num_a * pba^num_b * pi / ((paa^num_a * pba^num_b * pi) + (pab^num_a * pbb^num_b * (1 - pi)))
    
    if (prop_rational > 0) {
      rational_prior <- as.numeric(prior[c(match(rational_decision_matrix, prior)), 4])
      prior <- rbind(matrix(naive_prior), matrix(rational_prior))
    }
    
    else {
      prior <- naive_prior
    }
    
  }
  
  else {
    prior <- as.numeric(prior[c(match(decision_matrix, prior)), 4])
  }
  
  return(prior)
}

# Infer belief state = A for a naive agent who does not consider the correlation between actions 

# Infer belief state = A if signal = a

infer_if_a <- function(prior) {
  
  # Added a special exception for when prior is 0 or 1, such that if eta or alpha
  # is 0 the inference function won't throw an error for dividing by 0
  
  posterior_with_signal_a <- ifelse(prior == 1, 1,
                                    ifelse(prior == 0, 0,
                                           alpha * (1 - eta) * prior / (alpha * (1 - eta) * prior + 
                                                                          (1 - eta - beta * (1 - eta)) * (1 - prior))))
  
  return(posterior_with_signal_a)
}

# Infer belief state = A if signal = b

# Added a special exception for when prior is 0 or 1, such that if eta or beta
# is 0 the inference function won't throw an error for dividing by 0

infer_if_b <- function(prior) {
  posterior_with_signal_b <- ifelse(prior == 1, 1,
                                    ifelse(prior == 0, 0,
                                           (1 - eta - alpha * (1 - eta)) * prior / 
                                             (((1 - eta - alpha * (1 - eta)) * prior) + beta * (1 - eta) * (1 - prior))))
  
  return(posterior_with_signal_b)
}

# Input a decision history and output a posterior

infer_on_signal <- function(signal_matrix, prior, posterior_with_signal_a, posterior_with_signal_b, generation) {
  posterior <- ifelse(signal_matrix[, generation] == "a", posterior_with_signal_a,
                      ifelse(signal_matrix[, generation] == "b", posterior_with_signal_b,
                             prior))
  return(posterior)
}

#' Check which iterations have herded
#'
#' If the last action was X and a signal of x would cause the next agent to take
#' an action other than X, the iteration has herded
#' 
#' Else, the iteration has not herded

track_herd <- function(posterior_with_signal_a, posterior_with_signal_b, generation) {
  
  # Check which iterations have herded
  
  # If the last action was X and a signal of x would cause the next agent to take
  # an action other than X, the iteration has herded
  
  # Else, the iteration has not herded
  
  decisions_if_a <- decide(posterior_with_signal_a, c(rep("a", times = iterations)))
  decisions_if_b <- decide(posterior_with_signal_b, c(rep("b", times = iterations)))
  
  return(decisions_if_a == decisions_if_b)
  
}

# Create action posterior matrix

infer_on_actions <- function(decision_summary_df_a, decision_summary_df_b, prior, generation){
  temp_df <- dplyr::full_join(x = decision_summary_df_a, y = decision_summary_df_b, 
                              by = c("cat_decision_matrix_a" = "cat_decision_matrix_b"))
  temp_df[is.na(temp_df)] <- 0
  posterior <- (prior * temp_df$Freq.x) / (prior * temp_df$Freq.x + (1 - prior) * temp_df$Freq.y)
  temp_matrix <- as.matrix(cbind(temp_df, posterior))
  return(temp_matrix)
}

# Input a posterior and output a decision

decide <- function(posterior_column, signal_column){
  decisions <- ifelse(posterior_column > 0.5, "A", 
                      ifelse(posterior_column < 0.5, "B",
                             sample(c("A", "B"), replace = TRUE, prob = c(0.5, 0.5))))
  return(decisions)
}