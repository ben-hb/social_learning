library(xlsx)
library(writexl)
library(tidyverse)

# write.xlsx(correct_herd_rate_a, "sample\ simulation/correct_herd_rate_1hist_0.5_a.xlsx")
# write.xlsx(correct_herd_rate_b, "sample\ simulation/correct_herd_rate_1hist_0.5_b.xlsx")

action_posterior_matrices <- action_posterior_matrices[lapply(action_posterior_matrices, length) > 0]

action_posterior_dfs <- list()
Parameter = c("Iterations", "P(null)", "Prior state = A", "Signal strength a", "Signal strength b", 
              "P(signal = a | state = A)", "P(signal = b | state = B)",  "Memory length", "Order?")
Value = c(iterations, eta, pi, alpha, beta, alpha * (1 - eta), beta * (1 - eta), memory_length, as.character(order))

sheets <- c("Parameters")

for (i in 1:length(action_posterior_matrices)) {
  action_posterior_dfs[[1]] <- data.frame(Parameter, Value)
  action_posterior_dfs[[i + 1]] <- data.frame(action_posterior_matrices[[i]]) %>% 
    rename("Action Sequence" = cat_decision_matrix_a,
           "Frequency | state = A" = Freq.x,
           "Frequency | state = B" = Freq.y,
           "P(state = A)" = posterior)
  sheets <- append(sheets, paste0("Generation ", i))
}

names(action_posterior_dfs) <- sheets

write_xlsx(action_posterior_dfs, paste0("sample\ simulation/action_posterior_dfs_", pi, "_", alpha, "_", beta, "_", order, "_", memory_length, ".xlsx"))

