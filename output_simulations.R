library(xlsx)
library(writexl)
library(tidyverse)
library(stringi)

# write.xlsx(correct_herd_rate_a, "sample\ simulation/correct_herd_rate_1hist_0.5_a.xlsx")
# write.xlsx(correct_herd_rate_b, "sample\ simulation/correct_herd_rate_1hist_0.5_b.xlsx")

action_posterior_matrices <- action_posterior_matrices[lapply(action_posterior_matrices, length) > 0]

action_posterior_dfs <- list()
Parameter <- c("Iterations", "P(null)", "Prior state = A", "Signal strength a", "Signal strength b", 
              "P(signal = a | state = A)", "P(signal = b | state = B)",  "Memory length", "Order?", "Rational Prop", "Naive Prop")
Value <- c(iterations, eta, pi, alpha, beta, alpha * (1 - eta), beta * (1 - eta), memory_length, as.character(order), prop_rational, prop_naive)
blank <- rep(NA, length(Parameter))
States <- c("State = A", "State = B", rep(NA, length(Parameter) - 2))
PA <- c(alpha * (1 - eta), 1 - beta * (1 - eta) - eta, rep(NA, length(Parameter) - 2))
PB <- c(1 - alpha * (1 - eta) - eta, beta * (1 - eta), rep(NA, length(Parameter) - 2))
PNull <- c(eta, eta, rep(NA, length(Parameter) - 2))


sheets <- c("Parameters")

action_posterior_dfs[[1]] <- data.frame(Parameter, Value, blank, States, PA, PB, PNull) %>% 
  rename("P(A)" = PA,
         "P(B)" = PB,
         "P(Null)" = PNull,
         "_" = blank)

for (i in 1:length(action_posterior_matrices)) {
  temp <- NULL
  temp <- action_posterior_matrices[[i]] %>% 
    data.frame() %>%
    mutate(posterior = as.numeric(as.character(posterior))) %>% 
    rename("FreqA" = Freq.x, 
           "FreqB" = Freq.y) %>% 
    mutate("Herd" = ifelse(infer_if_b(posterior) > 0.5, "A",
                          ifelse(infer_if_a(posterior) < 0.5, "B", "No"))) %>% 
    rename("Action Sequence" = cat_decision_matrix_a,
           "P(state = A)" = posterior) 
  temp <- temp %>% 
    mutate(FreqA = as.numeric(as.character(FreqA)),
           FreqB = as.numeric(as.character(FreqB)))
  
  herd_freq <- temp %>% 
    group_by(Herd) %>% 
    summarize("A" = sum(FreqA),
              "B" = sum(FreqB))
    
  `_` <- rep(NA, nrow(temp))
  States <- c("State = A", "State = B", rep(NA, nrow(temp) - 2))
  `Prop Herd A` <- c(ifelse("A" %in% herd_freq$Herd, filter(herd_freq, Herd == "A")[[1, 2]] / iterations, 0), 
                     ifelse("A" %in% herd_freq$Herd, filter(herd_freq, Herd == "A")[[1, 3]] / iterations, 0),
                     rep(NA, nrow(temp) - 2))
  `Prop Herd B` <- c(ifelse("B" %in% herd_freq$Herd, filter(herd_freq, Herd == "B")[[1, 2]] / iterations, 0), 
                     ifelse("B" %in% herd_freq$Herd, filter(herd_freq, Herd == "B")[[1, 3]] / iterations, 0),
                     rep(NA, nrow(temp) - 2))
  `Prop Live` <- c(ifelse("No" %in% herd_freq$Herd, filter(herd_freq, Herd == "No")[[1, 2]] / iterations, 0), 
                   ifelse("No" %in% herd_freq$Herd, filter(herd_freq, Herd == "No")[[1, 3]] / iterations, 0),
                   rep(NA, nrow(temp) - 2))
  
  temp <- cbind(temp, `_`, States, `Prop Herd A`, `Prop Herd B`, `Prop Live`)

  temp <- temp %>%
    rename("Freq | State = A" = FreqA,
           "Freq | State = B" = FreqB)
  
  action_posterior_dfs[[i + 1]] <- temp
  sheets <- append(sheets, paste0("Generation ", i))
}

names(action_posterior_dfs) <- sheets

write_xlsx(action_posterior_dfs, paste0("sample\ simulation/action_posterior_dfs_", pi, "_", alpha, "_", beta, "_", order, "_", 
                                        memory_length, "_", prop_rational, ".xlsx"))

# For n = 1, visualize changes in P(A | a) and P(A | b) across generations 

# timeline <- data.frame("Generation" = c(), "P(A | a)" = c(), "P(A | b)" = c())
# 
# for (i in 1:length(action_posterior_matrices)) {
#   temp <- action_posterior_matrices[[i]]
#   timeline[i, 1] = i
#   timeline[i, 2] = as.numeric(temp[1, 4])
#   timeline[i, 3] = as.numeric(temp[2, 4])
# }
# 
# ggplot(timeline) +
#   geom_density()
  

