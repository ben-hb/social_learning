library(tidyverse)
library(stringdist)

# Analyzes results from action_posterior_matrices to assess instances of anti-imitation

# Anti-imitation occurs when substituting a B action for an A action in an
# agent's observed action sequence makes that agent more likely to believe that
# state B is the correct statev

sample_df <- action_posterior_dfs[[6]] 

sample_df <- sample_df %>% 
  mutate(numb = str_count("B", sample_df[[1]]))

if (adist > 0 && adist == str_co)
adist("aaaa", "bbbb")
str_count("ababb", "b")

for (i in 1:nrow(sample_df)) {
  adist(sample_df[i, 1], sample_df[[1]])
  str_count(sample_df[[1]], B)
}

adist(sample_df[1, 1], sample_df[[1]])
