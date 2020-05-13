library(tidyverse)
library(xlsx)
library(readxl)
library(viridis)
library(akima)

correct_herd_rate_0.1_a <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.1_a.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta) %>% 
  mutate(alpha = as.numeric(alpha),
         beta = as.numeric(beta),
         rate = as.numeric(rate)) %>% 
  replace(values = 0)
correct_herd_rate_0.1_b <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.1_b.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.2_a <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.2_a.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.2_b <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.2_b.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.3_a <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.3_a.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.3_b <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.3_b.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.4_a <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.4_a.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.4_b <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.4_b.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.5_a <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.5_a.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)
correct_herd_rate_0.5_b <- 
  read_excel("/Users/benjaminhoffner-brodsky/Dropbox/Herd\ Immunity/sample_simulations/05_12_20/correct_herd_rate_0.5_b.xlsx") %>% 
  rename(beta = `...1`) %>% 
  gather(key = alpha, value = rate, -beta)

correct_herd_rate_0.1_a_interp <- interp(x = correct_herd_rate_0.1_a$beta, y = correct_herd_rate_0.1_a$alpha,
                                         z = correct_herd_rate_0.1_a$rate, nx = 100, ny = 100)

ggplot(correct_herd_rate_0.1_a, aes(x = alpha, y = beta, z = rate)) + 
  stat_summary_2d(binwidth = 0.05) + 
  # scale_fill_viridis(option = "magma") + 
  labs(
    x = "Alpha",
    y = "Beta",
    title = ""
  )
 

