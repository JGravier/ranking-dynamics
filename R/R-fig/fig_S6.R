library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(latex2exp)
library(patchwork)

# functions
source(file = "R/functions.R")

### US companies ####
companies_ranked <- read_csv(file = 'data/data-reorganized/us-companies.csv')

# diverse N0/N
liste_tible_result <- companies_ranked %>% 
  arrange(date, rank) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

# calculus of phi
phi_result_tibble_companies <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'companies')

# calculus of f
f_result_tibble_companies <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'companies')


### Shanghai: universities ####
shangai_ranked <- read_csv(file = 'data/data-reorganized/shanghai-universities.csv')

# diverse N0/N
liste_tible_result <- shangai_ranked %>% 
  arrange(year, rank) %>%
  group_by(year) %>% 
  select(rowid) %>% 
  group_split()

# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

# calculus of phi
phi_result_tibble_universities <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'universities')

# calculus of f
f_result_tibble_universities <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'universities')


### chess players FIDE #####
chess_yearly <- read_csv(file = 'data/data-reorganized/chessplayers-FIDE-2004-2019.csv')

# data preparation
liste_tible_result <- chess_yearly %>% 
  filter(rank < 50436) %>%
  arrange(year, month, rank) %>%
  group_by(year, month) %>% 
  select(rowid) %>%
  group_split()

# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

# calculus of phi
phi_result_tibble_chessfide <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 100), 
                                                    times = length(vector_of_ids_rank), 
                                                    list_entry = vector_of_ids_rank,
                                                    N = length(vector_of_ids_rank[[k]]), vec_not_continuous = TRUE) %>%
  mutate(N = length(vector_of_ids_rank[[k]])) %>%
  mutate(name = 'chess: FIDE')

# calculus of f
f_result_tibble_chessfide <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 100), 
                                                    times = length(vector_of_ids_rank), 
                                                    list_entry = vector_of_ids_rank,
                                                    N = length(vector_of_ids_rank[[k]]), vec_not_continuous = TRUE) %>%
  mutate(N = length(vector_of_ids_rank[[k]])) %>%
  mutate(name = 'chess: FIDE')


### chess players chessmetrics ####
chess_yearly_chessmetrics <- read_csv(file = 'data/data-reorganized/chessplayers-chessmetrics-1966-2000.csv')

# diverse N0/N
liste_tible_result <- chess_yearly_chessmetrics %>% 
  arrange(year, ranking) %>%
  group_by(year) %>% 
  select(rowid) %>% 
  group_split()

# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

# calculus of phi
phi_result_tibble_chessmetric <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'chess: chessmetrics')

# calculus of f
f_result_tibble_chessmetric <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'chess: chessmetrics')


### soccer teams ####
soccer <- read_csv(file = 'data/data-reorganized/soccer-1954-2023.csv')

# diverse N0/N
liste_tible_result <- soccer %>% 
  filter(rank < 136) %>%
  arrange(year, rank) %>%
  group_by(year) %>% 
  select(rowid) %>% 
  group_split()

# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

# calculus of phi
phi_result_tibble_soccer <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                   times = length(vector_of_ids_rank), 
                                                   list_entry = vector_of_ids_rank,
                                                   N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'soccer teams')

# calculus of f
f_result_tibble_soccer <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                   times = length(vector_of_ids_rank), 
                                                   list_entry = vector_of_ids_rank,
                                                   N = length(vector_of_ids_rank[[1]])) %>%
  mutate(N = length(vector_of_ids_rank[[1]])) %>%
  mutate(name = 'soccer teams')


### Ploting result ####
phi_data_plot <- phi_result_tibble_companies %>%
  bind_rows(phi_result_tibble_universities) %>%
  bind_rows(phi_result_tibble_chessfide) %>%
  bind_rows(phi_result_tibble_chessmetric) %>%
  bind_rows(phi_result_tibble_soccer)

f_data_plot <- f_result_tibble_companies %>%
  bind_rows(f_result_tibble_universities) %>%
  bind_rows(f_result_tibble_chessfide) %>%
  bind_rows(f_result_tibble_chessmetric) %>%
  bind_rows(f_result_tibble_soccer)

# plot
phi_data_plot %>%
  ggplot(aes(x=N0_N, y=Fresult, color = name, group=name)) +
  geom_line(show.legend = FALSE, linewidth = 0.8) +
  theme_bw() +
  xlab(TeX(r"($W/N_0$)")) + # WARNING: new notation
  ylab(TeX(r"($\phi$)")) +
  f_data_plot %>%
  ggplot(aes(x=N0_N, y=Fresult, color = name)) +
  geom_line(linewidth = 0.8) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab(TeX(r"($W/N_0$)")) + # WARNING: new notation
  ylab("F")

ggsave(filename = "figures/phi_and_F_opensystems_N0_Nmax.pdf", width = 18, height = 10, dpi = 300, units = 'cm')

