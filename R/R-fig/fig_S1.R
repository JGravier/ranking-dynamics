library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(latex2exp)
library(patchwork)

# functions
source(file = "R/functions.R")


#### European cities ####
dk_countries_tradeve <- read_csv(file = 'data/data-reorganized/european_cities.csv')

##### Ft ####
# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")

# diverse N0/N
liste_tible_result <- dk_countries_tradeve %>%
  group_by(Country) %>%
  group_split(Country)

liste_ft_countries <- tibble()

for (country in 1:length(liste_tible_result)) {
  # create vector of ranking of IDs of elements (here cities) for each country
  tibble_of_ids_rank <- liste_tible_result[[country]] %>% 
    group_by(periods) %>% 
    select(rowid) %>% 
    group_split()
  
  vector_of_ids_rank <- list()
  
  for (k in 1:length(tibble_of_ids_rank)) {
    vector_of_ids_rank[[k]] <- tibble_of_ids_rank[[k]]$rowid
  }
  
  # calculus of Ft
  f_result_tibble_countries <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                             times = length(vector_of_ids_rank), 
                                             list_entry = vector_of_ids_rank,
                                             N = length(vector_of_ids_rank[[1]])) %>%
    mutate(N = length(vector_of_ids_rank[[1]])) %>%
    mutate(Country = liste_tible_result[[country]]$Country[1])
  
  # output tibble bind
  liste_ft_countries <- liste_ft_countries %>%
    bind_rows(f_result_tibble_countries)
  
}

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

# calculus of Ft
ft_result_tibble_companies <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                            times = length(vector_of_ids_rank), 
                                            list_entry = vector_of_ids_rank,
                                            N = length(vector_of_ids_rank[[1]])) 

#### Shanghai: universities ####
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

# calculus of Ft
ft_result_tibble_shanghai <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                            times = length(vector_of_ids_rank), 
                                            list_entry = vector_of_ids_rank,
                                            N = length(vector_of_ids_rank[[1]]))


#### chess players FIDE #####
chess_yearly <- read_csv(file = 'data/data-reorganized/chessplayers-FIDE-2004-2019.csv')

#### note: N_max is very diverse through time (x10)
npoblem <- chess_yearly %>%
  group_by(year, month) %>%
  summarise(n=n())

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

# calculus of Ft
ft_result_tibble_chessplayers <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 100), 
                                           times = length(vector_of_ids_rank), 
                                           list_entry = vector_of_ids_rank,
                                           N = length(vector_of_ids_rank[[k]]), 
                                           vec_not_continuous = TRUE)


#### chess players chessmetrics ####
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

# calculus of Ft
ft_result_tibble_chessmetrics <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                           times = length(vector_of_ids_rank), 
                                           list_entry = vector_of_ids_rank,
                                           N = length(vector_of_ids_rank[[1]]))


#### soccer teams ####
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

# calculus of Ft
ft_result_tibble_soccer <- f_calculus_Ft(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                               times = length(vector_of_ids_rank), 
                                               list_entry = vector_of_ids_rank,
                                               N = length(vector_of_ids_rank[[1]]))

#### Ploting result ####
data_plot <- liste_ft_countries %>%
  rename(data = Country) %>%
  mutate(data = paste0('cities: ', data)) %>%
  mutate(times=times-2) %>% # for visu
  mutate(times2 = times/max(times)) %>%
  bind_rows(ft_result_tibble_companies %>% mutate(data = 'companies') %>% 
              mutate(times=times-2) %>%mutate(times2 = times/max(times))) %>%
  bind_rows(ft_result_tibble_shanghai %>% mutate(data = 'universities') %>% 
              mutate(times=times-2) %>%mutate(times2 = times/max(times))) %>%
  bind_rows(ft_result_tibble_chessplayers %>% mutate(data = 'chess FIDE') %>%
              mutate(times=times-2) %>%mutate(times2 = times/max(times))) %>%
  bind_rows(ft_result_tibble_chessmetrics %>% mutate(data = 'chessmetric') %>%
              mutate(times=times-2) %>%mutate(times2 = times/max(times))) %>%
  bind_rows(ft_result_tibble_soccer %>% mutate(data = 'soccer teams') %>%
              mutate(times=times-2) %>%mutate(times2 = times/max(times)))

a <- data_plot %>%
  filter(N0_N > 0.1) %>%
  ggplot(aes(x=times2, y=Ft, color = N0_N, group=N0_N)) +
  geom_line(alpha=0.4) +
  scale_color_viridis_c() +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_x_continuous(name = TeX(r"($t/T$)"), limits = c(0,1), breaks = c(0,0.5,1)) +
  ylab(TeX(r"($Ft$)")) + 
  labs(title = "A", color = TeX(r"($N_0/N_{max}$)")) +
  facet_wrap(~data, scales = 'free', ncol = 5)

b <- data_plot %>%
  group_by(data, times2) %>%
  summarise(mean = mean(Ft), cv = sd(Ft)) %>%
  ggplot(aes(x=times2, y=mean)) +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  geom_line() +
  geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 0.05) +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t/T$)"), limits = c(0,1), breaks = c(0,0.5,1)) +
  ylab(TeX(r"($\bar{Ft}$)")) +
  labs(title = "B") +
  facet_wrap(~data, scales = 'free', ncol = 5)

a/b

ggsave(filename = "figures/Ft_data.pdf", width = 22, height = 24, dpi = 300, units = 'cm')






