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

##### calculus of phi ####
# diverse N0/N
liste_tible_result <- dk_countries_tradeve %>%
  group_by(Country) %>%
  group_split(Country)

# calculus of phi
liste_f_countries <- tibble()

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
  
  # calculus of f
  f_result_tibble_countries <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                 times = length(vector_of_ids_rank), 
                                                 list_entry = vector_of_ids_rank,
                                                 N = length(vector_of_ids_rank[[1]])) %>%
    mutate(N = length(vector_of_ids_rank[[1]])) %>%
    mutate(Country = liste_tible_result[[country]]$Country[1])
  
  # output tibble bind
  liste_f_countries <- liste_f_countries %>%
    bind_rows(f_result_tibble_countries)
  
}

##### F ####
liste_f_countries2 <- tibble()

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
  
  # calculus of f
  f_result_tibble_countries <- f_calculus_Finiguez(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                                   times = length(vector_of_ids_rank), 
                                                   list_entry = vector_of_ids_rank,
                                                   N = length(vector_of_ids_rank[[1]])) %>%
    mutate(N = length(vector_of_ids_rank[[1]])) %>%
    mutate(Country = liste_tible_result[[country]]$Country[1])
  
  # output tibble bind
  liste_f_countries2 <- liste_f_countries2 %>%
    bind_rows(f_result_tibble_countries)
  
}

liste_f_countries %>%
  ggplot(aes(x=N0_N, y=Fresult, color = Country)) +
  geom_line(show.legend = FALSE, linewidth = 0.25) +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($W/N_0$)")) +  # WARNING: new notation
  ylab(TeX(r"($\phi$)")) +
  liste_f_countries2 %>%
  ggplot(aes(x=N0_N, y=Fresult, color = Country)) +
  geom_line(linewidth = 0.25) +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($W/N_0$)")) +
  ylab("F")

ggsave(filename = "figures/comparing_F_cities.pdf", width = 18, height = 10, dpi = 300, units = 'cm')
