library(tidyverse)
library(latex2exp)

# functions
source(file = "R/functions.R")

# WARNING: code could be simplified with no computation of all t/T in function f_overlapping() #

### Probability of replacement considering W/Nzero ####
##### US companies ####
companies_ranked <- read_csv(file = 'data/data-reorganized/us-companies.csv')

# list of tibbles
liste_tible_result <- companies_ranked %>% 
  arrange(date, rank) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

# overlapping companies
my_seq_test <- seq(10, 500, 10)
o_companies_N0 <- f_overlapping(study_list = liste_tible_result, 
                                sequence_of_Nzero = my_seq_test)

o_companies_N0 <- o_companies_N0 %>%
  mutate(Nzero_Nmax = Nzero_size/500) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t)

##### Shanghai: universities ####
shangai_ranked <- read_csv(file = 'data/data-reorganized/shanghai-universities.csv')

# diverse N0/N
liste_tible_result <- shangai_ranked %>% 
  arrange(year, rank) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

my_seq_test <- seq(1, 100, 5)
o_shangai_N0 <- f_overlapping(study_list = liste_tible_result, 
                              sequence_of_Nzero = my_seq_test)

o_shangai_N0 <- o_shangai_N0 %>%
  mutate(Nzero_Nmax = Nzero_size/100) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t)

##### chess players FIDE #####
chess_yearly <- read_csv(file = 'data/data-reorganized/chessplayers-FIDE-2004-2019.csv')

# data preparation
liste_tible_result <- chess_yearly %>% 
  filter(rank < 50436) %>%
  arrange(year, month, rank) %>%
  rename(date = year) %>%
  group_by(date, month) %>% 
  select(rowid) %>%
  group_split()

my_seq_test <- seq(10, 50435, 500)
o_FIDE_N0 <- f_overlapping(study_list = liste_tible_result, 
                           sequence_of_Nzero = my_seq_test)

o_FIDE_N0 <- o_FIDE_N0 %>%
  mutate(Nzero_Nmax = Nzero_size/50435) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t)

##### chess players chessmetrics ####
chess_yearly_chessmetrics <- read_csv(file = 'data/data-reorganized/chessplayers-chessmetrics-1966-2000.csv')

# diverse N0/N
liste_tible_result <- chess_yearly_chessmetrics %>% 
  arrange(year, ranking) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

my_seq_test <- seq(10, 500, 10)
o_chessmetrics_N0 <- f_overlapping(study_list = liste_tible_result, 
                                   sequence_of_Nzero = my_seq_test)

o_chessmetrics_N0 <- o_chessmetrics_N0 %>%
  mutate(Nzero_Nmax = Nzero_size/500) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t)

##### soccer teams ####
soccer <- read_csv(file = 'data/data-reorganized/soccer-1954-2023.csv')

# diverse N0/N
liste_tible_result <- soccer %>% 
  filter(rank < 136) %>%
  arrange(year, rank) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

my_seq_test <- seq(1, 135, 5)
o_soccer_N0 <- f_overlapping(study_list = liste_tible_result, 
                             sequence_of_Nzero = my_seq_test)

o_soccer_N0 <- o_soccer_N0 %>%
  mutate(Nzero_Nmax = Nzero_size/135) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t)

# create one tibble for all systems
overlapping_all <- o_chessmetrics_N0 %>%
  mutate(systems = "chess: chessmetrics") %>%
  bind_rows(
    o_FIDE_N0 %>%
      mutate(systems = "chess: FIDE")
  ) %>%
  bind_rows(
    o_soccer_N0 %>%
      mutate(systems = "soccer teams")
  ) %>%
  bind_rows(
    o_companies_N0 %>%
      mutate(systems = "companies")
  ) %>%
  bind_rows(
    o_shangai_N0 %>%
      mutate(systems = "universities")
  )


### Compute probability of replacement ####
list_of_overlapping <- overlapping_all %>%
  group_by(systems, Nzero_Nmax) %>%
  group_split()

replacement <- tibble()

for (i in 1:length(list_of_overlapping)) {
  
  # compute probability
  size <- nrow(list_of_overlapping[[i]])
  
  ls <- list_of_overlapping[[i]]
  gs <- (ls$overlapping_index[1] - ls$overlapping_index[size]) / (ls$time_t_reality[1] - ls$time_t_reality[size])
  
  # output
  replacement <- replacement %>%
    bind_rows(
      tibble(
        systems = ls$systems[1],
        Nzero_size = ls$Nzero_size[1],
        Nzero_Nmax = ls$Nzero_Nmax[1],
        preplacement = round(x = gs, digits = 5)
      )
    )
  
}

# global slope
replacement %>%
  ggplot(mapping = aes(x = Nzero_Nmax, y = preplacement, colour = systems)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(legend.position = "left") +
  scale_x_continuous(name = TeX(r"($W/N_0$)")) +
  scale_y_continuous(name = TeX(r"($P_r$)"))

ggsave(filename = "figures/proba_replacement_open_systems.pdf", width = 18, height = 12, units = "cm", dpi = 300)
