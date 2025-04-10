library(tidyverse)
library(latex2exp)
library(nlstools) # for fit

# functions
source(file = "R/functions.R")

### Overlapping frequency in W/Nzero ####
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

liste_overlapping <- overlapping_all %>%
  group_by(systems, Nzero_size) %>%
  rename(Overlapfit = overlapping_index,
         t_T = time_t_minus_on_t) %>%
  group_split()

### Fitting with exponential ####
liste_overlapping <- liste_overlapping[-229] # Convergence failure on universities tibble when W = 1
liste_overlapping <- liste_overlapping[-202] # Failure on soccer teams tibble when W = 1

fitting_liste_results <- list()

for (i in 1:length(liste_overlapping)) {
  fitting_liste_results[[i]] <- nls(formula = formula_overlapping, # formula made
                                    data = liste_overlapping[[i]], # df for fit > ok for tibble
                                    start = list(a = 0.01), # starting model in
                                    # lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                                    # upper = list(ps = 1, pd = 1, alpha = 1), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    systems = liste_overlapping[[i]]$systems[1],
    W_Nzero = liste_overlapping[[i]]$Nzero_Nmax[1],
    a = round(extractionparam[1], digits = 4),
    a_st_error = round(summaryparam[1,2], digits = 4),
    rss = round(fitting_liste_results[[i]]$m$deviance(), 4)
  )
  output_fit_results <-  output_fit_results %>%
    bind_rows(constructiontibble)
}


# Visualization
output_fit_results %>%
  ggplot(mapping = aes(x = W_Nzero, y = a, color = systems)) +
  geom_line(linewidth = 0.8)+
  geom_pointrange(aes(ymin=a-a_st_error, ymax=a+a_st_error), size = 0, show.legend = FALSE)+
  theme_bw() +
  theme(legend.position = "left") +
  scale_x_continuous(name = TeX(r"($W/N_0$)")) +
  scale_y_continuous(name = TeX(r"($a$)"))

ggsave(filename = "figures/fit_exp_a_vs_WN0.pdf", width = 18, height = 12, units = "cm", dpi = 300)



