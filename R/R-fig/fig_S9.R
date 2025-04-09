library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(nlstools) # for fit

# functions
source(file = "R/functions.R")

### Overlapping frequency ####
#### US companies ####
companies_ranked <- read_csv(file = 'data/data-reorganized/us-companies.csv')

# list of tibbles
liste_tible_result <- companies_ranked %>% 
  arrange(date, rank) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

# overlapping companies
o_companies <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)

#### Shanghai: universities ####
shangai_ranked <- read_csv(file = 'data/data-reorganized/shanghai-universities.csv')

# diverse N0/N
liste_tible_result <- shangai_ranked %>% 
  arrange(year, rank) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

o_shangai <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)


#### chess players FIDE #####
chess_yearly <- read_csv(file = 'data/data-reorganized/chessplayers-FIDE-2004-2019.csv')

# data preparation
liste_tible_result <- chess_yearly %>% 
  filter(rank < 50436) %>%
  arrange(year, month, rank) %>%
  rename(date = year) %>%
  group_by(date, month) %>% 
  select(rowid) %>%
  group_split()

o_FIDE <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)

#### chess players chessmetrics ####
chess_yearly_chessmetrics <- read_csv(file = 'data/data-reorganized/chessplayers-chessmetrics-1966-2000.csv')

# diverse N0/N
liste_tible_result <- chess_yearly_chessmetrics %>% 
  arrange(year, ranking) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

o_chessmetrics <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)


#### soccer teams ####
soccer <- read_csv(file = 'data/data-reorganized/soccer-1954-2023.csv')

# diverse N0/N
liste_tible_result <- soccer %>% 
  filter(rank < 136) %>%
  arrange(year, rank) %>%
  rename(date = year) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

o_soccer <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)

#### Overlapping indices ####
overlapping_all <- o_chessmetrics %>%
  mutate(system = "chess: chessmetrics") %>%
  bind_rows(
    o_FIDE %>%
      mutate(system = "chess: FIDE")
  ) %>%
  bind_rows(
    o_soccer %>%
      mutate(system = "soccer teams")
  ) %>%
  bind_rows(
    o_companies %>%
      mutate(system = "companies")
  ) %>%
  bind_rows(
    o_shangai %>%
      mutate(system = "universities")
  )

liste_overlapping <- overlapping_all %>%
  group_by(system) %>%
  rename(Overlapfit = overlapping_index) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(t_T = time_t_minus/max_t) %>%
  group_split()


### Fitting with exponential ####
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
    systems = liste_overlapping[[i]]$system[1],
    a = round(extractionparam[1], digits = 4),
    a_st_error = round(summaryparam[1,2], digits = 4),
    rss = round(fitting_liste_results[[i]]$m$deviance(), 4)
  )
  output_fit_results <-  output_fit_results %>%
    bind_rows(constructiontibble)
}

### ploting multiple fits
par(mfrow= c(2,3))

for (i in 1:length(fitting_liste_results)) {
  plot(liste_overlapping[[i]]$t_T, liste_overlapping[[i]]$Overlapfit, pch=19, cex=0.5, xlab= TeX(r"($t/T$)"), ylab = TeX(r"($O$)"), # WARNING: new notation
       main=output_fit_results[i,]$systems, ylim = c(0,1))
  lines(liste_overlapping[[i]]$t_T, predict(fitting_liste_results[[i]]), col='red')
}

write_csv(x = output_fit_results, 
          file = "output_data/fit_overlapping_exponential.csv", 
          append = FALSE)
