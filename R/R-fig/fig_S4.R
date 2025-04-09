library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(nlstools) # for fit

# functions
source(file = "R/functions.R")


#### European cities ####
dk_countries_tradeve <- read_csv(file = 'data/data-reorganized/european_cities.csv')

##### calculus of phi ####
# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")

# diverse N0/N
liste_tible_result <- dk_countries_tradeve %>%
  group_by(Country) %>%
  group_split(Country)

# phi
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

##### fit [0,T] : exponential distribution ####
#### all countries
liste_outputs <- liste_f_countries %>%
  group_by(Country) %>%
  group_split()

fitting_liste_results <- list()

for (i in 1:length(liste_outputs)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric_exponential, # formula made
                                    data = liste_outputs[[i]], # df for fit > ok for tibble
                                    start = list(ps = 0.00001, pd = 0, ro = 0.1), # starting model in
                                    lower = list(ps = 0, pd = 0, ro = 0.00000000000001),
                                    upper = list(ps = 1, pd = 1, ro = 1000000000000000), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    Country = liste_outputs[[i]]$Country[1],
    ps = round(extractionparam[1], 4),
    ps_st_error = summaryparam[1,2],
    pd = round(extractionparam[2], 4),
    pd_st_error = summaryparam[2,2],
    ro = round(extractionparam[3],4),
    ro_N = round(extractionparam[3],4)/liste_outputs[[i]]$N[1],
    ro_st_error = summaryparam[3,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results <-  output_fit_results %>%
    bind_rows(constructiontibble)
}

### ploting multiple fits
par(mfrow= c(3,3))

for (i in 1:length(fitting_liste_results)) {
  plot(liste_outputs[[i]]$N0_N, liste_outputs[[i]]$Fresult, pch=19, cex=0.5, xlab= TeX(r"($W/N_0$)"), ylab = TeX(r"($\Phi$)"), # WARNING: new notation
       main=output_fit_results[i,]$Country)
  lines(liste_outputs[[i]]$N0_N, predict(fitting_liste_results[[i]]), col='red')
}

write_csv(x = output_fit_results %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_0T_all_cities_exponential.csv")
