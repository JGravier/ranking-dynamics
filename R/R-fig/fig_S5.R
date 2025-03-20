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

#### DE ####
ranking_DE <- liste_tible_result[[2]]

listerankde <- ranking_DE %>%
  group_by(periods) %>%
  select(rowid) %>%
  group_split()

##### phi of t ####
# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(listerankde)) {
  vector_of_ids_rank[[k]] <- listerankde[[k]]$rowid
}

# calculus of phi for each t
f_result_tibble_de <- tibble()

for (i in 2:length(vector_of_ids_rank)) {
  intermediaryresult <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1),
                                          times = 2,
                                          list_entry = list(vector_of_ids_rank[[i-1]], vector_of_ids_rank[[i]]), 
                                          N = length(vector_of_ids_rank[[k]])) %>%
    mutate(N = length(vector_of_ids_rank[[k]])) %>%
    mutate(timet = paste0('t=[',i-1,',',i,']'))
  
  f_result_tibble_de <- f_result_tibble_de %>%
    bind_rows(intermediaryresult)
}

a <- f_result_tibble_de %>% 
  rename(Country=timet) %>%
  mutate(factorvisu = as.numeric(str_sub(Country, 4, 4))) %>%
  bind_rows(liste_f_countries %>%
              filter(Country == 'DE') %>%
              mutate(Country = 'T') %>%
              mutate(factorvisu = 6)) %>%
  ggplot(aes(x=N0_N, y=Fresult, color = reorder(Country, factorvisu))) +
  geom_line(linewidth=0.5) +
  scale_color_manual(values = c('#E69F00', '#56B4E9', '#009E73', '#F0e442', '#0072b2','#000000')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.75),
        legend.background = element_rect(fill = "white", color = "grey30"),
        axis.title.x = element_blank(),
        axis.text = element_text(size=12)) +
  ylab(TeX(r"($\Phi$)")) +
  labs(title = 'A')

##### fit phi all t: power law ####
liste_outputs_de <- f_result_tibble_de %>%
  group_by(timet) %>%
  group_split()

fitting_liste_results <- list()

for (i in 1:length(liste_outputs_de)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric, # formula made
                                    data = liste_outputs_de[[i]], # df for fit > ok for tibble
                                    start = list(ps = 0.00001, pd = 0, alpha = -1), # starting model in
                                    lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                                    upper = list(ps = 1, pd = 1, alpha = 1), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results_de <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    timet = liste_outputs_de[[i]]$timet[1],
    ps = round(extractionparam[1], 4),
    ps_st_error = summaryparam[1,2],
    pd = round(extractionparam[2], 4),
    pd_st_error = summaryparam[2,2],
    alpha = round(extractionparam[3],4),
    alpha_st_error = summaryparam[3,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_de <- output_fit_results_de %>%
    bind_rows(constructiontibble)
}

# output of fit
write_csv(x = output_fit_results_de %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_de_t_powerlaw.csv")

### ploting: ps, pd and alpha
b <- output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, alpha), names_to = 'param', values_to = 'paramvalues') %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  scale_color_manual(values = c('#77ab43','#008fd5', '#FF2700')) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.55),
        legend.background = element_rect(fill = "white", color = "grey30"),
        axis.title.x = element_blank(), axis.text = element_text(size=12)) +
  ylab(TeX(r"($parameters$)")) +
  labs(title = 'B')

##### fit phi all t: exponential ####
fitting_liste_results <- list()

for (i in 1:length(liste_outputs_de)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric_exponential, # formula made
                                    data = liste_outputs_de[[i]], # df for fit > ok for tibble
                                    start = list(ps = 0.00001, pd = 0, ro = 0.1), # starting model in
                                    lower = list(ps = 0, pd = 0, ro = 0.0000000000001),
                                    upper = list(ps = 1, pd = 1, ro = 100000000000000), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results_de <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    timet = liste_outputs_de[[i]]$timet[1],
    ps = round(extractionparam[1], 4),
    ps_st_error = summaryparam[1,2],
    pd = round(extractionparam[2], 4),
    pd_st_error = summaryparam[2,2],
    ro = round(extractionparam[3],4),
    ro_N = round(extractionparam[3],4)/liste_outputs_de[[i]]$N[1],
    ro_st_error = summaryparam[3,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_de <- output_fit_results_de %>%
    bind_rows(constructiontibble)
}

# output of fit
write_csv(x = output_fit_results_de %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_de_t_de_exponential.csv")

### ploting: ps, pd and ro/N
c <- output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, ro_N), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(param = if_else(param == 'ro_N', 'ro/N', param)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  scale_color_manual(values = c('#008fd5', '#FF2700','#77ab43')) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.55, 0.70),
        legend.background = element_rect(fill = "white", color = "grey30"), 
        axis.title.x = element_blank(), axis.text = element_text(size=12)) +
  ylab(TeX(r"($parameters$)")) +
  labs(title = 'C')

#### FR ####
ranking_fr <- liste_tible_result[[4]]

listerank_fr <- ranking_fr %>%
  group_by(periods) %>%
  select(rowid) %>%
  group_split()

##### phi of t ####
# create vector of ranking of IDs of elements
vector_of_ids_rank <- list()

for (k in 1:length(listerank_fr)) {
  vector_of_ids_rank[[k]] <- listerank_fr[[k]]$rowid
}

# calculus of phi for each t
f_result_tibble_fr <- tibble()

for (i in 2:length(vector_of_ids_rank)) {
  intermediaryresult <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1),
                                          times = 2,
                                          list_entry = list(vector_of_ids_rank[[i-1]], vector_of_ids_rank[[i]]), 
                                          N = length(vector_of_ids_rank[[k]])) %>%
    mutate(N = length(vector_of_ids_rank[[k]])) %>%
    mutate(timet = paste0('t=[',i-1,',',i,']'))
  
  f_result_tibble_fr <- f_result_tibble_fr %>%
    bind_rows(intermediaryresult)
}

d <- f_result_tibble_fr %>% 
  rename(Country=timet) %>%
  mutate(factorvisu = as.numeric(str_sub(Country, 4, 4))) %>%
  bind_rows(liste_f_countries %>%
              filter(Country == 'UK') %>%
              mutate(Country = 'T') %>%
              mutate(factorvisu = 6)) %>%
  ggplot(aes(x=N0_N, y=Fresult, color = reorder(Country, factorvisu))) +
  geom_line(linewidth=0.5) +
  scale_color_manual(values = c('#E69F00', '#56B4E9', '#009E73', '#F0e442', '#0072b2','#000000')) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.75),
        legend.background = element_rect(fill = "white", color = "grey30"),
        axis.text = element_text(size=12)) +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\Phi$)"))

##### fit phi all t: power law ####
liste_outputs_fr <- f_result_tibble_fr %>%
  group_by(timet) %>%
  group_split()

fitting_liste_results <- list()

for (i in 1:length(liste_outputs_fr)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric, # formula made
                                    data = liste_outputs_fr[[i]], # df for fit > ok for tibble
                                    start = list(ps = 0.00001, pd = 0, alpha = -1), # starting model in
                                    lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                                    upper = list(ps = 1, pd = 1, alpha = 1), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results_fr <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    timet = liste_outputs_de[[i]]$timet[1],
    ps = round(extractionparam[1], 4),
    ps_st_error = summaryparam[1,2],
    pd = round(extractionparam[2], 4),
    pd_st_error = summaryparam[2,2],
    alpha = round(extractionparam[3],4),
    alpha_st_error = summaryparam[3,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_fr <- output_fit_results_fr %>%
    bind_rows(constructiontibble)
}

# output of fit
write_csv(x = output_fit_results_fr %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_de_t_fr_powerlaw.csv")

### ploting: ps, pd and alpha
e <- output_fit_results_fr %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, alpha), names_to = 'param', values_to = 'paramvalues') %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  scale_color_manual(values = c('#77ab43','#008fd5', '#FF2700')) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.55),
        legend.background = element_rect(fill = "white", color = "grey30"),
        axis.text = element_text(size=12)) +
  xlab(TeX(r"($t/T$)")) +
  ylab(TeX(r"($parameters$)"))

##### fit phi all t: exponential ####
fitting_liste_results <- list()

for (i in 1:length(liste_outputs_fr)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric_exponential, # formula made
                                    data = liste_outputs_fr[[i]], # df for fit > ok for tibble
                                    start = list(ps = 0.00001, pd = 0, ro = 0.1), # starting model in
                                    lower = list(ps = 0, pd = 0, ro = 0.0000000000001),
                                    upper = list(ps = 1, pd = 1, ro = 100000000000000), 
                                    algorithm = "port", nls.control(maxiter = 10000))
  
}

output_fit_results_fr2 <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  summaryparam <- summary(object = fitting_liste_results[[i]])
  summaryparam <- summaryparam$parameters
  constructiontibble <- tibble(
    timet = liste_outputs_de[[i]]$timet[1],
    ps = round(extractionparam[1], 4),
    ps_st_error = summaryparam[1,2],
    pd = round(extractionparam[2], 4),
    pd_st_error = summaryparam[2,2],
    ro = round(extractionparam[3],4),
    ro_N = round(extractionparam[3],4)/liste_outputs_de[[i]]$N[1],
    ro_st_error = summaryparam[3,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_fr2 <- output_fit_results_fr2 %>%
    bind_rows(constructiontibble)
}

# output of fit
write_csv(x = output_fit_results_fr2 %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_t_fr_exponential.csv")

### ploting: ps, pd and ro/N
f <- output_fit_results_fr2 %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, ro_N), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(param = if_else(param == 'ro_N', 'ro/N', param)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  scale_color_manual(values = c('#008fd5', '#FF2700','#77ab43')) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.55, 0.70),
        legend.background = element_rect(fill = "white", color = "grey30"),
        axis.text = element_text(size=12)) +
  xlab(TeX(r"($t/T$)")) +
  ylab(TeX(r"($parameters$)"))

#### All plots ####
(a + b + c)/(d + e + f)
ggsave(filename = 'figures/DE_FR_fit_timeseries.pdf', width = 33, height = 27, dpi = 300, units = 'cm')
