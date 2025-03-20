library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(latex2exp)
library(arrow)
library(readxl)
library(patchwork)
library(nlstools) # for fit
library(scales)

# functions
source(file = "functions.R")


#### System of cities ####
tradeve <- read_excel(path = "data/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

# creation of ranking by countries
dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011)))

##### calculus of phi ####

# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")

# diverse N0/N
liste_tible_result <- dk_countries_tradeve %>%
  filter(Country %in% liste_pays) %>%
  select(Country, rang_1961:rang_2011) %>%
  mutate(rowid = row_number()) %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "periods", values_to = "rank") %>%
  arrange(rank, Country, periods) %>%
  group_by(Country) %>%
  group_split(Country)


# calculus of f/phi
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

liste_f_countries

###### viz ####
liste_f_countries %>%
  ggplot(aes(x=N0_N, y=Fresult, color = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\Phi$)")) +
  labs(title = "Cities")

ggsave(filename = "figures/phi_cities.png", width = 15, height = 12, dpi = 300, units = 'cm')
ggsave(filename = "figures/phi_cities.pdf", width = 15, height = 12, dpi = 300, units = 'cm')


##### F Iniguez ####
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
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\phi$)")) +
  #labs(title = "Cities") + 
  liste_f_countries2 %>%
  ggplot(aes(x=N0_N, y=Fresult, color = Country)) +
  geom_line(linewidth = 0.25) +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab("F")

ggsave(filename = "figures/comparing_F_cities.png", width = 18, height = 10, dpi = 300, units = 'cm')
ggsave(filename = "figures/comparing_F_cities.pdf", width = 18, height = 10, dpi = 300, units = 'cm')

###### Ft #######
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

b <- liste_ft_countries %>%
  mutate(times2 = times/max(times)) %>%
  #filter(N0_N > 0.25) %>%
  ggplot(aes(x=times2, y=Ft, color = N0_N, group=N0_N)) +
  geom_line(alpha=0.4) +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($t/T$)")) +
  ylab(TeX(r"($Ft$)")) + 
  labs(title = "A") +
  facet_wrap(~Country, scales = 'free')

a <- liste_ft_countries %>%
  mutate(times2 = times/(max(times))) %>%
  group_by(Country, times2) %>%
  summarise(mean = mean(Ft), cv = sd(Ft)) %>%
  ggplot(aes(x=times2, y=mean)) +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  geom_line() +
  geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 1) +
  theme_bw() +
  xlab(TeX(r"($t/T$)")) +
  ylab(TeX(r"($Ft$)")) +
  labs(title = "B") +
  facet_wrap(~ Country)

b/a

ggsave(filename = "figures/Ft_cities1.png", width = 18, height = 20, dpi = 300, units = 'cm')
ggsave(filename = "figures/Ft_cities1.pdf", width = 18, height = 20, dpi = 300, units = 'cm')


liste_ft_countries %>%
  filter(N0_N > 0.2) %>%
  ggplot(aes(x=times, y=Ft, color = N0_N, group=N0_N)) +
  geom_line(alpha=0.4) +
  scale_color_viridis_c() +
  theme_bw() +
  xlab(TeX(r"($t$)")) +
  ylab(TeX(r"($Ft$)")) +
  labs(title = "Cities") +
  facet_wrap(~Country)


##### fit [0,T] : power law distribution ####
#### all countries
liste_outputs <- liste_f_countries %>%
  group_by(Country) %>%
  group_split()

fitting_liste_results <- list()

for (i in 1:length(liste_outputs)) {
  fitting_liste_results[[i]] <- nls(formula = formula_phi_asymetric, # formula made
                               data = liste_outputs[[i]], # df for fit > ok for tibble
                               start = list(ps = 0.00001, pd = 0, alpha = -1), # starting model in
                               lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                               upper = list(ps = 1, pd = 1, alpha = 1), 
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
    pd_st_error = summaryparam[1,2],
    alpha = round(extractionparam[3],4),
    alpha_st_error = summaryparam[1,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results <-  output_fit_results %>%
    bind_rows(constructiontibble)
}

### ploting multiple fits
par(mfrow= c(3,3))

for (i in 1:length(fitting_liste_results)) {
  plot(liste_outputs[[i]]$N0_N, liste_outputs[[i]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F',
       main=output_fit_results[i,]$Country)
  lines(liste_outputs[[i]]$N0_N, predict(fitting_liste_results[[i]]), col='red')
}

write_csv(x = output_fit_results %>% 
            mutate_if(is.numeric, ~ round(.x, digits = 4)), file = "output_data/fit_phi_0T_all_cities.csv")

##### fit [0,T] : exponential distribution ####
#### all countries
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
    pd_st_error = summaryparam[1,2],
    ro = round(extractionparam[3],4),
    ro_N = round(extractionparam[3],4)/liste_outputs[[i]]$N[1],
    ro_st_error = summaryparam[1,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results <-  output_fit_results %>%
    bind_rows(constructiontibble)
}

### ploting multiple fits
par(mfrow= c(3,3))

for (i in 1:length(fitting_liste_results)) {
  plot(liste_outputs[[i]]$N0_N, liste_outputs[[i]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F',
       main=output_fit_results[i,]$Country)
  lines(liste_outputs[[i]]$N0_N, predict(fitting_liste_results[[i]]), col='red')
}

write_csv(x = output_fit_results %>% mutate_if(is.numeric, ~ round(.x, digits = 4)), 
          file = "output_data/fit_phi_0T_all_cities_exponential.csv")


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

f_result_tibble_de

#### ploting by times t and [0,T]
f_result_tibble_de %>% 
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
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\Phi$)"))

ggsave(filename = "figures/phi_DE.png", width = 15, height = 12, dpi = 300, units = 'cm')
ggsave(filename = "figures/phi_DE.pdf", width = 15, height = 12, dpi = 300, units = 'cm')

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
    pd_st_error = summaryparam[1,2],
    alpha = round(extractionparam[3],4),
    alpha_st_error = summaryparam[1,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_de <- output_fit_results_de %>%
    bind_rows(constructiontibble)
}

### ploting multiple fits
par(mfrow= c(2,3))

for (i in 1:length(fitting_liste_results)) {
  plot(liste_outputs_de[[i]]$N0_N, liste_outputs_de[[i]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'phi',
       main=output_fit_results_de[i,]$timet)
  lines(liste_outputs_de[[i]]$N0_N, predict(fitting_liste_results[[i]]), col='red')
}


### ploting: ps, pd and alpha
output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid + 1) %>%
  pivot_longer(cols = c(ps,pd, alpha), names_to = 'param', values_to = 'paramvalues') %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  geom_line() +
  # geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 1)
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.55),
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($t$)")) +
  ylab(TeX(r"($parameters$)"))

ggsave(filename = "figures/fit_param_DE.png", width = 13, height = 10, dpi = 300, units = 'cm')
ggsave(filename = "figures/fit_param_DE.pdf", width = 13, height = 10, dpi = 300, units = 'cm')

b <- output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, alpha), names_to = 'param', values_to = 'paramvalues') %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  geom_line() +
  # geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 1)
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.55),
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($t/T$)")) +
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
    pd_st_error = summaryparam[1,2],
    ro = round(extractionparam[3],4),
    ro_N = round(extractionparam[3],4)/liste_outputs_de[[i]]$N[1],
    ro_st_error = summaryparam[1,2],
    rss = round(fitting_liste_results[[i]]$m$deviance(), 5)
  )
  output_fit_results_de <- output_fit_results_de %>%
    bind_rows(constructiontibble)
}

### ploting: ps, pd and ro/N
output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid + 1) %>%
  pivot_longer(cols = c(ps,pd, ro_N), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(param = if_else(param == 'ro_N', 'ro/N', param)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  geom_line() +
  # geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 1)
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.90),
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($t$)")) +
  ylab(TeX(r"($parameters$)"))

ggsave(filename = "figures/fit_param_DE_exponential.png", width = 13, height = 10, dpi = 300, units = 'cm')
ggsave(filename = "figures/fit_param_DE_exponential.pdf", width = 13, height = 10, dpi = 300, units = 'cm')


#### DE all ploting ####
devisu <- f_result_tibble_de %>% 
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
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\Phi$)")) +
  labs(title = 'A')

c <- output_fit_results_de %>%
  rowid_to_column() %>%
  mutate(rowid = rowid - 1) %>%
  mutate(rowid = rowid/max(rowid)) %>%
  pivot_longer(cols = c(ps,pd, ro_N), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(param = if_else(param == 'ro_N', 'ro/N', param)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = param)) +
  geom_line() +
  # geom_pointrange(aes(ymin=mean-cv, ymax=mean+cv), fatten = 1)
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.90),
        legend.background = element_rect(fill = "white", color = "grey30")) +
  xlab(TeX(r"($t/T$)")) +
  ylab(TeX(r"($parameters$)")) +
  labs(title = 'C')

devisu + b + c
ggsave(filename = 'figures/DE_general_fit_timeseries.pdf', width = 30, height = 13, dpi = 300, units = 'cm')



#### all cities, all t ####
# list of t of list of countries
liste_ranking_countries <- list()
for (i in 1:length(liste_tible_result)) {
  liste_ranking_countries[[i]] <- liste_tible_result[[i]] %>%
    group_by(periods) %>%
    select(rowid) %>%
    group_split()
}

##### phi of t ####
### computing phi: see upper on DE
vector_of_ids_rank <- list() # create a list of list
phi_tibble <- tibble()

for (i in 1:length(liste_ranking_countries)) {
  # create vector of ranking of IDs of elements
  vector_of_ids_rank[[i]] <- list() # create a list of list
  for (k in 1:length(liste_ranking_countries[[i]])) {
    vector_of_ids_rank[[i]][[k]] <- liste_ranking_countries[[i]][[k]]$rowid
  }
  
  f_result_tibble <- tibble()
  # calculus of phi for each t
  for (j in 2:length(vector_of_ids_rank[[i]])) {
    intermediaryresult <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[i]][[k]]), 1),
                                            times = 2,
                                            list_entry = list(vector_of_ids_rank[[i]][[j-1]], vector_of_ids_rank[[i]][[j]]), 
                                            N = length(vector_of_ids_rank[[i]][[k]])) %>%
      mutate(N = length(vector_of_ids_rank[[i]][[k]])) %>%
      mutate(timet = paste0('t=[',j-1,',',j,']')) %>%
      mutate(Country = liste_tible_result[[i]]$Country[1])
    
    f_result_tibble <- f_result_tibble %>%
      bind_rows(intermediaryresult)
  }
  phi_tibble <- phi_tibble %>%
    bind_rows(f_result_tibble)
}

phi_tibble

#### ploting by times t by country
phi_tibble %>%
  ggplot(aes(x=N0_N, y=Fresult, color = timet)) +
  geom_line(linewidth=0.2) +
  ggthemes::scale_color_calc() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  xlab(TeX(r"($N_{0}/N$)")) +
  scale_x_continuous(labels = scales::comma) +
  ylab(TeX(r"($\Phi$)")) +
  facet_wrap(~Country, scales = 'free')

ggsave(filename = "figures/phi_cities_by_t.png", width = 20, height = 18, dpi = 300, units = 'cm')


##### fit phi: power law ####
liste_phi_all_t <- phi_tibble %>%
  group_by(Country) %>%
  group_split()
liste_for_fit_phi_all_t <- list()
liste_for_fit_phi_all_t[[i]] <- list()
for (i in 1:length(liste_phi_all_t)) {
  liste_for_fit_phi_all_t[[i]] <- liste_phi_all_t[[i]] %>%
    group_by(timet) %>%
    group_split()
}

# compute fit of phi for all t of all systems of cities
fitting_liste_results_cities_all_t <- list() # empty list
output_fit_results_cities_all_t <- tibble() # output tibble

for (i in 1:length(liste_for_fit_phi_all_t)) {
  
  fitting_liste_results_cities_all_t[[i]] <- list()
  for (j in 1:length(liste_for_fit_phi_all_t[[i]])) {
    fitting_liste_results_cities_all_t[[i]][[j]] <- nls(formula = formula_phi_asymetric, # formula made
                                                        data = liste_for_fit_phi_all_t[[i]][[j]], # df for fit > ok for tibble
                                                        start = list(ps = 0.00001, pd = 0, alpha = -1), # starting model in
                                                        lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                                                        upper = list(ps = 1, pd = 1, alpha = 1), 
                                                        algorithm = "port", nls.control(maxiter = 100000))
    
  }
  
  tibble_fit_results_cities_all_t <- tibble()
  for (j in 1:length(fitting_liste_results_cities_all_t[[i]])) {
    extractionparam <- fitting_liste_results_cities_all_t[[i]][[j]]$m$getAllPars()
    summaryparam <- summary(object = fitting_liste_results_cities_all_t[[i]][[j]])
    summaryparam <- summaryparam$parameters
    constructiontibble <- tibble(
      Country = liste_for_fit_phi_all_t[[i]][[j]]$Country[1],
      timet = liste_for_fit_phi_all_t[[i]][[j]]$timet[1],
      ps = extractionparam[1],
      ps_st_error = summaryparam[1,2],
      pd = extractionparam[2],
      pd_st_error = summaryparam[1,2],
      alpha = round(extractionparam[3],4),
      alpha_st_error = summaryparam[1,2],
      rss = fitting_liste_results_cities_all_t[[i]][[j]]$m$deviance()
    )
    tibble_fit_results_cities_all_t <- tibble_fit_results_cities_all_t %>%
      bind_rows(constructiontibble)
  }
  
  output_fit_results_cities_all_t <- output_fit_results_cities_all_t %>%
    bind_rows(tibble_fit_results_cities_all_t)
}

write_csv(x = output_fit_results_cities_all_t, file = "output_data/fit_phi_all_times_t_all_cities.csv")

###### plotting multiple fits ####
# by hand
par(mfrow= c(2,3))
for (p in 1:length(fitting_liste_results_cities_all_t[[9]])) {
  plot(liste_for_fit_phi_all_t[[9]][[p]]$N0_N, liste_for_fit_phi_all_t[[9]][[p]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'phi',
       main=output_fit_results_de[p,]$timet)
  lines(liste_for_fit_phi_all_t[[9]][[p]]$N0_N, predict(fitting_liste_results_cities_all_t[[9]][[p]]), col='red')
}


###### plot: ps, pd and alpha ####
output_fit_results_cities_all_t %>%
  group_by(Country) %>%
  mutate(rowid = row_number()) %>%
  mutate(rowid = rowid + 1) %>%
  pivot_longer(cols = c(ps, pd, alpha), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(reorderingvisu = case_when(param == 'ps' ~ 1, param == 'pd' ~ 2, TRUE ~ 3)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = Country)) +
  geom_line(linewidth=0.3) +
  geom_point(size=0.7, show.legend = FALSE) +
  ggthemes::scale_color_tableau() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  xlab(TeX(r"($t$)")) +
  ylab(TeX(r"($parameters$)")) +
  facet_wrap(~reorder(param, reorderingvisu), scales = 'free') + 
  guides(colour = guide_legend(nrow = 1))

ggsave(filename = "figures/fit_param_all_countries_all_t.png", width = 20, height = 12, dpi = 300, units = 'cm')


##### fit phi: exponential ####
liste_phi_all_t <- phi_tibble %>%
  group_by(Country) %>%
  group_split()
liste_for_fit_phi_all_t <- list()
liste_for_fit_phi_all_t[[i]] <- list()
for (i in 1:length(liste_phi_all_t)) {
  liste_for_fit_phi_all_t[[i]] <- liste_phi_all_t[[i]] %>%
    group_by(timet) %>%
    group_split()
}

# compute fit of phi for all t of all systems of cities
fitting_liste_results_cities_all_t <- list() # empty list
output_fit_results_cities_all_t <- tibble() # output tibble

for (i in 1:length(liste_for_fit_phi_all_t)) {
  
  fitting_liste_results_cities_all_t[[i]] <- list()
  for (j in 1:length(liste_for_fit_phi_all_t[[i]])) {
    fitting_liste_results_cities_all_t[[i]][[j]] <- nls(formula = formula_phi_asymetric_exponential, # formula made
                                                        data = liste_for_fit_phi_all_t[[i]][[j]], # df for fit > ok for tibble
                                                        start = list(ps = 0.00001, pd = 0, ro = 0.1), # starting model in
                                                        lower = list(ps = 0, pd = 0, ro = 0.0000000000001),
                                                        upper = list(ps = 1, pd = 1, ro = 100000000000000), 
                                                        algorithm = "port", nls.control(maxiter = 100000))
    
  }
  
  tibble_fit_results_cities_all_t <- tibble()
  for (j in 1:length(fitting_liste_results_cities_all_t[[i]])) {
    extractionparam <- fitting_liste_results_cities_all_t[[i]][[j]]$m$getAllPars()
    summaryparam <- summary(object = fitting_liste_results_cities_all_t[[i]][[j]])
    summaryparam <- summaryparam$parameters
    constructiontibble <- tibble(
      Country = liste_for_fit_phi_all_t[[i]][[j]]$Country[1],
      timet = liste_for_fit_phi_all_t[[i]][[j]]$timet[1],
      ps = extractionparam[1],
      ps_st_error = summaryparam[1,2],
      pd = extractionparam[2],
      pd_st_error = summaryparam[1,2],
      ro = round(extractionparam[3],4),
      ro_N = round(extractionparam[3],4)/liste_for_fit_phi_all_t[[i]][[j]]$N[1],
      ro_st_error = summaryparam[1,2],
      rss = fitting_liste_results_cities_all_t[[i]][[j]]$m$deviance()
    )
    tibble_fit_results_cities_all_t <- tibble_fit_results_cities_all_t %>%
      bind_rows(constructiontibble)
  }
  
  output_fit_results_cities_all_t <- output_fit_results_cities_all_t %>%
    bind_rows(tibble_fit_results_cities_all_t)
}

write_csv(x = output_fit_results_cities_all_t, file = "output_data/fit_phi_all_times_t_all_cities_exponential.csv")

##### plotting multiple fits ####
# by hand
par(mfrow= c(2,3))
for (p in 1:length(fitting_liste_results_cities_all_t[[9]])) {
  plot(liste_for_fit_phi_all_t[[9]][[p]]$N0_N, liste_for_fit_phi_all_t[[9]][[p]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'phi',
       main=output_fit_results_de[p,]$timet)
  lines(liste_for_fit_phi_all_t[[9]][[p]]$N0_N, predict(fitting_liste_results_cities_all_t[[9]][[p]]), col='red')
}


##### plot: ps, pd and ro/N ####
output_fit_results_cities_all_t %>%
  group_by(Country) %>%
  mutate(rowid = row_number()) %>%
  mutate(rowid = rowid + 1) %>%
  pivot_longer(cols = c(ps, pd, ro_N), names_to = 'param', values_to = 'paramvalues') %>%
  mutate(param = if_else(param == 'ro_N', 'r0/N', param)) %>%
  mutate(reorderingvisu = case_when(param == 'ps' ~ 1, param == 'pd' ~ 2, TRUE ~ 3)) %>%
  ggplot(aes(x=rowid, y = paramvalues, color = Country)) +
  geom_line(linewidth=0.3) +
  geom_point(size=0.7, show.legend = FALSE) +
  ggthemes::scale_color_tableau() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  xlab(TeX(r"($t$)")) +
  ylab(TeX(r"($parameters$)")) +
  facet_wrap(~reorder(param, reorderingvisu), scales = 'free') + 
  guides(colour = guide_legend(nrow = 1))

ggsave(filename = "figures/fit_exp_param_all_countries_all_t.png", width = 20, height = 12, dpi = 300, units = 'cm')

#### Shanghai data ####
shangai <- read.csv2(file = "data/shanghai-world-university-ranking.csv") %>%
  as_tibble()

shangai_ranked <- shangai %>%
  # need renaming
  mutate(University = case_when(
    University %in% c("Texas A & M University", "Texas A&M University - College Station") ~ "Texas A&M University",
    University %in% c("Rutgers, The State University of New Jersey - New Brunswick") ~ "Rutgers, The State University of New Jersey",
    University %in% c("Pierre and Marie  Curie University - Paris 6") ~ "Pierre and Marie Curie University - Paris 6",
    University %in% c("University of California, Berkeley") ~ "University of California-Berkeley",
    University %in% c("University of Michigan - Ann Arbor") ~ "University of Michigan-Ann Arbor",
    University %in% c("University of Paris Sud (Paris 11)") ~ "University of Paris-Sud (Paris 11)",
    University %in% c("University of Pittsburgh, Pittsburgh Campus", "University of Pittsburgh-Pittsburgh Campus") ~ "University of Pittsburgh",
    University %in% c("Arizona State University - Tempe") ~ "Arizona State University",
    TRUE ~ University
  )) %>%
  mutate(rank = as.numeric(World.rank)) %>%
  filter(rank <= 100) %>%
  select(University, Year, rank) %>%
  pivot_wider(names_from = Year, values_from = rank) %>%
  rowid_to_column()

shangai_ranked <- shangai_ranked %>%
  pivot_longer(cols = `2012`:`2010`, names_to = "year", values_to = "rank") %>%
  arrange(rowid, year) %>%
  # if is NA <=> more than 100, remove
  filter(!is.na(rank))


##### calculus of F ####
# diverse N0/N
liste_tible_result <- shangai_ranked %>% 
  arrange(year, rank) %>%
  group_by(year) %>% 
  select(rowid) %>% 
  group_split()

# create vector of ranking of IDs of elements (here universities)
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}


# calculus of f
f_result_tibble_shanghai <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                            times = length(vector_of_ids_rank), 
                                            list_entry = vector_of_ids_rank, N = 2500) %>%
    mutate(N = 2500)

f_result_tibble_shanghai <- f_result_tibble_shanghai %>%
  mutate(Fresult = Fsum/sum(Fsum))

##### viz ####
f_result_tibble_shanghai %>%
  ggplot(aes(x=N0_N, y=Fresult)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  labs(title = "Universities")

ggsave(filename = "figures/F_shanghai.png", width = 15, height = 12, dpi = 300, units = 'cm')


##### fit ####
output <- f_result_tibble_shanghai
# manual observation of potential fit
par(mfrow=c(1,1))
preview(formula_phi_asymetric, data = output, 
        start = list(ps = 0.1, pd = 0, alpha=-1))

# fitting analysis with nls
fitting_results_shanghai <- nls(formula = formula_phi_asymetric, # formula made
                       data = output, # df for fit > ok for tibble
                       start = list(ps = 0.0001, pd = 0, alpha = -1), # starting model in
                       lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                       upper = list(ps = 1, pd = 1, alpha = 1), 
                       algorithm = "port", nls.control(maxiter = 10000))

fitting_results
summary(fitting_results) # pd error is big with small T ; less in large T

# visualization
plot(output$N0_N, output$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F')
lines(output$N0_N, predict(fitting_results), col='red')


#### Chessplayers FIDE ####
chess_yearly <- read_parquet(file = "data/FIDE_standard_compilations_Dahiya.parquet", 
                             as_data_frame = TRUE) %>%
  as_tibble()

chess_yearly <- chess_yearly %>%
  mutate(year = str_sub(string = Date, start = 1, end = 4),
         month = str_sub(string = Date, start = 6, end = 7))

chess_yearly <- chess_yearly %>%
  filter(month %in% "01") # 1 times a year [, '04', '07', '10']


#### note: N is very diverse through time (x10)
npoblem <- chess_yearly %>% 
  select(year, month, ID_Number) %>% 
  group_by(year, month) %>% 
  summarise(n=n())

##### for diff t: compute entries #####
# data preparation
sequence_de_n <- seq(1000, 29000, 1000)

# for 3 different t
tibble_diff_n <- tibble()

for (i in 1:length(sequence_de_n)) {
  
  chess_yearly_select <- chess_yearly %>%
    select(ID_Number, ranking, year, month) %>%
    rename(rank = ranking, rowid = ID_Number) %>%
    filter(rank < sequence_de_n[i]) %>%
    arrange(rowid, year)
  
  liste_tible_result <- chess_yearly_select %>% 
    arrange(year, month, rank) %>%
    group_by(year, month) %>% 
    select(rowid) %>%
    group_split()
  
  # create vector of ranking of IDs of elements (here universities)
  vector_of_ids_rank <- list()
  
  for (k in 1:length(liste_tible_result)) {
    vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
  }
  
  n_tibble_t <- tibble()
  
  for (v in 2:length(vector_of_ids_rank)) {
    vecdiff <- vector_of_ids_rank[[v-1]] %in% vector_of_ids_rank[[v]]
    ntot <- length(vecdiff[vecdiff == FALSE])
    n_tibble_t <- n_tibble_t %>%
      bind_rows(
        tibble(n_entrant = ntot,
               freq_entrant = ntot/sequence_de_n[i],
               timet = paste0('t=[',v-1,',',v,']'),
               orderingt = v,
               size_n = sequence_de_n[i])
      )
  }
  
  tibble_diff_n <- tibble_diff_n %>%
    bind_rows(n_tibble_t)
}

tibble_diff_n %>%
  ggplot(aes(x = size_n, y = freq_entrant, color = reorder(timet, orderingt))) +
  geom_line() +
  geom_point(size=0.4) +
  theme_bw() +
  ylab('Frequency of new players') +
  xlab('Size of list') +
  theme(legend.title = element_blank())

ggsave(filename = "figures/chessplayers_freq_new_players.png", width = 18, height = 15, dpi = 300, units = 'cm')


### en virant 2001, 2002 et 2003
# data preparation
sequence_de_n <- seq(1000, 50000, 1000)

# for 3 different t
tibble_diff_n <- tibble()

for (i in 1:length(sequence_de_n)) {
  
  chess_yearly_select <- chess_yearly %>%
    filter(year != '2001' & year != '2002' & year != '2003') %>%
    select(ID_Number, ranking, year, month) %>%
    rename(rank = ranking, rowid = ID_Number) %>%
    filter(rank < sequence_de_n[i]) %>%
    arrange(rowid, year)
  
  liste_tible_result <- chess_yearly_select %>% 
    arrange(year, month, rank) %>%
    group_by(year, month) %>% 
    select(rowid) %>%
    group_split()
  
  # create vector of ranking of IDs of elements (here universities)
  vector_of_ids_rank <- list()
  
  for (k in 1:length(liste_tible_result)) {
    vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
  }
  
  n_tibble_t <- tibble()
  
  for (v in 2:length(vector_of_ids_rank)) {
    vecdiff <- vector_of_ids_rank[[v-1]] %in% vector_of_ids_rank[[v]]
    ntot <- length(vecdiff[vecdiff == FALSE])
    n_tibble_t <- n_tibble_t %>%
      bind_rows(
        tibble(n_entrant = ntot,
               freq_entrant = ntot/sequence_de_n[i],
               timet = paste0('t=[',v-1,',',v,']'),
               orderingt = v,
               size_n = sequence_de_n[i])
      )
  }
  
  tibble_diff_n <- tibble_diff_n %>%
    bind_rows(n_tibble_t)
}

tibble_diff_n %>%
  ggplot(aes(x = size_n, y = freq_entrant, color = reorder(timet, orderingt))) +
  geom_line(linewidth=0.3) +
  geom_point(size=0.2) +
  theme_bw() +
  ylab('Frequency of new players') +
  xlab('Size of list') +
  theme(legend.title = element_blank()) +
  labs(subtitle = '2004-2019')

ggsave(filename = "figures/chessplayers_freq_new_players_2004_2019.png", width = 18, height = 15, dpi = 300, units = 'cm')


##### phi for all t ####
chess_yearly_select <- chess_yearly %>%
  filter(year != '2001' & year != '2002' & year != '2003') %>%
  select(ID_Number, ranking, year, month) %>%
  rename(rank = ranking, rowid = ID_Number) %>%
  filter(rank < 25001) %>% # choice from data observation
  arrange(rowid, year)

liste_tible_result <- chess_yearly_select %>% 
  arrange(year, month, rank) %>%
  group_by(year, month) %>% 
  select(rowid) %>%
  group_split()

# create vector of ranking of IDs of elements (here universities)
vector_of_ids_rank <- list()

for (k in 1:length(liste_tible_result)) {
  vector_of_ids_rank[[k]] <- liste_tible_result[[k]]$rowid
}

###### removing presents #####
vector_of_ids_rank_remove <- list()
vecdiff <- vector_of_ids_rank[[2-1]] %in% vector_of_ids_rank[[2]]
vector_of_ids_rank_remove[[1]] <- vector_of_ids_rank[[1]][vecdiff == TRUE]

for (v in 2:length(vector_of_ids_rank)) {
  vecdiff <- vector_of_ids_rank[[v-1]] %in% vector_of_ids_rank[[v]]
  vector_of_ids_rank_remove[[v]] <- vector_of_ids_rank[[v]][vecdiff == TRUE]
}

###### calculus of phi for each t #####
f_chessplayers <- tibble()

for (i in 2:length(vector_of_ids_rank_remove)) {
  Nmax <- npoblem[4:19]
  Nmax <- Nmax[(i-1):i]
  Nmax <- max(Nmax[1,3], Nmax[2,3])
  # Nmax <- 250000
  minimallength <- min(length(vector_of_ids_rank_remove[[i-1]]),length(vector_of_ids_rank_remove[[i]]))
  intermediaryresult <- f_calculus_F_data(vector_entry = seq(1, minimallength, 1),
                                          times = 2,
                                          list_entry = list(vector_of_ids_rank_remove[[i-1]], vector_of_ids_rank_remove[[i]]), 
                                          N = Nmax) %>%
    mutate(N = Nmax) %>%
    mutate(timet = paste0('t=[',i-1,',',i,']'), 
           orderingt = i)
  
  f_chessplayers <- f_chessplayers %>%
    bind_rows(intermediaryresult)
  print(paste0('end of computing of ', 't=[',i-1,',',i,']'))
}

f_chessplayers


#### ploting by times t and [0,T]
f_chessplayers %>%
  filter(N0_N <= 1) %>%
  ggplot(aes(x=N0_N, y=Fresult, color = timet)) +
  geom_line(linewidth=0.2) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($\Phi$)"))

ggsave(filename = "figures/phi_chessplayers_removing_25000.png", width = 20, height = 18, dpi = 300, units = 'cm')


##### fit phi: power law ####
output <- f_result_tibble_FIDE
# manual observation of potential fit
par(mfrow=c(1,1))
preview(formula_phi_asymetric, data = output, 
        start = list(ps = 0.0001, pd = 0, alpha=-1))

# fitting analysis with nls
fitting_results_FIDE <- nls(formula = formula_phi_asymetric, # formula made
                       data = output, # df for fit > ok for tibble
                       start = list(ps = 0.0001, pd = 0, alpha = -1), # starting model in
                       lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                       upper = list(ps = 1, pd = 1, alpha = 1), 
                       algorithm = "port", nls.control(maxiter = 10000))

fitting_results_FIDE
summary(fitting_results_FIDE)

# visualization
plot(output$N0_N, output$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F')
lines(output$N0_N, predict(fitting_results), col='red')


### ploting multiple fits
par(mfrow= c(3,4))

fitting_liste_results[[10]] <- fitting_results_shanghai
fitting_liste_results[[11]] <- fitting_results_FIDE

liste_outputs[[10]] <- f_result_tibble_shanghai %>%
  mutate(Country = 'University') %>%
  relocate(Country, .before = Fresult)

liste_outputs[[11]] <- f_result_tibble_shanghai %>%
  mutate(Country = 'University') %>%
  relocate(Country, .before = Fresult)

for (i in 1:length(fitting_liste_results)) {
  plot(liste_outputs[[i]]$N0_N, liste_outputs[[i]]$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F',
       main=output_fit_results[i,]$Country)
  lines(liste_outputs[[i]]$N0_N, predict(fitting_liste_results[[i]]), col='red')
}



