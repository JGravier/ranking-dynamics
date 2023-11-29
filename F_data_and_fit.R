library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(latex2exp)
library(arrow)
library(readxl)
library(patchwork)
library(nlstools) # for fit

# functions
source(file = "functions.R")

# formula of fit
formula_F_asymetric <- as.formula(Fresult ~ ps * ( (N0_N^(1-alpha)) * (1-N0_N) + ( (1-N0_N)^(1+alpha) ) * N0_N ) +
                                    (1 - ps) * pd * N0_N^(-alpha) * (1/N) )


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

##### calculus of F ####

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


# calculus of f
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

liste_f_countries <- liste_f_countries %>%
  group_by(Country) %>%
  mutate(Fresult = Fsum/sum(Fsum))

##### viz ####
liste_f_countries %>%
  ggplot(aes(x=N0_N, y=Fresult, color = Country)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  labs(title = "Cities")

ggsave(filename = "figures/F_cities.png", width = 15, height = 12, dpi = 300, units = 'cm')


##### fit ####
# test d'exemple avec l'Espagne
output <- liste_f_countries %>% filter(Country == 'ES')
# manual observation of potential fit
preview(formula_F_asymetric, data = output, 
        start = list(ps = 0.1, pd = 0, alpha=-0.3))

# fitting analysis with nls
fitting_results <- nls(formula = formula_F_asymetric, # formula made
                       data = output, # df for fit > ok for tibble
                       start = list(ps = 0.01, pd = 0, alpha = -1), # starting model in
                       lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                       upper = list(ps = 1, pd = 1, alpha = 1), algorithm = "port", nls.control(maxiter = 10000))

fitting_results
summary(fitting_results) # pd error is big with small T ; less in large T

# visualization
plot(output$N0_N, output$Fresult, pch=19, cex=0.5, xlab='p=N0/N', ylab = 'F')
lines(output$N0_N, predict(fitting_results), col='red')


#### avec tous les pays
liste_outputs <- liste_f_countries %>%
  group_by(Country) %>%
  group_split()

fitting_liste_results <- list()

for (i in 1:length(liste_outputs)) {
  fitting_liste_results[[i]] <- nls(formula = formula_F_asymetric, # formula made
                               data = liste_outputs[[i]], # df for fit > ok for tibble
                               start = list(ps = 0.00001, pd = 0, alpha = -1), # starting model in
                               lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                               upper = list(ps = 1, pd = 1, alpha = 1), 
                               algorithm = "port", nls.control(maxiter = 10000))

}

output_fit_results <- tibble()

for (i in 1:length(fitting_liste_results)) {
  extractionparam <- fitting_liste_results[[i]]$m$getAllPars()
  constructiontibble <- tibble(
    Country = liste_outputs[[i]]$Country[1],
    ps = round(extractionparam[1], 4),
    pd = round(extractionparam[2], 4),
    alpha = round(extractionparam[3],4),
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


#### shanghai data ####
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
preview(formula_F_asymetric, data = output, 
        start = list(ps = 0.1, pd = 0, alpha=-1))

# fitting analysis with nls
fitting_results <- nls(formula = formula_F_asymetric, # formula made
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
  filter(month %in% c("01", '04', '07', '10')) # 4 times a year


#### note: N is very diverse through time (x10)
chess_yearly %>% 
  select(year, month, ID_Number) %>% 
  group_by(year, month) %>% 
  summarise(n=n()) %>%
  view()

##### top 5000 2001-2019 ####
chess_yearly_select <- chess_yearly %>%
  select(ID_Number, ranking, year, month) %>%
  rename(rank = ranking, rowid = ID_Number) %>%
  filter(rank < 10001) %>%
  arrange(rowid, year)

##### calculus of F ####
# diverse N0/N
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


# calculus of f
f_result_tibble_FIDE <- f_calculus_F_data(vector_entry = seq(1, length(vector_of_ids_rank[[k]]), 1), 
                                              times = length(vector_of_ids_rank), 
                                              list_entry = vector_of_ids_rank, N = 29273) %>%
  mutate(N = 29273)

f_result_tibble_FIDE <- f_result_tibble_FIDE %>%
  mutate(Fresult=Fsum/sum(Fsum))

##### viz ####
f_result_tibble_FIDE %>%
  ggplot(aes(x=N0_N, y=Fresult)) +
  geom_line() +
  ggthemes::scale_color_tableau(palette = 'Tableau 10') +
  theme_bw() +
  xlab(TeX(r"($N_{0}/N$)")) +
  ylab(TeX(r"($F$)")) +
  labs(title = "Chessplayers")

ggsave(filename = "figures/F_chessplayers.png", width = 15, height = 12, dpi = 300, units = 'cm')

##### fit ####
output <- f_result_tibble_FIDE
# manual observation of potential fit
par(mfrow=c(1,1))
preview(formula_F_asymetric, data = output, 
        start = list(ps = 0.0001, pd = 0, alpha=-1))

# fitting analysis with nls
fitting_results <- nls(formula = formula_F_asymetric, # formula made
                       data = output, # df for fit > ok for tibble
                       start = list(ps = 0.0001, pd = 0, alpha = -1), # starting model in
                       lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                       upper = list(ps = 1, pd = 1, alpha = 1), 
                       algorithm = "port", nls.control(maxiter = 10000))

fitting_results
summary(fitting_results)

# visualization
plot(output$N0_N, output$Fresult, pch=19, cex=0.5, xlab='N0/N', ylab = 'F')
lines(output$N0_N, predict(fitting_results), col='red')






