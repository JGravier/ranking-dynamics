library(tidyverse)
library(arrow)
library(readxl)

#### European cities ####
tradeve <- read_excel(path = "data/TRADEVE_UrbanAreas_Data.xlsx", sheet = "UrbanAreas_Data")

# creation of ranking by countries
dk_countries_tradeve <- tradeve %>%
  group_by(Country) %>%
  mutate(rang_1961 = rank(desc(Pop_1961)),
         rang_1971 = rank(desc(Pop_1971)),
         rang_1981 = rank(desc(Pop_1981)),
         rang_1991 = rank(desc(Pop_1991)),
         rang_2001 = rank(desc(Pop_2001)),
         rang_2011 = rank(desc(Pop_2011))) %>%
  select(-Event, -Area_1961:-Area_2011)

# list of countries studied
liste_pays <- c("DE", "CZ", "ES", "FR", "UK", "IT", "NL", "PL", "RO")

dk_countries_tradeve <- dk_countries_tradeve %>%
  filter(Country %in% liste_pays) %>%
  select(Country, Name, rang_1961:rang_2011) %>%
  mutate(rowid = row_number()) %>%
  pivot_longer(cols = rang_1961:rang_2011, names_to = "periods", values_to = "rank") %>%
  arrange(rank, Country, periods)

write_csv(x = dk_countries_tradeve, file = 'data/data-reorganized/european_cities.csv')

### US Companies ####
companies <- read_csv(file = "data/fortune500_1955_2019.csv") %>%
  filter(date < 2006)

rowidcompany <- companies %>%
  select(company) %>%
  unique() %>%
  rowid_to_column()

companies_ranked <- companies %>%
  select(-revenue) %>%
  left_join(rowidcompany, by = 'company') %>%
  arrange(rowid, date)

write_csv(x = companies_ranked, file = 'data/data-reorganized/us-companies.csv')

#### Shanghai universities ####
shanghai <- read.csv2(file = "data/shanghai-world-university-ranking.csv") %>%
  as_tibble()

shanghai_ranked <- shanghai %>%
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

shanghai_ranked <- shanghai_ranked %>%
  pivot_longer(cols = `2012`:`2010`, names_to = "year", values_to = "rank") %>%
  arrange(rowid, year) %>%
  # if is NA <=> more than 100, remove
  filter(!is.na(rank))

write_csv(x = shanghai_ranked, file = 'data/data-reorganized/shanghai-universities.csv')

#### Chess players: FIDE, 2004-2019, January #####
chess_yearly <- read_parquet(file = "data/FIDE_standard_compilations_Dahiya.parquet", 
                             as_data_frame = TRUE) %>%
  as_tibble()

chess_yearly <- chess_yearly %>%
  mutate(year = str_sub(string = Date, start = 1, end = 4),
         month = str_sub(string = Date, start = 6, end = 7))

chess_yearly <- chess_yearly %>%
  filter(month %in% c('01')) # 1 times a year [, '04', '07', '10']

chess_yearly <- chess_yearly %>%
  filter(year != '2001' & year != '2002' & year != '2003') %>%
  select(ID_Number, Name, ranking, year, month) %>%
  rename(rank = ranking, rowid = ID_Number) %>%
  arrange(rowid, year)

write_csv(x = chess_yearly, file = 'data/data-reorganized/chessplayers-FIDE-2004-2019.csv')

#### Chess players: chessmetrics, 2004-2019, January #####
chess_yearly <- read_csv(file = 'data/ranking_chessplayers_1851_2000_yearly.csv')

chess_yearly <- chess_yearly %>%
  filter(year > 1965)

chess_yearly <- chess_yearly %>%
  mutate(Player = case_when(
    str_detect(Player, 'Granda Zu') ~ 'Granda Zuniga, Julio E',
    str_detect(Player, 'Hjartarson, J') ~ 'Hjartarson, Johann',
    str_detect(Player, 'Ribli, Zolt') ~ 'Ribli, Zoltán',
    str_detect(Player, 'Silva S') ~ 'Silva Sanchez, Carlos',
    str_detect(Player, 'Szab') ~ 'Szabo, Laszlo',
    str_detect(Player, 'Sanguinetti, Ra') ~ 'Sanguinetti, Raul C',
    TRUE ~ Player
  ))

rowid <- chess_yearly %>%
  select(Player) %>%
  unique() %>%
  rowid_to_column()

chess_yearly <- chess_yearly %>%
  left_join(y = rowid, by = 'Player') %>%
  arrange(rowid, year)

write_csv(x = chess_yearly, file = 'data/data-reorganized/chessplayers-chessmetrics-1966-2000.csv')

#### World Football Elo Rating #####
soccer_elo <- read_csv(file = 'data/ranking_soccer_1901-2023.csv')

soccer_elo %>%
  group_by(year) %>%
  count() %>%
  view()

rowid <- soccer_elo %>%
  select(team) %>%
  unique() %>%
  rowid_to_column()

soccer_elo <- soccer_elo %>%
  filter(year > 1953) %>% # at least 135 teams
  left_join(y = rowid, by = 'team') %>%
  arrange(rowid, year)
  
write_csv(x = soccer_elo, file = 'data/data-reorganized/soccer-1954-2023.csv')



