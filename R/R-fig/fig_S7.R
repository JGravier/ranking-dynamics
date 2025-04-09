library(tidyverse)
library(latex2exp)
library(patchwork)

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

#### Visualization of overlapping indices ####
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

a <- overlapping_all %>%
  group_by(system) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t) %>%
  ggplot(mapping = aes(x = time_t_minus_on_t, y = overlapping_index, color = system)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(legend.position = "left") +
  scale_x_continuous(name = TeX(r"($t/T$)")) +
  scale_y_continuous(name = TeX(r"($O$)"), limits = c(0,1), breaks = seq(0,1,0.2))

### Filtering soccer and companies ####
# companies: from 1994, when 1994 = T
# list of tibbles
liste_tible_result <- companies_ranked %>% 
  filter(date < 1995) %>%
  arrange(date, rank) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

# overlapping companies
o_companies_b_1995 <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)

# soccer: from 1991, when 1991 = T
# list of tibbles
liste_tible_result <- soccer %>% 
  arrange(year, rank) %>%
  rename(date = year) %>%
  filter(date < 1992) %>%
  group_by(date) %>% 
  select(rowid) %>% 
  group_split()

o_soccer_b_1992 <- f_overlapping(study_list = liste_tible_result, Nzero = FALSE)

overlapping_all <- o_companies_b_1995 %>%
  mutate(system = "companies b. 1995") %>%
  bind_rows(
    o_soccer_b_1992 %>%
      mutate(system = "soccer teams b. 1992")
  )

# vizu
b <- overlapping_all %>%
  group_by(system) %>%
  mutate(max_t = max(time_t_minus)) %>%
  mutate(time_t_minus_on_t = time_t_minus/max_t) %>%
  ggplot(mapping = aes(x = time_t_minus_on_t, y = overlapping_index, 
                       color = system)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  theme_bw() +
  scale_x_continuous(name = TeX(r"($t/T$)")) +
  scale_y_continuous(name = "", limits = c(0,1), breaks = seq(0,1,0.2)) +
  scale_color_manual(values = c("#00BF7D", "#00B0F6"))

a + b + plot_annotation(tag_levels = 'A')

ggsave(filename = "figures/overlapping_open_systems.pdf", 
       width = 25, height = 12, units = "cm", dpi = 300)



