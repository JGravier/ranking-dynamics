library(tidyverse)
library(data.table)
library(tidyfast)
library(tidytable)
library(latex2exp)
library(nlstools) # for fit

source("functions.R")
set.seed(2123)
#### ranking model: numeric VS theoretical ####
init_size <- seq(1, 200, 1)
number_times <- seq(1, 500, 1)

### theoretical model
theoreticalmodel <- 2*init_size/length(init_size)*(1-init_size/length(init_size))
# for plot
theoreticalmodel <- theoreticalmodel %>% 
  as_tibble() %>% 
  rename(Fresult = value) %>%
  mutate(N0_N = init_size/length(init_size), model="2p(1-p)") %>%
  relocate(N0_N, .before = Fresult)

##### ps = 1 #####
base_model_f <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 1, pd = 0, alpha = 0)

# calculs of F:
output <- f_calculus_F(vector_entry = init_size, list_entry = base_model_f, N = length(init_size))

# plot
output %>%
  mutate(model = 'ps=1, N=1000, T=500') %>%
  bind_rows(theoreticalmodel) %>%
  ggplot(aes(N0_N, Fresult, color = model)) +
  geom_line() +
  xlab(TeX('$N_0/N$')) +
  ylab(TeX(input = '$F$')) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.9), legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.background = element_rect(colour = 'black', linewidth = 0.1))

ggsave(filename = "figures/ranking_model_base.pdf", width = 14, height = 12, dpi = 400, units = "cm")


##### with various alpha #####
# basic elements
init_size <- seq(1, 200, 1)
number_times <- seq(1, 500, 1)

# alpha=+0.5 ; 0 and -0.5
# ps=0.1 and pd=0.4
base_model_f <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = -0.5)
output <- f_calculus_F(vector_entry = init_size, list_entry = base_model_f)

bmf2 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0)
output2 <- f_calculus_F(vector_entry = init_size, list_entry = bmf2)

bmf3 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0.5)
output3 <- f_calculus_F(vector_entry = init_size, list_entry = bmf3)

# "\U03B1" is unicode for alpha
# plot
combine_output <- output %>%
  mutate(model = "\U03B1=-0.5") %>%
  bind_rows(output2 %>% mutate(model = "\U03B1=0")) %>%
  bind_rows(output3 %>% mutate(model = "\U03B1=0.5"))

combine_output %>%
  ggplot(aes(N0_N, Fresult, color = model)) +
  geom_line() +
  theme_bw() +
  xlab(TeX('$N_0/N$')) +
  ylab(TeX(input = '$F$')) +
  theme(legend.position = c(0.9, 0.9), legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.background = element_rect(colour = 'black', linewidth = 0.1))

ggsave(filename = "figures/ranking_model_alpha.pdf", width = 14, height = 12, dpi = 400, units = "cm")

#### ...and large T ####
# basic elements
init_size <- seq(1, 200, 1)
number_times <- seq(1, 10000, 1)

# personnal
round(x = 0.1*length(number_times), digits = 0) + round(x = (1-0.1)*0.4*length(number_times), digits = 0)

# alpha=+0.5 ; 0 and -0.5
# ps=0.1 and pd=0.4
base_model_f <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = -0.5)
output <- f_calculus_F(vector_entry = init_size, list_entry = base_model_f)

bmf2 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0)
output2 <- f_calculus_F(vector_entry = init_size, list_entry = bmf2)

bmf3 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0.5)
output3 <- f_calculus_F(vector_entry = init_size, list_entry = bmf3)

# "\U03B1" is unicode for alpha
# plot
combine_output <- output %>%
  mutate(model = "\U03B1=-0.5") %>%
  bind_rows(output2 %>% mutate(model = "\U03B1=0")) %>%
  bind_rows(output3 %>% mutate(model = "\U03B1=0.5"))

combine_output %>%
  ggplot(aes(N0_N, Fresult, color = model)) +
  geom_line() +
  theme_bw() +
  xlab(TeX('$N_0/N$')) +
  ylab(TeX(input = '$F$')) +
  theme(legend.position = c(0.9, 0.9), legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.background = element_rect(colour = 'black', linewidth = 0.1))

ggsave(filename = "figures/ranking_model_alpha.png", width = 14, height = 12, dpi = 400, units = "cm")
ggsave(filename = "figures/ranking_model_alpha.pdf", width = 14, height = 12, dpi = 400, units = "cm")

#### fit with alpha = -0.5 ####
output <- output %>%
  mutate(N = length(init_size))

# write output
write_csv(x = output, file = "output_data/model_alpha_0.5_T_10000.csv")

# manual observation of potential fit
preview(formula_phi_asymetric, data = output, start = list(ps = 0.1, pd = 0, alpha=-0.3))

# fitting analysis with nls
fitting_results <- nls(formula = formula_phi_asymetric, # formula made
                       data = output, # df for fit > ok for tibble
                       start = list(ps = 0.01, pd = 0, alpha = -1), # starting model in
                       lower = list(ps = 0, pd = 0, alpha = -10000000000000),
                       upper = list(ps = 1, pd = 1, alpha = 1), algorithm = "port", nls.control(maxiter = 10000))

fitting_results
summary(fitting_results) # pd error is big with small T ; less in large T

# visualization
plot(output$N0_N, output$Fresult, pch=19, cex=0.5, xlab='p=N0/N', ylab = 'F')
lines(output$N0_N, predict(fitting_results), col='red')

#### ranking model Ft ####
init_size <- seq(1, 200, 1)
number_times <- seq(1, 500, 1)

base_model_f <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 1, pd = 0, alpha = 0)
output <- f_calculus_Ft(vector_entry = init_size, times = length(number_times), list_entry = base_model_f, N = length(init_size))

formula_F_no_alpha <- as.formula(Ft ~ (ps*(1 - N0_N)) + pd*(1 - ps)*(1/N0))
# en moyenne
outputfresult <- output %>%
  group_by(N0_N) %>%
  summarise(Ft = sum(Ft)/(500-1))
# preview fit
preview(formula_F_no_alpha, data = outputfresult %>% mutate(N0 = seq(1, nrow(outputfresult), 1)), 
        start = list(ps = 0.01, pd = 0))

# viz
output %>%
  filter(N0_N %in% c(0.20, 0.30, 0.60)) %>%
  ggplot(aes(times, Ft, color = N0_N, group = N0_N)) +
  geom_line()

# manual observation of potential fit
output2 <- output %>%
  group_by(times) %>%
  group_split()

preview(formula_F_asymetric, data = output2[[1]] %>% mutate(N0 = seq(1, nrow(output2[[1]])), vectorank = cumsum(x = N0)), 
        start = list(ps = 1, pd = 0, alpha=-0.5))


# en moyenne
outputfresult <- output %>%
  group_by(N0_N) %>%
  summarise(Ft = sum(Ft)/(500-1))

preview(formula_F_asymetric, data = outputfresult %>% mutate(N0 = seq(1, nrow(outputfresult), 1), vectorank = N0), 
        start = list(ps = 0.001, pd = 0.004, alpha=-0.5))







