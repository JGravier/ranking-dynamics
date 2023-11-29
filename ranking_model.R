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
output <- f_calculus_F(vector_entry = init_size, list_entry = base_model_f, 
                       timesT = length(number_times), N = length(init_size))

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
output <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = base_model_f)

bmf2 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0)
output2 <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = bmf2)

bmf3 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0.5)
output3 <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = bmf3)

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
output <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = base_model_f)

bmf2 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0)
output2 <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = bmf2)

bmf3 <- f_ranking_model(vector_entry = init_size, times = number_times, ps = 0.1, pd = 0.4, alpha = 0.5)
output3 <- f_calculus_F(vector_entry = init_size, times = number_times, list_entry = bmf3)

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

#### fit with alpha = -0.5 ####
# asymetrique model
formula_F_asymetric <- as.formula(Fresult ~ ps * ( (N0_N^(1-alpha)) * (1-N0_N) + ( (1-N0_N)^(1+alpha) ) * N0_N ) +
                                    (1 - ps) * pd * N0_N^(-alpha) * (1/N) )

output <- output %>%
  mutate(N = length(init_size))

# write output
write_csv(x = output, file = "output_data/model_alpha_0.5_T_10000.csv")

# manual observation of potential fit
preview(formula_F_asymetric, data = output, start = list(ps = 0.1, pd = 0, alpha=-0.3))

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

