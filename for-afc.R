library(brms)
library(readr)

d <- read_csv("data-for-hpc.csv")
m <- brms::brm(mvbind(posRate, negRate) ~ -1 + 
                 year_fct + adoption_key + isChat + 
                 (1|state) + (1|screen_name), 
               data = d, 
               chains = 4, cores = 4, iter = 1000)
