library(tidyverse)
library(survey)

d <- read_csv("survey-data/AIR0519T_ATP_CSV file_Portal.csv")

des <- svydesign(ids = ~1, 
                 data = d, 
                 weights = ~weight)

svymean(~air_t_cms_640_s_00, design = des, na.rm = T)
mean(stq$stq47b, na.rm = TRUE)

d$air_t_cms_640