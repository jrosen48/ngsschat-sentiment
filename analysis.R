library(tidyverse)
library(osfr)
library(lubridate)
library(tidytags)

# uncomment the following lines of code to download the data

f <- osf_retrieve_file("https://osf.io/9ydz6/")
osf_download(f)
file.rename("ALL_NGSSchat_DATA_201109060000_TO_202006202334.rda", "all_ngsschat.rda")

load("all_ngsschat.rda")

d <- tweets_dl %>%
  mutate(screen_name = tolower(screen_name),
         year = year(created_at),
         day = round_date(created_at, "day"))

d %>% 
  count(screen_name) %>% # 16,328 participants
  filter(n > 1) %>% # 6,065
  filter(n >= 10) # 1,472

d %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n)) +
  geom_point() 

d %>% 
  count(day) %>% 
  ggplot(aes(x = day, y = n)) +
  geom_point()


