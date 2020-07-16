# 1. loading, setting up

library(tidyverse)
library(osfr)
library(lubridate)
library(tidytags)
library(sf)
library(albersusa)
library(gganimate)

# uncomment the following two lines of code to download the data

# f <- osf_retrieve_file("https://osf.io/684cx/")
# osf_download(f)

d <- read_csv("NGSSchat_sentiment_states.csv")
state_counts <- read_csv("state_counts.csv")

d <- d %>%
  mutate(screen_name = tolower(screen_name),
         year = year(created_at),
         month = month(created_at),
         day = round_date(created_at, "day"))

d %>% 
  select(year, state, Positive, Negative, senti_class)

# note to Ben - `d` is our new data

# 2. mapping

US <- usa_sf()

# note to Ben - `d` is our new data
# the goal is to get a data frame like state counts using our new data 
# and using the sentiment scores (mean sentiment per state), instead of a count of 
# people per state

state_counts <- users %>% 
  count(state, key) %>% 
  rename(name = state) %>% 
  mutate(name = tools::toTitleCase(name)) %>% 
  spread(key, n, fill = 0) %>% 
  gather(year, n, -name)

state_conts

s <- left_join(US, state_counts)
s$`No. of Participants` <- s$n
s$year <- as.integer(s$year)
s <- s %>% 
  filter(!is.na(year))

p <- ggplot(s, aes(fill = `No. of Participants`)) +
  geom_sf() + 
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  theme_void() +
  #ggtitle("From where have people participated in #NGSSchat (2012-2029)?") +
  scale_fill_viridis_c(option = "D") +
  hrbrthemes::theme_ipsum(grid = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {key}') +
  transition_time(key) +
  ease_aes('linear')

animate(p)

ggsave("ngsschat-location.png", width = 8, height = 6)