library(tidyverse)
library(osfr)
library(lubridate)
library(tidytags)
library(sf)

# uncomment the following lines of code to download the data

# f <- osf_retrieve_file("https://osf.io/9ydz6/")
# osf_download(f)
# file.rename("ALL_NGSSchat_DATA_201109060000_TO_202006202334.rda", "all_ngsschat.rda")

load("all_ngsschat.rda")

d <- tweets_dl %>%
  mutate(screen_name = tolower(screen_name),
         year = year(created_at),
         month = month(created_at),
         day = round_date(created_at, "day"))

spam <- read_csv("spam.csv")

sn <- spam %>% 
  mutate(screen_name = tolower(X47)) %>% 
  pull(screen_name)

sn[which(sn %in% d$screen_name)]

d %>% 
  mutate(screen_name = tolower(screen_name))

# geocode

# 
# t1 <- Sys.time()
# geocode_tags
# geocoded_tags1 <- tidytags::geocode_tags(users[1001:length(users$location),])
# t2 <- Sys.time()
# t2 - t1

users <- rtweet::users_data(d)

users <- users %>% 
  mutate(location = tolower(location)) %>% 
  distinct(location, .keep_all = TRUE)

location_index <-
  which((users$location != "") & !(stringr::str_detect(users$location, "#")))
locations_minus_blank <-
  users$location[location_index]

users <- users[location_index, ]

#geocoded_locs <- tidytags::geocode_tags(users)
#write_rds(geocoded_locs, "geocoded-locs.rds")

geocoded_locs <- read_rds("geocoded-locs.rds")

geocoded_locs

coords <- do.call(rbind, st_geometry(geocoded_locs)) %>% 
  as_data_frame() %>% setNames(c("lon","lat")) %>% 
  filter(!is.na(lon), !is.na(lat))

missing_coord <- is.na(st_dimension(geocoded_locs))
t <- tibble(locations = locations_minus_blank, geocoded_locs)
t <- t[!missing_coord,]


states <- maps::map.where("state", x = coords$lon, y = coords$lat)

t$state <- states

t <- t %>% 
  select(location = locations, state) %>% 
  mutate(state = if_else(str_detect(location, "alaska"), "alaska", state),
         state = if_else(str_detect(location, "hawaii"), "hawaii", state),
         )

all_users <- rtweet::users_data(d)

all_users <- all_users %>% 
  mutate(location = tolower(location)) %>% 
  distinct(user_id, .keep_all = TRUE) %>% 
  left_join(t)

write_csv(all_users, "all-users-with-locations.csv")

users <- users %>% 
  left_join(t)

users$state <- users$state %>% 
  str_split(":") %>% 
  map_chr(~.[1])

f <- function(x) {
  x > 0
}

spread_users <- d %>% 
  count(screen_name, year) %>% 
  spread(year, n, fill = 0) %>% 
  select(-`2011`, -`2020`) %>% 
  mutate_if(is.double, f)

users <- users %>% 
  left_join(spread_users) %>% 
  gather(key, val, `2012`:`2019`)

users <- users %>% 
  filter(val == TRUE)

write_csv(state_counts, "state-counts.csv")

library(albersusa)
library(gganimate)

US <- usa_sf()

state_counts <- users %>% 
  count(state, key) %>% 
  rename(name = state) %>% 
  mutate(name = tools::toTitleCase(name)) %>% 
  spread(key, n, fill = 0) %>% 
  gather(year, n, -name)

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

d %>% 
  count(screen_name, year) %>% 
  filter(n >= 10)
  mutate(date = ymd(str_c(year, "-01-01"))) %>% 
  ggplot(aes(x = date, y = n, group = screen_name)) +
  geom_point() +
  geom_line() 

d %>% 
  count(screen_name, month, year) %>% 
  count(month, year) %>% 
  mutate(date = ymd(str_c(year, "-", month, "-01"))) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_point()

d %>% 
  count(screen_name, year) %>% # 16,328 participants
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


