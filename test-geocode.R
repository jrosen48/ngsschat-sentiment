# test-geocode

library(rtweet)
library(mapsapi)
library(dplyr)
library(readr)
library(sf)

# # requires setting up credentials first using rtweet::create_token()
# 
# tweet_data <- rtweet::search_tweets("#rstats", n = 50) 
# 
# write_rds(tweet_data, "example-tweet-data.rds")

tweet_data <- read_rds("example-tweet-data.rds")

# requires passing a Google Maps Geocoding API key; tweet_data$location can be
# replaced with the variable/value for the location using the new API

geocoded_locs <- mapsapi::mp_geocode(tweet_data$location,
                                          key = "")

geocoded_pnts <- mapsapi::mp_get_points(geocoded_locs)

coords <- do.call(rbind, st_geometry(geocoded_pnts$pnt)) %>%
  as.data.frame() %>% setNames(c("lon","lat")) %>%
  filter(!is.na(lon), !is.na(lat))

identified_states <- maps::map.where(x = coords$lon, y = coords$lat, database = "state")

identified_states

# can also find for the world

# maps::map.where(x = coords$lon, y = coords$lat, database = "world")