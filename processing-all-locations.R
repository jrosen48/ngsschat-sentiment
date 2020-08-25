# processing-all-locations

locs <- read_csv("geolocated-locs.csv")
locs <- locs$value

load('data_aggregated_2020_08_25.rda')
d <- tweets_dl
d <- as_tibble(d)

new_locs <- d %>% 
  pull(location) %>% 
  unique() %>% 
  tolower()

new_locs <- new_locs[!(new_locs %in% locs)]

tidytags::geocode_tags

new_locs_df <- new_locs %>% as_tibble() %>% set_names("location")

location_index <-
  which((new_locs_df$location != "") & !(stringr::str_detect(new_locs_df$location, "#")))
locations_minus_blank <-
  new_locs_df$location[location_index]

out1 <- mapsapi::mp_geocode(locations_minus_blank[1:2500], key = Sys.getenv("Google_API_key"))
