# processing-all-locations
library(tidyverse)
library(sf)
# locs <- read_csv("geolocated-locs.csv")
# locs <- locs$value

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
  which((new_locs_df$location != "") & !(stringr::str_detect(new_locs_df$location, "#")) & !(stringr::str_detect(new_locs_df$location, "&")))

locations_minus_blank <-
  new_locs_df$location[location_index]

out1a <- mapsapi::mp_geocode(locations_minus_blank[1:1250], key ="AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out1a, "out1a.rds")
out1b <- mapsapi::mp_geocode(locations_minus_blank[1251:2499], key ="AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out1b, "out1b.rds")
out2 <- mapsapi::mp_geocode(locations_minus_blank[2501:5000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out2, "out2.rds")
out3 <- mapsapi::mp_geocode(locations_minus_blank[5001:7500], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out3, "out3.rds")
out4a <- mapsapi::mp_geocode(locations_minus_blank[7501:8750], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out4a, "out4a.rds")
out4b <- mapsapi::mp_geocode(locations_minus_blank[8751:10000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out4b, "out4b.rds")

out5 <- mapsapi::mp_geocode(locations_minus_blank[10001:11000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out5, "out5.rds")
out6 <- mapsapi::mp_geocode(locations_minus_blank[11001:12000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out6, "out6.rds")

out7 <- mapsapi::mp_geocode(locations_minus_blank[12001:14000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out7, "out7.rds")

out8 <- mapsapi::mp_geocode(locations_minus_blank[14001:15000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out8, "out8.rds")

out9a <- mapsapi::mp_geocode(locations_minus_blank[15001:16000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out9a, "out9a.rds")
out9b <- mapsapi::mp_geocode(locations_minus_blank[16001:17000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out9b, "out9b.rds")

out10 <- mapsapi::mp_geocode(locations_minus_blank[17001:19000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out10, "out10.rds")

out11 <- mapsapi::mp_geocode(locations_minus_blank[19001:21000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out11, "out11.rds")

out12 <- mapsapi::mp_geocode(locations_minus_blank[21001:23000], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out12, "out12.rds")

out13 <- mapsapi::mp_geocode(locations_minus_blank[23000:23874], key = "AIzaSyAn7acOv54hSusg4khN5X2sPtIg8LSVzBM")
write_rds(out13, "out13.rds")

files <- list.files(pattern = "*.rds")[-1]
l <- map(files, read_rds)

f <- function(geocoded_locs) {
  coords <- do.call(rbind, st_geometry(geocoded_locs)) %>% 
    as_data_frame() %>% setNames(c("lon","lat")) %>% 
    filter(!is.na(lon), !is.na(lat))
}

f(l[[1]])
