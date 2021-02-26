library(dplyr)
library(purrr)
library(stringr)

rm(list=ls())

d <- readRDS("../data-raw/ngss-sentiment-raw-2021-20-02.rds")$users %>%
    select(id, location) %>%
    rename(user_id=id) %>%
    filter(!is.na(location)) %>%
    distinct(user_id, .keep_all=T)

# Pre-selection to reduce runtime of geocoding API

states <- read.table(sep=",",header=T,text="
name,short,postal
Alabama,Ala\\.,AL
Alaska,Alaska,AK
Arizona,Ariz\\.,AZ
Arkansas,Ark\\.,AR
California,Calif\\.,CA
Colorado,Colo\\.,CO
Connecticut,Conn\\.,CT
Delaware,Del\\.,DE
District of Columbia,D\\.C\\.,DC
Florida,Fla\\.,FL
Georgia,Ga\\.,GA
Hawaii,Hawaii,HI
Idaho,Idaho,ID
Illinois,Ill\\.,IL
Indiana,Ind\\.,IN
Iowa,Iowa,IA
Kansas,Kans\\.,KS
Kentucky,Ky\\.,KY
Louisiana,La\\.,LA
Maine,Maine,ME
Maryland,Md\\.,MD
Massachusetts,Mass\\.,MA
Michigan,Mich\\.,MI
Minnesota,Minn\\.,MN
Mississippi,Miss\\.,MS
Missouri,Mo\\.,MO
Montana,Mont\\.,MT
Nebraska,Nebr\\.,NE
Nevada,Nev\\.,NV
New Hampshire,N\\.H\\.,NH
New Jersey,N\\.J\\.,NJ
New Mexico,N\\.M\\.,NM
New York,N\\.Y\\.,NY
North Carolina,N\\.C\\.,NC
North Dakota,N\\.D\\.,ND
Ohio,Ohio,OH
Oklahoma,Okla\\.,OK
Oregon,Ore\\.,OR
Pennsylvania,Pa\\.,PA
Rhode Island,R\\.I\\.,RI
South Carolina,S\\.C\\.,SC
South Dakota,S\\.D\\.,SD
Tennessee,Tenn\\.,TN
Texas,Tex\\.,TX
Utah,Utah,UT
Vermont,Vt\\.,VT
Virginia,Va\\.,VA
Washington,Wash\\.,WA
West Virginia,W\\.Va\\.,WV
Wisconsin,Wis\\.,WI
Wyoming,Wyo\\.,WY
")

## Postal
d$state <- str_extract(d$location, paste0(states$postal, collapse="|"))
d %>% filter(!is.na(state)) %>% select(user_id, state) -> res_postal
d <- d %>% filter(is.na(state))

# Postal second letter lower
states$postal_last_lower <- paste0(substr(states$postal,1,1),substr(states$postal,2,2) %>% tolower())
d$state <- str_extract(d$location, paste0(states$postal_last_lower, collapse="|"))
d %>% filter(!is.na(state)) %>% select(user_id, state) -> res_postal_last_lower
d <- d %>% filter(is.na(state))

## Full names
d$state <- str_extract(d$location %>% tolower(), paste0(states$name, collapse="|") %>% tolower())
d %>% filter(!is.na(state)) %>% select(user_id, state) -> res_name
d <- d %>% filter(is.na(state))

# Short names
d$state <- str_extract(d$location %>% tolower(), paste0(states$short, collapse="|") %>% tolower())
d %>% filter(!is.na(state)) %>% select(user_id, state) -> res_short
d <- d %>% filter(is.na(state))

# Match old API calls
o <- readr::read_csv("old-location-matches.csv") %>% select(location, state)
o$state[is.na(o$state)] <- "no match" 
d$location_orig <- d$location; d$location <- d$location %>% tolower()
d <- d %>% select(-state) %>% left_join(o, by="location") %>% distinct(user_id, .keep_all=T)
d %>% filter(!is.na(state)) %>% select(user_id, state) -> res_old
d <- d %>% filter(is.na(state)) %>% select(-location) %>% rename(location=location_orig) 

d$location <- d$location %>% str_remove_all("[[:punct:]]")
#saveRDS(d, "remaining.rds")

###

library(dplyr)
library(purrr)
library(stringr)
library(mapsapi)
library(sf)

#rm(list=ls())

#d <- readRDS("remaining.rds")
#key <- readLines("geokey.txt")

#out <- list()
#j <- 1

#out <- readRDS("interim-out.rds")
#j <- readRDS("interim-j.rds")

#for (i in j:nrow(d)) {
#   res <- mapsapi::mp_geocode(d$location[i], key=key)
#   res <- try(mapsapi::mp_get_points(res))
#   if(res$status=="ZERO_RESULTS" | !is.logical(res$status=="ZERO_RESULTS") | is.na(res$status)){
#      j<-j+1
#      cat("\014", i, "out of", nrow(d), "geocoded\n")
#      next
#   }
#   address <- res %>% pull(address_google)
 #  res <- res %>% pull(pnt) %>% as.character() %>% str_remove_all("^c\\(|\\)$") %>% str_split(", ") %>% unlist() %>% as.numeric()
 ##  res <- maps::map.where(x = res[1], y = res[2], database = "state")
 #  out[[j]] <- list(d$user_id[i], address, res)
 #  j<-j+1
 #  cat("\014", i, "out of", nrow(d), "geocoded\n")
#}#

#saveRDS(out, "interim-out.rds")
#saveRDS(j, "interim-j.rds")

### Merge all together

out <- readRDS("interim-out.rds")
vectors <- map(out, unlist)
cleaned <- vectors[lapply(vectors, length)==3]
out <- do.call(rbind, cleaned) %>% tibble()
out <- out$. %>% as.data.frame() %>% tibble()

out <- out %>% 
    filter(!is.na(V3)) %>% 
    filter(V2!="United States") %>% # gets matched as "Kansas"
    filter(V2!="USA") %>%
    filter(V2!="US") %>%
    filter(V2!="U.S.") %>%
    filter(V2!="U.S.A") %>%
    select(-V2) %>%
    rename(user_id=V1, state=V3)

all <- rbind(
out,
res_name,
res_old,
res_postal,
res_postal_last_lower,
res_short
)

states <- states %>%
    select(postal, name) %>%
    rename(state=postal)

all$state <- all$state %>% 
    str_remove_all(":.*") %>% # remove specifications (e.g., "michigan:south")
    str_remove_all("\\.") %>% # remove dots
    toupper() 

all <- all %>% 
    left_join(states, by="state")

all$state[!is.na(all$name)] <- all$name[!is.na(all$name)]
all <- all %>% select(-name) %>% mutate(state=toupper(state)) %>% filter(state!="NO MATCH")

all$state[all$state=="ARK"] <- "ARKANSAS"
all$state[all$state=="DEL"] <- "DELAWARE"
all$state[all$state=="FLA"] <- "FLORIDA"
all$state[all$state=="IND"] <- "INDIANA"
all$state[all$state=="ORE"] <- "OREGON"
all$state[all$state=="TENN"] <- "TENNESSEE"

saveRDS(all, "user-state-final-2021-02-25.rds")

