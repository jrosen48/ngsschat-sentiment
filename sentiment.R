# 1. loading, setting up

library(tidyverse)
library(osfr)
library(lubridate)
library(tidytags)
library(sf)
library(albersusa)
library(gganimate)
library(transformr)

state_data <- read_csv("ngsschat-state-data.csv")

state_data <- state_data %>% 
  select(State, Early:Not) %>% 
  gather(key, val, -State) %>% 
  filter(!is.na(val))

# uncomment the following two lines of code to download the data
f <- osf_retrieve_file("https://osf.io/7sgw6/")
osf_download(f, conflicts = "overwrite")

d <- read_csv("NGSSchat_sentiment_states_revised.csv")
state_counts <- read_csv("state-counts.csv")

d <- d %>%
  mutate(screen_name = tolower(screen_name),
         year = year(created_at),
         month = month(created_at),
         day = round_date(created_at, "day"))

US <- usa_sf()

##### SENTIMENT MEANS #####
sentiment_states <- d %>% 
  select(year, state, Positive, Negative, senti_class)

unique(sentiment_states$state)
sentiment_states$state <- sentiment_states$state %>%
  str_remove(":main") %>%
  str_remove(":south") %>%
  str_remove(":north") %>%
  str_remove(":long island") %>%
  str_remove(":manhattan")
sentiment_states <- mutate(sentiment_states, state = tools::toTitleCase(state))
unique(sentiment_states$state)

sentiment_means <- state_counts

sentiment_states$pos <- 0
sentiment_states$neg <- 0
sentiment_states$pos[sentiment_states$senti_class == "positive"] <- 1
sentiment_states$neg[sentiment_states$senti_class == "negative"] <- 1

poscount <- aggregate(sentiment_states$pos,
                               by = list(sentiment_states$state, sentiment_states$year),
                               FUN = sum, 
                               na.rm = TRUE)
poscount <- rename(poscount, "name" = "Group.1", "year" = "Group.2", "nPos" = x)

negcount <- aggregate(sentiment_states$neg,
                      by = list(sentiment_states$state, sentiment_states$year),
                      FUN = sum, 
                      na.rm = TRUE)
negcount <- rename(negcount, "name" = "Group.1", "year" = "Group.2", "nNeg" = x)

senticount <- left_join(poscount, negcount, by = c("name", "year"))
senticount$avg <- (senticount$nNeg) / (senticount$nPos)
senticount$avg[senticount$avg == Inf] <- 0

sentiment_means <- left_join(sentiment_means, senticount, by = c("name", "year"))
sentiment_means <- sentiment_means %>% subset(select = -c(n))

# overall mean
sentiment_means %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE))

# mean by year
sentiment_means %>% 
  group_by(year) %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE))

# mean by state
sentiment_means %>% 
  group_by(name) %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE)) %>% 
  arrange(desc(sentiment_ratio))

# by status
sentiment_means %>%
  rename(State = name) %>% 
  left_join(state_data) %>%
  group_by(key) %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE)) %>% 
  arrange(desc(sentiment_ratio)) %>% 
  filter(!is.na(key)) %>% 
  clipr::write_clip()

# for plot
state_means <- sentiment_means %>% 
  group_by(name) %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE)) %>% 
  arrange(desc(sentiment_ratio))

s <- left_join(US, state_means)

p1 <- ggplot(s, aes(fill = sentiment_ratio)) +
  geom_sf() + 
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  theme_void() +
  scale_fill_viridis_c("Sentiment Ratio (- / + Tweets)", option = "D") +
  hrbrthemes::theme_ipsum(grid = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(text = element_text(family = "Times"))

p1

ggsave("states-sentiment.png", width = 10, height = 6)


p2 <- sentiment_means %>% 
  filter(!is.na(name)) %>% 
  group_by(year) %>% 
  summarise(sentiment_ratio = sum(nNeg, na.rm = TRUE)/sum(nPos, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = sentiment_ratio)) +
  geom_smooth(color = "#39568CFF") +
  geom_point() +
  theme_minimal() +
  theme(text = element_text(family = "Times")) +
  ylab("Sentiment Ratio (- / + Tweets)") +
  xlab(NULL) +
  theme(text = element_text(family = "Times", size = 16)) 

p2

ggsave("year-sentiment.png", width = 10, height = 6)


##### MAPPING SENTIMENT MEANS #####
s <- left_join(US, sentiment_means)
s$`Mean Sentiment Score` <- s$avg
s$year <- as.integer(s$year)
s <- s %>% 
  filter(!is.na(year))

p <- ggplot(s, aes(fill = `Mean Sentiment Score`)) +
  geom_sf() + 
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") +
  theme_void() +
  ggtitle("Mean sentiment of tweets regarding #NGSSchat (2012-2019)?") +
  scale_fill_viridis_c(option = "D") +
  hrbrthemes::theme_ipsum(grid = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')

animate(p)

anim_save("ngsschat-sentiment-means.gif")
