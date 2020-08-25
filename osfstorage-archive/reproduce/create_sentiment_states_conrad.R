# Setup

rm(list=ls())

library(magrittr)
library(rtweet)

# This is how "NGSSchat_with_states.rda" was created, takes around 2 minutes to run

users <- read.csv("all-users-with-locations.csv")
load("ALL_NGSSchat_DATA_201109060000_TO_202006202334.rda")

ind <- NULL
tweets_dl$state <- NA

users$state <- as.character(users$state)

for (i in 1:nrow(users)){
 ind <- which(users$user_id[i] == tweets_dl$user_id)
  tweets_dl$state[ind] <- users$state[i]
}

tweets_dl$state <- as.factor(tweets_dl$state)

load("NGSSchat_with_states.rda")

# Sentiment analysis

# SentiStrength data prep

## Remove line breaks

senti <- tweets_dl

senti$text <- gsub("[\r\n]", "", senti$text)

write.table(senti$text, "_senti_full.txt", row.names = FALSE)

## APPLY SENTISTRENGTH

senti_out <- read.table("_senti_full+results.txt", sep="\t", header = T)

senti_out <- read.table("_senti+results.txt", sep="\t", header = T)
senti_out <- cbind(senti$status_id, senti_out[, 2:3])

senti_class <- rep(NA, nrow(tweets_dl))

for (i in 1:nrow(tweets_dl)){

if (senti_out$Positive[i] +  senti_out$Negative[i] == 0){
  senti_class[i] <- "neutral"
}
  
else if (senti_out$Positive[i] + senti_out$Negative[i] > 0){
  senti_class[i] <- "positive"
}
  
else {
  senti_class[i] <- "negative"
}
  
}

senti_out$senti_class <- senti_class
senti_out$senti_class <- as.factor(senti_out$senti_class)

# names(tweets_dl)

tweets_dl <- cbind(tweets_dl, senti_out)

# save(tweets_dl, file="NGSSchat_sentiment_states.rda")
# write_as_csv(tweets_dl, "NGSSchat_sentiment_states.csv")
