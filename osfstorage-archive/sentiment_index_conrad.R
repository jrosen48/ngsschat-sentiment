rm(list=ls())

load("NGSSchat_sentiment_states.rda")
# tweets_dl <- rtweet::read_twitter_csv("NGSSchat_sentiment_states.csv")

# Sentiment index over complete data

table(tweets_dl$senti_class)[1] / table(tweets_dl$senti_class)[3]

sentiment_index <- table(tweets_dl$senti_class, tweets_dl$state)

apply(sentiment_index, 1, sum)  # overall positive, neutral, negative sentiment

# Sentiment index by state

apply(sentiment_index, 2, sum)  # number of tweets per state

sentiment_index_states <- sentiment_index[1,] / sentiment_index[3,]

print(sentiment_index_states)   # index by state
summary(sentiment_index_states)

