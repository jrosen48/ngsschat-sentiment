rm(list=ls())

library(rtweet)

load("NGSSchat_sentiment_states.rda")

binary <- read.table("binary.txt", sep="\t", header = T, quote="")
trinary <- read.table("trinary.txt", sep="\t", header = T, quote="")

tweets_dl <- cbind(tweets_dl, binary$Overall, trinary$Overall)

names(tweets_dl)[95:96] <- c("senti_binary", "senti_trinary")

# save(tweets_dl, file="NGSSchat_sentiment_states_revised.rda")
# save_as_csv(tweets_dl, "NGSSchat_sentiment_states_revised.csv")

# Is there a difference between the classifications?

# Sentiment index by "manual classification" of default scale
table(tweets_dl$senti_class)[1] / table(tweets_dl$senti_class)[3] # 0.21

# Sentiment index by binary classification
table(tweets_dl$senti_binary)[1] / table(tweets_dl$senti_binary)[2] # 0.12

# Sentiment index by trinary classification
table(tweets_dl$senti_trinary)[1] / table(tweets_dl$senti_trinary)[3] # 0.22
