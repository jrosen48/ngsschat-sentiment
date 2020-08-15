## code for creation of NGSSchat_sentiment_states_newly_revised.csv
## end result: adds positive and negative continuous scale columns

d <- read.csv("NGSSchat_sentiment_states_newly_revised.csv")
state_counts <- read_csv("state-counts.csv")

h <- read_csv("NGSSchat_sentiment_states_revised.csv")
h <- h[, 5]
h$text <- gsub("[\r\n]", "", h$text)
write.table(h, file = "ngsschat_tweets.txt", sep = "", row.names = F)
## run sentistrength and read file back in
h2 <- read_table2("~/Desktop/ngsschat_tweets1_out.txt")
d$posRate_cont <- h2$Positive
d$negRate_cont <- h2$Negative
write.csv(d, "NGSSchat_sentiment_states_newly_revised.csv")
