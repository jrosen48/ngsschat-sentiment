library(rtweet)

token <- create_token(app = "ed-sentiment",
             consumer_key = "Kaw5a6IZMCcwRX6M98rv8iPQA",
             consumer_secret = "1rKosimSLM8YztOZ9U5TKua4raHQTstSSCykHWi3v0B6A5DkwO",
             access_token = "10950512-iR1UMTnGJHDjZ4KcgHs4pstMxJanA1C1BGwKrQOhl",
             access_secret = "aUBwgxEYs4m2N3rMdetDHVqTtUNfDtxPORBsH8wAEheMg")

## select one or more twitter users to lookup
responded <- readr::read_csv("~/Downloads/responded.csv")

## get users data
usr_df <- lookup_users(responded$name, token = token)

drake::loadd(data_to_model)

data_to_model %>% 
  filter(context == "#NGSSchat") %>% 
  nrow()

data_to_model %>% 
  filter(context == "#NGSSchat") %>% 
  filter(user_id %in% usr_df$user_id) %>% 
  nrow()

