#! /usr/bin/env bash

# References: 
# https://developer.twitter.com/en/docs/twitter-api/tweets/search/api-reference/get-tweets-search-all

# Setup
BEARER_TOKEN=$(cat token.txt)

from="2008-01-01T00%3A00%3A00Z"
to="2020-12-31T23%3A59%3A59Z"
#max="10"   # testing
max="500" 	# download

# Variables, currently no pre-selection
tweet="&tweet.fields=attachments,author_id,context_annotations,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,public_metrics,possibly_sensitive,referenced_tweets,reply_settings,source,text,withheld"
user="&user.fields=created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld"
expansions="&expansions=attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id"
media="&media.fields=duration_ms,height,media_key,preview_image_url,type,url,width,public_metrics"
geo="&place.fields=contained_within,country,country_code,full_name,geo,id,name,place_type"
poll="&poll.fields=duration_minutes,end_datetime,id,options,voting_status"
vars="$tweet$user$expansions$media$geo$poll"

# Functions
. functions.sh

# Re-run script with each
q="#ngsschat"
download_current_q

q="ngss"
download_current_q

q="next gen science standard"
download_current_q

q="next gen science standards"
download_current_q

q="next generation science standard"
download_current_q

q="next generation science standards"
download_current_q
