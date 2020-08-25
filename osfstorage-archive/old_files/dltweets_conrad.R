###################################################################
# dltweets_conrad.R
# Last mod: Feb/02/2020

# Main dependencies:
# R 3.6.2, win x64
# osfr_0.2.8
# rtweet_0.7.0
# Token for OSF project /vk67n

# With this script, tweets between specific timeframes 
# can be downloaded employing the R-package rtweet.

# This script is currently under construction and built for
# the research project "SOCIAL NETWORK ANALYSIS OF #NGSSCHAT"
###################################################################

token <- rtweet::create_token(app = 'ngsschat',
                              consumer_key = 'Jyw9P8DUekBmNlW1GgvmDJ8e7',
                              consumer_secret = 'h2zm3ekapwr22PZIGbtMBT3vfFSVy1IF8c7hPtFCF5XGIhnG4D',
                              access_token = '10950512-4gZlYGwvX8rB1EogWJtPvMHLZf5cwvM5F6crvJQjf',
                              access_secret = '07OsaMI3cqPfLa9cO4UEV2QOSmETJ6CxOpUUlv4Vx3a4y')

rt <- rtweet::search_fullarchive("eloned", n = 100, env_name = "ngsschat",
                                 fromDate = "201401010000", toDate = "201406302359",
                                 token = token)

# Loading packages, installing non-installed packages

packages <- list("rtweet", "ggplot2", "dplyr", "tidytext", 
                 "magrittr", "osfr")

for (package in packages){
  if(eval(bquote(!require(.(package))))){
    install.packages(package);
    eval(bquote(require(.(package))))
  }}

rm(packages)

# Downloading and sourcing all relevant functions
# For documentation, see "functions_conrad.R"

# If "functions_conrad.R" is not in current working directory:

# filepath <- osf_retrieve_file("https://osf.io/k6sbh/")
# osf_download(filepath, conflict="overwrite")

source("osfstorage-archive/old_files/functions_conrad.R")

# Retrieving Twitter API token

get_token()

# Example:

days <- date.convert("2019-04-01", "2019-04-03")
frames <- days2frames(days, "1200", "1230")
tweets <- frames.dl(frames, "#food", 3)

# last.status <- exceeded.framestatus(tweets, 3)

################# File routines to OSF ############################ 

# Retrieving project tibble 

# project <- osf_retrieve_node("vk67n")

# Downloading this file

# filepath <- osf_retrieve_file("https://osf.io/h8jva/")
# osf_download(filepath, conflict="overwrite")

# Updating this file

# osf_upload(project, "dltweets_conrad.R", conflicts="overwrite")
