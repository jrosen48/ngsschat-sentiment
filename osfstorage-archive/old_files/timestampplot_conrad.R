###################################################################
# timestampplot_conrad.R
# Last mod: Feb/02/2020

# Main dependencies:
# R 3.6.2, win x64
# osfr_0.2.8
# rtweet_0.7.0
# Token for OSF project /vk67n

# With this script, timeseries plots of the timestamps of
# #NGSSchat tweets are plotted and explored.

# This script is currently under construction and built for
# the research project "SOCIAL NETWORK ANALYSIS OF #NGSSCHAT"
###################################################################

# Loading packages, installing non-installed packages

packages <- list("ggplot2", "dplyr", "tidytext", "igraph",
                 "magrittr", "osfr", "tidyverse", "lme4",
                 "sjstats", "rtweet", "rTAGS", "brms", "ggthemes",
                "mapproj", "ggraph", "tidygraph", "broom")


for (package in packages){
		if(eval(bquote(!require(.(package))))){
				install.packages(package);
				eval(bquote(require(.(package))))
}}

rm(packages)

# Downloading and sourcing data

# If "all-ngsschat-tweets-flattened.csv" 
# is not in current working directory:

# filepath <- osf_retrieve_file("https://osf.io/xhab9/")
# osf_download(filepath, conflict="overwrite")

dat <- read.csv("all-ngsschat-tweets-flattened.csv")

dat %>% 
  filter(!is_retweet) %>% 
  ts_plot() +
  theme_bw() +
  xlab("Day") +
  ylab("Number of Original Tweets") +
  xlim(c(as.POSIXct(as.Date("2012-01-01")), as.POSIXct(as.Date("2017-12-31")))) +
  geom_rect(aes(xmin = as.POSIXct(as.Date(c("2013-08-01"))), 
                xmax =
                  as.POSIXct(as.Date(c("2014-07-31"))),
                ymin = 0,
                ymax = 925),
            fill = "gray55", alpha = 0.025) +
  geom_rect(aes(xmin = as.POSIXct(as.Date(c("2015-08-01"))), 
                xmax =
                  as.POSIXct(as.Date(c("2016-07-31"))),
                ymin = 0,
                ymax = 925),
            fill = "gray55", color = "gray55", alpha = 0.025) +
  geom_rect(aes(xmin = as.POSIXct(as.Date(c("2014-07-31"))), 
                xmax = as.POSIXct(as.Date(c("2015-07-31"))),
                ymin = 0,
                ymax = 925),
            fill = "gray90", alpha = .025) +
  theme(text = element_text(family = "Times"))

# ggsave("ngsschat-n-tweets.png", width = 7, height = 5)

################# First tweet ####################################

# Judging by "all-ngsschat-tweets-flattened.csv":

tweet.times <- dat$created_at
tweet.times <- substr(tweet.times, 1, 10)

sort(tweet.times) %>% head()

# Output: May 11th, 2012

# Search for additional older tweets using Twitter Premium API
# (n can be small since oldest tweet is returned first)

search_fullarchive(q="#NGSSchat", n=10, fromDate = "201001010000",
                   toDate = "201205102359", env_name = "research")	

# Output: tba, currently my (Conrad) API queries expired

################# File routines to OSF ############################ 

# Retrieving project tibble 

# project <- osf_retrieve_node("vk67n")

# Downloading this file

# filepath <- osf_retrieve_file("https://osf.io/xuczv/")
# osf_download(filepath, conflict="overwrite")

# Updating this file

# osf_upload(project, "timestampplot_conrad.R", conflicts="overwrite")
