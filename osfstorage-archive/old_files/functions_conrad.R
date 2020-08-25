###################################################################
# functions_conrad.R
# Last mod: Feb/02/2020

# Main dependencies:
# R 3.6.2, win x64
# osfr_0.2.8
# rtweet_0.7.0
# Token for OSF project /vk67n

# With this script, all functions for dltweets_conrad.R 
# are defined, explained and showcased using examples.

# After sourcing this script with source(), all relevant
# functions are stored in the R environment, so that the
# main script dltweets_conrad.R can be run. This command
# is already included in dltweets_conrad.R

# Time in this script is standardized to Greenwich Mean Time (UTC).

# This script is currently under construction and built for
# the research project "SOCIAL NETWORK ANALYSIS OF #NGSSCHAT"
###################################################################

# Loading packages, installing non-installed packages

packages <- list("rtweet", "magrittr", "osfr", "rlist")

for (package in packages){
		if(eval(bquote(!require(.(package))))){
				install.packages(package);
				eval(bquote(require(.(package))))
}}

rm(packages)

############## All functions ########################

# date.convert()

# This function converts a range of days of the format
# "yyyy-mm-dd" to "yyyymmdd", as necessary for Twitter
# API queries.

# The arguments of the function are the starting and 
# ending day of the given range.

# The function returns a list of all days in the 
# new format.

date.convert <- function(sday, eday){
			days <- seq(as.Date(sday), as.Date(eday), by="days")
			days <- gsub("-", "", days) %>% as.list()
			return(days)
}

# Example for date.convert()

days <- date.convert("2000-01-01", "2000-02-01")
days

rm(days)

#######################################################

# days2frames()

# This function adds a timeframe to each
# element in a list of days.

# It takes a list of days of the format
# "yyyymmdd" and a time frame with two points 
# of time of the format "hhmm" as arguments.

# The function returns a list of timeframes
# in which each element of the list consists
# of two elements of the format "yyyymmddhhmm",
# making up the beginning and end of the 
# timeframe of the days.

# This format can directly be used for queries
# withing the R-package rtweet.

days2frames <- function(days, sframe, eframe){
			for (i in 1:length(days)){
					working <- days[i]
					days[[i]][1] <- paste(c(working, sframe), collapse="")
					days[[i]][2] <- paste(c(working, eframe), collapse="") 
			}
            frames <- days
			return(frames)
}

# Example days2frames()

days <- date.convert("2000-01-01", "2000-02-01")

frames <- days2frames(days, "1200", "1230")
frames

rm(days, frames)

#######################################################

# frames.dl()

# This function downloads tweets for all elements
# in a list of timeframes as returned by days2frames()
# and stores each query in a list.

# It additionally takes a query string "q" and
# a maximum number of tweets to return n as arguments.
# By default, the limit n is set to 500.

# It employs the function search_fullarchive()
# from the R-package rtweet_0.7.0 within an
# research environment Twitter API.

# Be aware that each day in your frames object
# corresponds to one query in your Twitter API.

frames.dl <- function(frames, q, n=500){
				tweets <- list(NULL)
				for (i in 1:length(frames)){
					tweets[[i]] <- search_fullarchive(q=q, n=n, 
					                fromDate = frames[[i]][1], 
                                    toDate = frames[[i]][2],
									env_name = "research",
									token = token)
				}
				return(tweets)
}

# Example frames.dl()

# As this example requires Twitter API queries, it is put as a comment

# get.token()

days <- date.convert("2019-04-01", "2019-04-03")
frames <- days2frames(days, "1200", "1230")

tweets <- frames.dl(frames, "#food", 3)
# tweets

# rm(days, frames, tweets)

#######################################################

# to.UCT()

# This function standardizes a datetime format 
# (e.g. POSIXct) to Greenwich Mean Time (UCT).

# This is relevant since the Twitter API does not always
# return UCT format, but rather POSIXct (CET)
# when used in central Europe without additional settings.

to.UCT <- function(datetime){
    return(as.POSIXlt(datetime, tz="UTC"))
}

# Example to.UCT()

datetime <- as.POSIXct("2020-01-31 12:00")

to.UCT(datetime)

rm(datetime)

#######################################################

# UCT2frame()

# This function converts a UCT datetime object
# to a timeframe of the format yyyymmddhhmm
# so that is can be used for future queries.

UCT2frame <- function(datetime){
    frame <- strftime(datetime,"%Y-%m-%d %H:%M:%S", tz="UCT")
    frame <- substr(frame, 1, nchar(frame) - 3)
    frame <- gsub("-", "", frame)
    frame <- gsub(":", "", frame)
    frame <- gsub(" ", "", frame)
    return(frame)
}

# Example UCT2frame()

datetime <- as.POSIXct("2020-01-31 12:00")
datetime <- to.UCT(datetime)

frame <- UCT2frame(datetime)
frame

rm(datetime, frame)

#######################################################

# mark.exceeded()

# This functions marks all queries done by frames.dl() which 
# returned more tweets than a given number n (which is by default 
# set to 500, like a standard query) by returning the respective
# last tweet.

# Therefore, the argument tweets is set to be a list of query
# returns done by the R-package rtweet.

mark.exceeded <- function(tweets, n=500){
    marked <- list()
    for (i in 1:length(tweets)){
             t.len <- length(tweets[[i]]$text)
             if (t.len >= n){
                 marked <- list.append(marked, tweets[[i]][t.len,])
             }
        }
    return(marked)
}

# Example mark.exceeded()

# As this example requires Twitter API queries, it is put as a comment

# get.token()

# days <- date.convert("2019-04-01", "2019-04-03")
# frames <- days2frames(days, "1200", "1230")
# tweets <- frames.dl(frames, "#food", 3)

# last.tweets <- mark.exceeded(tweets, 3)

# rm(days, frames, tweets, last.tweets)

#######################################################

# exceeded.framestatus()

# This functions returns a list in which last frames of 
# exceeded queries are put into the correct query-format
# yyyymmddhhmm.

# Additionally, to check for overlaps in the following query,
# which is caused by each query only being exact to the minute,
# the exact time yyyy-mm-dd hh:mm:ss UCT as well as the 
# text of each last tweet is returned.

# The function therefore returns a list in which each object 
# holds the last frame, time and text of the last tweet
# in a given exceeded query from a list of queries put as an argument.

# Employing the returned timeframes, one can later do another query that 
# potentially completes the incomplete queries for each incomplete day.
# Overlaps can later be deleted through the additionally returned text
# and time markers.

 exceeded.framestatus <- function(tweets, n=500){
    marked <- mark.exceeded(tweets, n)
    last.status <- list()
    for (i in 1:length(marked)){
             last.time <- to.UCT(marked[[i]]$created_at)
             endframe <- UCT2frame(last.time)
             last.status[[i]] <- c(endframe,  
                    strftime(last.time, "%Y-%m-%d %H:%M:%S", tz="UCT"),
                    marked[[i]]$text)
                                    }
    return(last.status)
}

# Example exceeded.framestatus()

# As this example requires Twitter API queries, it is put as a comment

# get.token()

# days <- date.convert("2019-04-01", "2019-04-03")
# frames <- days2frames(days, "1200", "1230")
# tweets <- frames.dl(frames, "#food", 3)

# last.status <- exceeded.framestatus(tweets, 3)

# rm(days, frames, tweets, last.status)

#######################################################

# get.exceeded() [IN PROGRESS]

# This function will start a new query to obtain the rest of
# the tweets for queries that exceeded n. Overlaps in
# tuples of queries will automatically be deleted.

################# File routines to OSF ############################ 

# Retrieving project tibble 

# project <- osf_retrieve_node("vk67n")

# Downloading this file

# filepath <- osf_retrieve_file("https://osf.io/k6sbh/")
# osf_download(filepath, conflict="overwrite")

# Updating this file

# osf_upload(project, "functions_conrad.R", conflicts="overwrite")
