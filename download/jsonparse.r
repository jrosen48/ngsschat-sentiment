library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(jsonlite)

rm(list=ls())

fs <- dir(path="json", full.names=T)

flatten_if_not_null <- function(l) {
    if (is.null(l)) return(NULL)
    else return(flatten(l, recursive=T))
}

collect_data <- function(files) {
    main=list(); users=list(); tweets=list()
    i <- 1
    for (file in files) {
        d <- fromJSON(file)     
        main[[i]] <- flatten_if_not_null(d$data) %>%
            select(id,created_at,author_id,text,lang,starts_with("public_metrics"),referenced_tweets)
        users[[i]] <- flatten_if_not_null(d$includes$users) %>%
            select(id,created_at,description,location,starts_with("public_metrics"))
        tweets[[i]] <- flatten_if_not_null(d$includes$tweets) %>%
            select(id,text)
        cat("\014Read", i, "out of", length(files), "files\n")
        i <- i+1
    }
    list(
         main=plyr::rbind.fill(main) %>% tibble(),
         users=plyr::rbind.fill(users) %>% tibble(),
         tweets=plyr::rbind.fill(tweets) %>% tibble()
    ) %>% saveRDS("ngss-sentiment-raw-2021-20-02.rds")
}

collect_data(files=fs)

