
rename_vars <- function(d) {
  d <- map(d, janitor::clean_names)
  d$main <- d$main %>% rename(status_id=id,user_id=author_id)
  d$users <- d$users  %>% rename(user_id=id,user_created_at=created_at)
  d$tweets <- d$tweets %>% rename(ref_id=id, ref_text=text)
  d <- map(d, function(x) {names(x)<-names(x)%>%str_remove_all("public_metrics_");return(x)})
  return(d)
}

drop_duplicates <- function(d) {
  d$main <- d$main %>% distinct(status_id, .keep_all=T)
  d$users <- d$users %>% distinct(user_id, .keep_all=T)
  d$tweets <- d$tweets %>% distinct(ref_id, .keep_all=T) 
  return(d)
} 

prepare_join <- function(d) {
  d$main$is_retweet <- map(d$main$referenced_tweets, function(x) return(isTRUE(x$type=="retweeted"))) %>% unlist()
  d$main$is_quote <- map(d$main$referenced_tweets, function(x) return(isTRUE(x$type=="quoted"))) %>% unlist()
  d$main$is_reply <- map(d$main$referenced_tweets, function(x) return(isTRUE(x$type=="replied_to"))) %>% unlist()
  d$main$ref_id <- map(d$main$referenced_tweets, function(x) if(is.null(x)) return(NA) else return(head(x$id,1))) %>% unlist()
  d$main <- d$main %>% select(-referenced_tweets)    
  return(d)
}


join_tables <- function(d) { 
  return(
    d$main %>%
      left_join(d$tweets,by="ref_id") %>%
      left_join(d$users,by="user_id")
  )
}

no_duplicates <- function(d) {
  return(
    d %>% 
      distinct(status_id, .keep_all=T)
  )
}

preproc_master <- function(d) {
  d %>%
    rename_vars() %>%
    drop_duplicates() %>%
    prepare_join() %>%
    join_tables() %>% 
    no_duplicates()
}

remove_langs <- function(d) {
  return(
    d %>% 
      filter(lang %in% c("en", "und"))
  )
}

get_full_text <- function(d) {
  d$text[d$is_retweet==T] <- d$ref_text[d$is_retweet==T] # retweet text abbreviated @ main
  return(d %>% select(-starts_with("ref")))
}

clean_timevars <- function(d) {
  d$created_at <- d$created_at %>% 
    str_replace("T", " ") %>% 
    str_remove("\\.000Z$") %>% 
    as.POSIXct(tz="UTC", format="%Y-%m-%d %H:%M:%OS")
  d$user_created_at <- d$user_created_at %>% 
    str_replace("T", " ") %>% 
    str_remove("\\.000Z$") %>% 
    as.POSIXct(tz="UTC", format="%Y-%m-%d %H:%M:%OS")
  return(d)
}

clean_master <- function(d) {
  return(
    d %>% 
      remove_langs() %>% 
      get_full_text() %>% 
      clean_timevars()
  )
}

export_for_sentistrength <- function(d) {
  f <- "data-raw/in-sentistrength.txt"
  write.table(gsub("[\r\n]", "", d$text), f, row.names = F, col.names = F)
  return(f)
}

export_for_userclass <- function(d) {
  f <- "data-raw/in-userclass.csv"
  write.table(gsub("[\r\n]", "", d$description), f, row.names = F, col.names = F)
  return(f)
}

add_external <- function(d, scales, binary, trinary, userclass) {
  d$ss_pos <- scales$Positive; d$ss_neg <- scales$Negative
  d$ss_binary <- binary$Overall; d$ss_trinary <- trinary$Overall
  d$is_teacher <- userclass$prediction_by_keywords
  return(d)
}

add_external_geo <- function(d, geo) {
  return(
    d %>% left_join(geo, by="user_id")
  )
}

add_context <- function(d) {
  d$has_ngsschat <- grepl("\\#\\bngsschat\\b", d$text, ignore.case=T)
  d$has_ngss <- grepl("\\bngss\\b", d$text, ignore.case=T)
  d$context <- NA
  d$context[d$has_ngsschat==T] <- "#NGSSchat"
  d$context[d$has_ngsschat==F & d$has_ngss==T] <- "ngss"
  d$context[d$has_ngsschat==F & d$has_ngss==F] <- "other"
  return(d %>% select(-has_ngsschat, -has_ngss))
}

add_is_chat <- function(d) {
  d2 <- d[d$context == "#NGSSchat",] %>% 
    select(c("status_id", "created_at", "text"))
  
  time <- d2$created_at
  time <- format(time, format="%Y-%m-%d %H")
  
  freq <- table(time) 
  freq <- sort(freq, decreasing = T)
  freq <- freq %>% head(1000) 
  
  hours <- names(freq) # Select 1,000 most busy hours in data set
  
  # Select tweets at the beginning of these 1,000 hours (+- 5 minutes around edge)
  
  hours <- hours %>%  # format to hour
    sapply(paste, ":00:00 UTC", sep="") %>% 
    as.character() %>% 
    as.POSIXct(tz = "UTC")
  
  # Add minutes around edges of beginning of hours
  
  minutes <- hours
  
  for (i in seq(60, 5*60, 60)){
    minutes <- c(minutes, hours-i) # seconds are added and substracted in steps of 60
    minutes <- c(minutes, hours+i)
  }
  
  # Standardize tweet posting times to minutes in order to obtain tweets around beginning of must busy hours
  
  post_minutes <- d2$created_at %>%
    round_date(unit="1 minute")
  
  possible_chat_openings <- d2[which(post_minutes %in% minutes),]
  
  # Grab tweets with "Welcome to "NGSSchat" and related terms, openings lines have been manually looked up before
  
  ind <- grep("Welcome to #NGSSchat", possible_chat_openings$text)
  ind <- c(ind, grep("Welcome to the first session of #NGSSchat", possible_chat_openings$text))
  ind <- c(ind, grep("Our #nhed guest moderator for this evening is", possible_chat_openings$text))
  ind <- c(ind, grep("Excited to learn and connect with my #NGSSchat community-- Join us-- happening", possible_chat_openings$text))
  
  ind <- unique(ind)
  
  # Sort out which specific hours are chats based on ind
  
  hours_with_opening_lines <- possible_chat_openings$created_at[ind] %>%
    format(format="%Y-%m-%d %H") %>%
    sapply(paste, ":00:00 UTC", sep="") %>%
    as.character() %>%
    as.POSIXct(tz = "UTC")
  
  chat_hours <- hours[which(hours %in% hours_with_opening_lines)]  # declare busiest hours with opening lines as chat hours
  
  # Create Variable "isChat", strict definition of chat as a 1 hour timeframe
  
  time <- d2$created_at %>%
    format(format="%Y-%m-%d %H")
  
  hours <- time %>% 
    sapply(paste, ":00:00 UTC", sep="") %>%
    as.character() %>%
    as.POSIXct(tz = "UTC")
  
  ind <- which(hours %in% chat_hours)
  
  isChat <- rep(0, nrow(d2))
  isChat[ind] <- 1  # 1 if tweets is in chat session
  
  d2$isChat <- isChat
  
  # Knit back together with full data frame
  
  d$is_chat <- rep(NA, nrow(d))
  d$is_chat[d$status_id %in% d2$status_id] <- d2$isChat
  d$is_chat <- d$is_chat %>% as.factor()
  
  return(d)
}

add_time_on_twitter <- function(d) {
  return(
    d %>% 
      mutate(time_on_twitter_seconds = created_at - user_created_at)
  )
}

add_year_centered <- function(d) {
  return(
    d %>% 
      mutate(year_of_post_centered = year(created_at) - 2016)
  )
}

add_type_of_tweet <- function(d) {
  return(
    d %>% 
      mutate(type_of_tweet = ifelse(is_chat==1,"ngsschat-chat",ifelse(is_chat==0,"ngsschat-non-chat",NA))) %>% 
      mutate(type_of_tweet = ifelse(is.na(type_of_tweet),"non-ngsschat",type_of_tweet))
  )
}

add_senti_scale <- function(d) {
  return(
    d %>% 
      mutate(ss_scale = ss_pos + ss_neg)
  )
}

add_has_joined_chat <- function(d) {
  d %>% 
    arrange(created_at) %>% 
    group_by(user_id, is_chat) %>% 
    summarise(joined=first(created_at)) %>% 
    filter(is_chat==1) %>% 
    select(-is_chat) -> reference
  d <- d %>% 
    left_join(reference, by="user_id") %>% 
    mutate(has_joined_chat = ifelse(created_at>=joined,1,0)) %>% 
    mutate(has_joined_chat = ifelse(is.na(has_joined_chat),0,has_joined_chat)) %>% 
    select(-joined)
  return(d)
}

add_n_posted_vars <- function(d) {
  d %>% 
    group_by(user_id, context) %>% 
    summarise(n_tweets=n()) %>% 
    ungroup() %>% 
    pivot_wider(names_from=context, values_from=n_tweets) %>% 
    rename(n_posted_ngss_nonchat=ngss,n_posted_chatsessions=`#NGSSchat`,
           n_posted_non_ngsschat=other) -> reference
  return(
    d %>% 
      left_join(reference, by="user_id") %>% 
      mutate(across(starts_with("n_posted"),  ~replace(., is.na(.), 0)))
  )
}

aggregate_variables <- function(d){
  return(
    d %>% 
      add_context() %>% 
      add_is_chat() %>% 
      add_time_on_twitter() %>% 
      add_year_centered() %>% 
      add_type_of_tweet() %>% 
      add_senti_scale() %>% 
      add_has_joined_chat() %>% 
      add_n_posted_vars()
  )
}

rgx_build <- function(words){
  searchfor <- paste(words, sep="", collapse="\\>|\\<")
  searchfor <- paste(c("\\<", searchfor, "\\>"), sep="", collapse="")
  return(searchfor)
}

get_more_geo <- function(d) {
  acros <- read.table(header=T, sep=";", text=
                        "acronym;state
  AK;Alaska
  AL;Alabama
  AR;Arkansas
  AZ;Arizona
  CA;California
  CO;Colorado
  CT;Connecticut
  DC;District of Columbia
  DE;Delaware
  FL;Florida
  GA;Georgia
  HI;Hawaii
  IA;Iowa
  ID;Idaho
  IL;Illinois
  IN;Indiana
  KS;Kansas
  KY;Kentucky
  LA;Louisiana
  MA;Massachusetts
  MD;Maryland
  ME;Maine
  MI;Michigan
  MN;Minnesota
  MO;Missouri
  MS;Mississippi
  MT;Montana
  NC;North Carolina
  ND;North Dakota
  NE;Nebraska
  NH;New Hampshire
  NJ;New Jersey
  NM;New Mexico
  NV;Nevada
  NY;New York
  OH;Ohio
  OK;Oklahoma
  OR;Oregon
  PA;Pennsylvania
  PR;Puerto Rico
  RI;Rhode Island
  SC;South Carolina
  SD;South Dakota
  TN;Tennessee
  TX;Texas
  UT;Utah
  VA;Virginia
  VT;Vermont
  WA;Washington
  WI;Wisconsin
  WV;West Virginia
  WY;Wyoming")
  
  acros$acronym <- acros$acronym %>% trimws()
  acros$state <- tolower(acros$state)
  acros$state <- acros$state %>% str_remove("district of ") %>% trimws()
  
  # By introduction
  
  ngsschat <- d[d$context == "#NGSSchat",]
  
  ind <- which(ngsschat$is_chat == 1)
  ind <- ind[ngsschat$is_chat[ind-1] != 1]  # starting points of chats
  
  for (element in ind){
    ind <- c(ind, seq(element-50, element+150))
  }
  
  ind <- sort(unique(ind))
  ind <- ind[ind >= 0]
  
  possible_cases <- ngsschat[ind,]
  
  possible_cases$text <- possible_cases$text %>%
    tolower() %>% 
    str_remove_all("[[:punct:]]")
  
  result <- list()
  
  for (i in 1:nrow(acros)){
    raw <- c(
      possible_cases$user_id[grep(rgx_build(tolower(acros$acronym[i])), possible_cases$text)],
      possible_cases$user_id[grep(rgx_build(tolower(acros$state[i])), possible_cases$text)]
    ) %>% unique()
    result[[acros$state[i]]] <- list(raw)[[1]]
    cat("\014 Searching for states in chat openings... Iteration", i, "/ 52 done.\n")
  }
  
  reshape2::melt(result) %>% 
    tibble() %>% 
    rename(user_id=value, state=L1) %>% 
    group_by(user_id) %>% 
    filter(n() == 1) %>% 
    ungroup() -> for_join
  
  already_matched <- d$user_id[!is.na(d$state)]
  for_join <- for_join[!(for_join$user_id %in% already_matched),]
  for_join$state <- for_join$state %>% toupper()
  
  d <- d %>% left_join(for_join, by="user_id") 
  d$state <- coalesce(d$state.x, d$state.y)
  d <- d %>% select(-state.x, -state.y)
  
  # By all tweets
  
  possible_cases <- d %>% filter(is.na(state))
  
  possible_cases$text <- possible_cases$text %>%
    tolower() %>% 
    str_remove_all("[[:punct:]]")
  
  result <- list()
  
  for (i in 1:nrow(acros)){
    raw <- c(
      possible_cases$user_id[grep(rgx_build(tolower(acros$acronym[i])), possible_cases$text)],
      possible_cases$user_id[grep(rgx_build(tolower(acros$state[i])), possible_cases$text)]
    ) %>% unique()
    result[[acros$state[i]]] <- list(raw)[[1]]
    cat("\014 Searching for states in remaining tweets... Iteration", i, "/ 52 done.\n")
  }
  
  reshape2::melt(result) %>% 
    tibble() %>% 
    rename(user_id=value, state=L1) %>% 
    group_by(user_id) %>% 
    filter(n() == 1) %>% 
    ungroup() -> for_join
  
  already_matched <- d$user_id[!is.na(d$state)]
  for_join <- for_join[!(for_join$user_id %in% already_matched),]
  for_join$state <- for_join$state %>% toupper()
  
  d <- d %>% left_join(for_join, by="user_id") 
  d$state <- coalesce(d$state.x, d$state.y)
  d <- d %>% select(-state.x, -state.y)
  
  return(d)
}


###########################

load_rda <- function(f) {
  load(f)
  tweets_dl
}

render_site <- function() {
  file.rename(from = "./graph.html", to = "docs/graph.html")
  #fs::dir_delete("graph_files")
  rmarkdown::render_site("docs")
}

join_state <- function(d, state_data) {
  
  # state_data <- state_data %>% 
  #   mutate(adoption_status = case_when(
  #     adopted == 1 & lead == "Yes" ~ "adopted-lead",
  #     adopted == 1 & lead != "Yes" ~ "adapted",
  #     adopted == 0 ~ "did-not-adopt",
  #     TRUE ~ NA))
  
  ## ---- proc-state-data---------------------------------------------------------------------------------------------------
  state_data$month <- state_data$date_month %>% 
    str_split("\\-") %>% 
    purrr::map(~.[[1]]) %>% as.integer()
  
  state_data$year_month <- as.numeric(str_c(state_data$date_year, ".", state_data$month))
  
  state_data_merge <- state_data %>% 
    rename(state = State) %>% 
    mutate(state = tolower(state)) %>% 
    select(state, year_month, adopted = Not, year = date_year, modified, lead) %>% 
    mutate(adopted = if_else(is.na(adopted), 1, 0)) %>% 
    mutate(year_adopted = year) %>% 
    mutate(modified = ifelse(is.na(modified), 0, modified)) %>% 
    mutate(lead = ifelse(lead == "Yes", 1, lead),
           lead = ifelse(is.na(lead), 0, lead))
  
  state_data_final <- state_data_merge %>% 
    complete(state, year = 2011:2020) %>% 
    group_by(state) %>% 
    fill(adopted, year_month) %>% 
    fill(adopted, year_month, .direction = "up")
  
  state_data_final <- state_data_final %>% 
    mutate(year_adopted_plus = year_adopted + 1) %>% 
    mutate(year_adopted_minus = year_adopted - 1) %>% 
    fill(year_adopted, year_adopted_minus, year_adopted_plus, .direction = "updown") %>% 
    mutate(near_adoption = ifelse(year == year_adopted, 1, 0),
           near_adoption = ifelse(year == year_adopted_minus, 1, near_adoption),
           near_adoption = ifelse(year == year_adopted_plus, 1, near_adoption)) %>% 
    mutate(before_adoption = ifelse(year < year_adopted_minus, 1, 0)) %>% 
    mutate(after_adoption = ifelse(year > year_adopted_plus, 1, 0)) %>% 
    fill(modified, lead, adopted, .direction = 'updown') %>% 
    ungroup()
  
  state_data_final <- state_data_final %>% 
    mutate(adopted = ifelse(year <= year_adopted, 0, 1)) %>% 
    select(state, year_of_post = year, adopted) %>% 
    mutate(state=toupper(state))
  
  d$year_of_post <- lubridate::year(d$created_at)
  
  d <- d %>% 
    left_join(state_data_final, by = c("state", "year_of_post"))
  
  d
  
}

create_new_variables_and_filter_by_language <- function(d) {
  
  # Get hashmap of all chats that holds all status IDs
  # for each chat (key) as value (status IDs)
  
  ngsschat <- d[d$context == "#NGSSchat",]
  
  ind <- which(ngsschat$is_chat == 1)
  ind_start <- ind[ngsschat$is_chat[ind-1] != 1]  # starting points of chats
  ind_end <- ind[ngsschat$is_chat[ind+1] != 1]  # end points of chats
  
  # Mark all indexes in between to corresponding chats, values become status IDs
  # and not $row because $row was calculated over all $q queries
  
  h_chats <- hash()   # h_chats[["1"]] gives statues of first chat session 
  
  for (i in 1:length(ind_start)){
    chat_indexes <- (ind_start[i]):(ind_end[i])
    h_chats[[as.character(i)]] <- ngsschat[chat_indexes,]$status_id
  }
  
  # Create variables:
  
  d$chat_id <- rep(NA, nrow(d))
  
  i <- 1
  for (key in keys(h_chats)){
    d$chat_id[which(d$status_id %in% h_chats[[key]])] <- key
    cat("\014 Hash chat index iteration", i, "out of", length(keys(h_chats)), "complete\n")
    i <- i+1
  }
  
  d$chat_id <- d$chat_id %>% as.numeric()
  
  # creating factor for chat_id
  d <- d %>% 
    mutate(chat_id_fct = as.factor(chat_id)) %>%
    mutate(chat_id_fct = forcats::fct_explicit_na(chat_id_fct))
  
  d$year_fct <- factor(d$year_of_post) %>% forcats::fct_relevel('2016')
  
  d <- d %>% 
    mutate(type_of_tweet = ifelse(is_chat == 0, "ngsschat-non-chat",
                                  ifelse(is_chat == 1, "ngsschat-chat", NA))) %>% 
    mutate(type_of_tweet = ifelse(is.na(type_of_tweet), "non-ngsschat", type_of_tweet))
  
  d <- d %>% 
    mutate(year_centered = scale(year_of_post, scale = FALSE),
           time_on_twitter = (d$created_at - d$user_created_at) / (60 * 60 * 24) / 365,
           year_fct = factor(as.numeric(as.character(d$year_fct))))
  
  d <- d %>% 
    mutate(year_of_post_centered = year_of_post - 2016)
  
  d <- d %>% filter(lang == "en")
  
  d <- d %>% mutate(type_of_tweet = forcats::fct_relevel(type_of_tweet, "non-ngsschat"))
  
  d <- d %>% 
    mutate(adopted_fct = ifelse(is.na(adopted), "missing", 
                                ifelse(adopted == 1, "adopted", "not-adopted"))) %>% 
    mutate(adopted_fct = forcats::fct_relevel(adopted_fct, "not-adopted"))
  
  #d %>% 
  #  as_tibble()
  
  #d <- d[-536375, ] # performance::check_outliers(m_rs) revealed this to be an outlier; inspection of it confirms
  
  d
  
}

add_lead_state_status <- function(d, s) {
  
  state_data_merge <- s %>% 
    rename(state = State) %>% 
    mutate(state = tolower(state)) %>% 
    select(state, lead)
  
  d %>%
    left_join(state_data_merge, by = "state") %>% 
    mutate(adopted_chr = as.character(adopted_fct)) %>% 
    mutate(adopted_chr = 
             case_when(
               adopted_chr == "adopted" & lead == "Yes" ~ "adopted-lead",
               TRUE ~ adopted_chr
             )
    ) %>% 
    mutate(adopted_chr = as.factor(adopted_chr)) %>% 
    mutate(adopted_fct = forcats::fct_relevel(adopted_fct, "not-adopted")) %>% 
    mutate(state = tolower(state),
           state = tools::toTitleCase(state)) %>% 
    rename(state_master = state)
  
}

filter_data_by_year <- function(d) {
  d %>% 
    filter(year_of_post >= 2010)
}

return_state_ranefs <- function(m) {
  broom.mixed::tidy(m, effects = "ran_vals") %>% 
    filter(group == "state_master")
}

scale_key_vars <- function(d) {
  
  d %>% 
    rename(n_posted_ngsschat_nonchat = n_posted_ngss_nonchat) %>% 
    mutate(time_on_twitter_s = as.numeric(scale(as.numeric(time_on_twitter_seconds))),
           n_posted_chatsessions_s = as.numeric(scale(n_posted_chatsessions)),
           n_posted_ngsschat_nonchat_s = as.numeric(scale(n_posted_ngsschat_nonchat)),
           n_posted_non_ngsschat_s = as.numeric(scale(n_posted_non_ngsschat)),
           senti_scale_s = ss_scale / sd(ss_scale))
}

create_figure_1 <- function(d) {
  
  d$dates <- d$created_at %>% lubridate::date()
  
  d <- d %>% rename(q = context, isChat = is_chat) # old code names
  # Recode q
  
  d$q <- d$q %>% 
    recode(
      next_generation_science_standards = "non-ngsschat",
      next_generation_science_standard = "non-ngsschat",
      next_gen_science_standards = "non-ngsschat",
      next_gen_science_standard = "non-ngsschat",
      ngss = "non-ngsschat",
      other = "non-ngsschat"
    )
  
  d$q[which(d$isChat == 1 & d$q == "#NGSSchat")] <- "ngsschat-inside"
  d$q[which(d$isChat == 0 & d$q == "#NGSSchat")] <- "ngsschat-outside"
  
  # Aggregate tweet count over days
  
  d_days <- d %>% 
    group_by(dates, q) %>% 
    summarise(n_tweets = n()) %>% 
    pivot_wider(names_from="q", values_from="n_tweets", values_fill=0)
  
  # Fill in missing days and code with 0
  
  ts <- seq.POSIXt(as.POSIXlt(min(d$dates)), as.POSIXlt(max(d$dates)), by="day")
  ts <- ts %>% lubridate::date()
  df <- data.frame(dates=ts)
  
  d_all <- full_join(df, d_days) %>% replace(is.na(.), 0)
  names(d_all) <- c("day", "Non-#NGSSchat", "Non-chat", "Chat")
  
  d_all <- d_all %>% pivot_longer(!day, names_to="Category")
  
  # Cut off dates 2008-2012 for better readability
  
  d_all <- d_all[lubridate::year(d_all$day) >= 2012,]
  
  # Plot by week
  
  d_all <- d_all %>%
    mutate(week = lubridate::floor_date(day, "week")) %>%
    group_by(week, Category) %>% 
    summarise(value=sum(value))
  
  switch(Sys.info()[['sysname']],
         Windows= {loadfonts(device = "win")},
         Linux  = {loadfonts(device = "pdf")},
         Darwin = {loadfonts(device = "pdf")}
  )
  
  d_all$Category <- factor(d_all$Category, levels=c("Chat", "Non-chat", "Non-#NGSSchat"))
  
  filename <- "fig1.png"
  
  month_data <- d_all %>% 
    ungroup() %>% 
    mutate(month = lubridate::month(week),
           year = lubridate::year(week),
           xaxis_labs = lubridate::make_date(month = month, year = year),
           xaxis = as.numeric(xaxis_labs),
           Category = factor(Category, 
                             levels = c("Non-#NGSSchat", "Non-chat", "Chat"),
                             labels = c("Non-#NGSSchat", "#NGSSchat non-chat", "#NGSSchat"))) %>% 
    count(Category, xaxis_labs, xaxis, wt = value) 
    
  quarters <- unique(round_date(as_date(month_data$xaxis), "halfyear"))
  quarter_breaks <- as.numeric(quarters)
  quarter_labels <- format(quarters, "%m-%y")
  
  label_locs <- month_data %>% 
    group_by(Category) %>% 
    filter(xaxis_labs == max(xaxis_labs)) %>% 
    ungroup()
  
  theme_set(theme_minimal(15))
  
  ggplot(month_data, aes(x = xaxis, y = n)) + 
    geom_line(aes(color = Category), size = 1.2) +
    geom_hline(yintercept = 0, size = 1.5) +
    annotate("rect",
             xmin = 18600,
             xmax = Inf,
             ymin = -Inf,
             ymax = Inf,
             color = "#ffffff",
             fill = "#ffffff") +
    scale_y_continuous(
      name = "Total number of tweets",
      limits = c(0, 17000), 
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      name = "Month (aggregated)",
      breaks = quarter_breaks,
      labels = quarter_labels,
      expand = expansion(mult = c(0, .3))
    ) +
    scale_color_manual("", values = c("#373B41", "#67E0CF", "#ABCFED")) +
    theme(legend.position = c(0.89, 0.12),
          text = element_text(family = "Times New Roman"),
          axis.text.x = element_text(size = 9),
          panel.grid.major.x = element_line(color = "gray95", size = 0.5),
          panel.grid.major.y = element_line(color = "gray95", size = 0.5),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())
  
  ggsave(filename, width = 9.5, height = 4, dpi = 800)
  
  return(filename) 

}
