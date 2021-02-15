load_rda <- function(f) {
  load(f)
  tweets_dl
}

render_site <- function() {
  file.rename(from = "graph.html", to = "docs/graph.html")
  fs::dir_delete("graph_files")
  rmarkdown::render_site("docs")
}

join_state <- function(d, state_data) {
  
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
    select(state_master = state, year_of_post = year, adopted)
  
  d <- d %>% 
    left_join(state_data_final, by = c("state_master", "year_of_post"))
  
  d
  
}

create_new_variables_and_filter_by_language <- function(d) {
  
  # creating factor for chat_id
  d <- d %>% 
    mutate(chat_id_fct = as.factor(chat_id)) %>%
    mutate(chat_id_fct = forcats::fct_explicit_na(chat_id_fct))
  
  d$year_fct <- factor(d$year_of_post) %>% forcats::fct_relevel('2016')
  
  d <- d %>% 
    mutate(type_of_tweet = ifelse(isChat == 0, "ngsschat-non-chat",
                                  ifelse(isChat == 1, "ngsschat-chat", NA))) %>% 
    mutate(type_of_tweet = ifelse(is.na(type_of_tweet), "non-ngsschat", type_of_tweet))
  
  d <- d %>% 
    mutate(year_centered = scale(year_of_post, scale = FALSE),
           time_on_twitter = (d$created_at - d$account_created_at) / (60 * 60 * 24) / 365,
           year_fct = factor(as.numeric(as.character(d$year_fct))))
  
  d <- d %>% 
    mutate(year_of_post_centered = year_of_post - 2016)
  
  d <- d %>% filter(lang == "en")
  
  d <- d %>% mutate(type_of_tweet = forcats::fct_relevel(type_of_tweet, "non-ngsschat"))
  
  d <- d %>% 
    mutate(adopted_fct = ifelse(is.na(adopted), "missing", 
                                ifelse(adopted == 1, "adopted", "not-adopted"))) %>% 
    mutate(adopted_fct = forcats::fct_relevel(adopted_fct, "not-adopted"))
  
  d %>% 
    as_tibble()
  
  d <- d[-536375, ] # performance::check_outliers(m_rs) revealed this to be an outlier; inspection of it confirms
  
  d
  
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
    mutate(time_on_twitter_s = as.numeric(scale(time_on_twitter)),
           n_posted_chatsessions_s = as.numeric(scale(n_posted_chatsessions)),
           n_posted_ngsschat_nonchat_s = as.numeric(scale(n_posted_ngsschat_nonchat)),
           n_posted_non_ngsschat_s = as.numeric(scale(n_posted_non_ngsschat)),
           senti_scale_s = as.numeric(scale(senti_scale, center = FALSE)))
}

create_figure_1 <- function(d) {
  
  d$dates <- d$created_at %>% lubridate::date()
  
  # Recode q
  
  d$q <- d$q %>% 
    recode(
      next_generation_science_standards = "non-ngsschat",
      next_generation_science_standard = "non-ngsschat",
      next_gen_science_standards = "non-ngsschat",
      next_gen_science_standard = "non-ngsschat",
      ngss = "non-ngsschat"
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
  
  d_all <- d_all[year(d_all$day) >= 2012,]
  
  # Cap dates at last download for all categories for unbiased representation
  
  maxd <- d %>% group_by(q) %>% summarise(last_dl=min(dl_at)) %>% 
    pull(last_dl) %>% min() %>% lubridate::date()
  
  d_all <- d_all[d_all$day <= maxd,]
  
  # Plot by week
  
  d_all <- d_all %>%
    mutate(week = lubridate::floor_date(day, "week")) %>%
    group_by(week, Category) %>% 
    summarise(value=sum(value))
  
  loadfonts(device="win") # edit depending on OS
  
  d_all$Category <- factor(d_all$Category, levels=c("Chat", "Non-chat", "Non-#NGSSchat"))
  
  filename <- "fig1.png"
  
  png(filename, width = 480*4, height = 480*2)
  d_all %>% 
    ggplot(aes(x = week, y = value)) + 
    geom_col(aes(fill = Category), width=2) + 
    scale_y_continuous(limits=c(0, 4000), oob = scales::squish) +
    xlab("Week") + 
    ylab("Number of Tweets") + 
    theme_bw() + 
    theme(legend.position="top") + 
    scale_fill_manual(values = c("#426600", "#00998F", "#990000")) + 
    theme(text=element_text(family="Times New Roman", size=36)) 
  dev.off()
  
  return(filename) 

}
