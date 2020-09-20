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
    mutate(year_of_post_centered = scale(year_of_post, scale = FALSE))
  
  d <- d %>% filter(lang == "en")
  
  d %>% 
    as_tibble()
  
}

filter_data_by_year(d) {
  d %>% 
    filter(year_of_post >= 2010)
}
