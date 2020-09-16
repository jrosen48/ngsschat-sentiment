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
  
  state_data_adopt <- state_data %>%
    select(State, Early:Not) %>%
    gather(key, val, -State) %>%
    rename(state = State,
           adoption_status = key) %>%
    mutate(state = tolower(state),
           adoption_status = if_else(adoption_status == "Near", "Mid", adoption_status),
           adoption_status = factor(adoption_status, levels = c("Not", "Early", "Mid", "Late"))) %>%
    filter(!is.na(val)) %>%
    select(-val) %>% 
    select(state, adoption_status)
  
  state_data_merged <- state_data_merge %>% 
    left_join(state_data_adopt)
  
  state_data_final <- state_data_merged %>% 
    complete(state, year = 2011:2020) %>% 
    group_by(state) %>% 
    fill(adopted, year_month, adoption_status) %>% 
    fill(adopted, year_month, adoption_status, .direction = "up")
  
  state_data_final <- state_data_final %>% 
    mutate(year_adopted_plus = year_adopted + 1) %>% 
    mutate(year_adopted_minus = year_adopted - 1) %>% 
    fill(year_adopted, year_adopted_minus, year_adopted_plus, .direction = "updown") %>% 
    mutate(near_adoption = ifelse(year == year_adopted, 1, 0),
           near_adoption = ifelse(year == year_adopted_minus, 1, near_adoption),
           near_adoption = ifelse(year == year_adopted_plus, 1, near_adoption)) %>% 
    mutate(before_adoption = ifelse(year < year_adopted_minus, 1, 0)) %>% 
    mutate(after_adoption = ifelse(year > year_adopted_plus, 1, 0)) %>% 
    fill(modified, lead, .direction = 'updown') %>% 
    ungroup()
  
  state_data_final <- state_data_final %>% 
    mutate(no_adoption = ifelse(adoption_status == "Not", 1, 0)) %>% 
    select(state, year, contains("_adoption"), modified, lead)
  
  dstatus <- state_data_final %>% 
    select(contains("_adoption")) %>% 
    gather(key, val) %>% 
    mutate(val = ifelse(is.na(val), 0, val)) %>% 
    filter(val == 1) %>% 
    select(-val) %>% 
    rename(adoption_key = key)
  
  state_data_final$adoption_key <- factor(dstatus$adoption_key) %>% 
    forcats::fct_relevel("no_adoption")
  
  state_data_final <- rename(state_data_final, state_master = state)
  
  ## ---- joining-state-data------------------------------------------------------------------------------------------------
  state_data_final <- state_data_final %>% 
    rename(year_of_post = year)
  
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
  
  d <- d %>% 
    mutate(adoption_key = adoption_key %>% forcats::fct_explicit_na())
  
  d %>% 
    as_tibble()
  
}

model_null_model <- function(d, dependent_variable_string) {
  
  d <- d %>% 
    rename(dependent_variable = all_of(dependent_variable_string)) # probably better to use NSE in this function, but this seems to work
  
  print(str_c("running lme4::lmer() with ", dependent_variable_string, " as the dependent variable and the full random effects structure with no fixed effects"))
  
  m <- lmer(dependent_variable ~ 
              
              #(1|chat_id_fct) + 
              
              (1|state_master) +
              
              (1|screen_name), 
            
            data = d)
  
  m
  
}

model_full_model <- function(d, dependent_variable_string) {
  
  d <- d %>% 
    rename(dependent_variable = all_of(dependent_variable_string)) # probably better to use NSE in this function, but this seems to work
  
  print(str_c("running lme4::lmer() with ", dependent_variable_string, " as the dependent variable and the full set of independent variables"))
  
  m <- lmer(dependent_variable ~ 
              
              type_of_tweet + # NGSSchat - chat, #NGSSChat non-chat, non-#NGSSchat (includes e.g. NGSS)
              adoption_key + # status of an individual's state regarding when they adopted the NGSS
              
              scale(time_on_twitter_period) + # for how long a person has been on Twitter
              isTeacher + # participant is a teacher or not
              
              year_of_post_centered + 
              
              # scale(favorite_count) + scale(retweet_count) + scale(reply_count) + # tweet-level variables
              
              hasJoinedChat + 
              
              # postedNGSSchat + postedChatSession + hasJoinedChat + scale(total_n_chats) + # user-level variables
              
              # scale(n_posted_chatsessions) + scale(n_posted_ngsschat_nonchat) + scale(n_posted_non_ngsschat) + # also user-level variables; should these be time-varying?
              
              (1|screen_name), 
            
            data = d)
  
  print("estimation complete; saving and then returning output")
  
  write_rds(m, str_c("out/", dependent_variable_string))
  
  m
  
}
