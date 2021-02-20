the_plan <-
  drake_plan(
    
    # loading data
    
    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_16.rda")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    
    # figure 1
    
    fig1_path = create_figure_1(loaded_rda_data),
    
    # joining data
    
    loaded_rda_data_with_state = join_state(loaded_rda_data, state_data),
    
    # preparing for modeling
    
    joined_data = create_new_variables_and_filter_by_language(loaded_rda_data_with_state),
    
    data_to_model_filtered = filter_data_by_year(joined_data), # removes 20 cases before 2010
    
    data_to_model_lead_states = add_lead_state_status(data_to_model_filtered, state_data),
    
    data_to_model = scale_key_vars(data_to_model_lead_states),
    
    # estimating models
    
    null_model = lmer(senti_scale_s ~ 1 + (1|state_master) + (1|screen_name), data = data_to_model),
    
    state_ranefs = return_state_ranefs(null_model),
    
    full_model = lmer(senti_scale_s ~
                        type_of_tweet +
                        time_on_twitter_s +
                        isTeacher +
                        year_of_post_centered +
                        isTeacher:year_of_post_centered +
                        type_of_tweet:year_of_post_centered +
                        hasJoinedChat +
                        n_posted_chatsessions_s + n_posted_ngsschat_nonchat_s + n_posted_non_ngsschat_s +
                        adopted_fct +
                        (1|screen_name),
                      data = data_to_model),
    
    augmented_full_model_data = augment(full_model),
    
    full_model_with_three_way_interact = lmer(senti_scale_s ~
                                                type_of_tweet +
                                                time_on_twitter_s +
                                                isTeacher +
                                                year_of_post_centered +
                                                isTeacher:year_of_post_centered +
                                                type_of_tweet:year_of_post_centered +
                                                isTeacher:type_of_tweet:year_of_post_centered +
                                                hasJoinedChat +
                                                n_posted_chatsessions_s + n_posted_ngsschat_nonchat_s + n_posted_non_ngsschat_s +
                                                adopted_fct +
                                                (1|screen_name),
                                              data = data_to_model),
    
    # for RMD output
    
    descriptives = rmarkdown::render(
      knitr_in("describe-data.Rmd"),
      output_file = file_out("docs/descriptives.html"),
      params = list(d = data_to_model)),
    
    binary_descriptives_to_compare_directly_to_commoncore = rmarkdown::render(
      knitr_in("describe-data-with-binary-scale.Rmd"),
      output_file = file_out("docs/descriptives-binary.html"),
      params = list(d = data_to_model)),
    
    models = rmarkdown::render(
      knitr_in("models.Rmd"),
      output_file = file_out("docs/models.html"),
      params = list(null_model = null_model,
                    full_model = full_model,
                    full_model_with_three_way_interact = full_model_with_three_way_interact,
                    augmented_full_model_data = augmented_full_model_data,
                    state_ranefs = state_ranefs)),
    
    # for site
    
    dependencies = rmarkdown::render(
      knitr_in("dependencies.Rmd"),
      output_file = file_out("docs/dependencies.html")),
    
    rendered_site = target(
      command = render_site(),
      trigger = trigger(condition = TRUE)
    ),
    
    # to open site: 
    #   browseURL("docs/index.html")
    
  )
