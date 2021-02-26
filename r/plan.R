the_plan <-
  drake_plan(
    
    # loading and cleaning data
    
    raw = readRDS(file_in("data-raw/ngss-sentiment-raw-2021-20-02.rds")),
    preproc = raw %>% preproc_master(),
    clean = preproc %>% clean_master(),
    
    # aggregating data and variables for `clean`
    
    export_senti = clean %>% export_for_sentistrength(),
    export_uclass = clean %>% export_for_userclass(),
    in_senti_scales =  read.table(file_in("data-raw/out-sentistrength-scales.txt"), sep="\t", header = T, quote=""),
    in_senti_binary =  read.table(file_in("data-raw/out-sentistrength-binary.txt"), sep="\t", header = T, quote=""),
    in_senti_trinary =  read.table(file_in("data-raw/out-sentistrength-trinary.txt"), sep="\t", header = T, quote=""),
    in_userclass = read.csv(file_in("data-raw/out-userclass.csv"), header=T),
    in_geo = readRDS(file_in("data-raw/user-state-final-2021-02-25.rds")),
    
    with_external = add_external(clean,in_senti_scales,in_senti_binary,in_senti_trinary,in_userclass),
    with_geo = add_external_geo(with_external, in_geo),
    all_vars = with_geo %>% aggregate_variables(),
    additional_geo = all_vars %>% get_more_geo(),
    
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),

    # figure 1

    fig1_path = create_figure_1(additional_geo),

    # joining data

    loaded_rda_data_with_state = join_state(additional_geo, state_data),

    # preparing for modeling

    joined_data = create_new_variables_and_filter_by_language(loaded_rda_data_with_state),

    data_to_model_filtered = filter_data_by_year(joined_data), # removes 20 cases before 2010

    data_to_model_lead_states = add_lead_state_status(data_to_model_filtered, state_data),

    data_to_model = scale_key_vars(data_to_model_lead_states),

    # estimating models

    null_model = lmer(senti_scale_s ~ 1 + (1|state_master) + (1|user_id), data = data_to_model),

    state_ranefs = return_state_ranefs(null_model),

    full_model = lmer(senti_scale_s ~
                        type_of_tweet +
                        time_on_twitter_s +
                        is_teacher +
                        year_of_post_centered +
                        is_teacher:year_of_post_centered +
                        type_of_tweet:year_of_post_centered +
                        has_joined_chat +
                        n_posted_chatsessions_s + n_posted_ngsschat_nonchat_s + n_posted_non_ngsschat_s +
                        adopted_fct +
                        (1|user_id),
                      data = data_to_model),

    augmented_full_model_data = augment(full_model),

    full_model_with_three_way_interact = lmer(senti_scale_s ~
                                                type_of_tweet +
                                                time_on_twitter_s +
                                                is_teacher +
                                                year_of_post_centered +
                                                is_teacher:year_of_post_centered +
                                                type_of_tweet:year_of_post_centered +
                                                is_teacher:type_of_tweet:year_of_post_centered +
                                                has_joined_chat +
                                                n_posted_chatsessions_s + n_posted_ngsschat_nonchat_s + n_posted_non_ngsschat_s +
                                                adopted_fct +
                                                (1|user_id),
                                              data = data_to_model),

    # for RMD output

    # descriptives = rmarkdown::render(
    #   knitr_in("describe-data.Rmd"),
    #   output_file = file_out("docs/descriptives.html"),
    #   params = list(d = data_to_model)),

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
      output_file = file_out("docs/dependencies.html"))

    # rendered_site = target(
    #   command = render_site(),
    #   trigger = trigger(condition = TRUE)
    # ),

    #to open site:
      # browseURL("docs/index.html")
    
  )
