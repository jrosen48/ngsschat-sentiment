the_plan <-
  drake_plan(
    
    # loading data
    
    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_09.rda")),
    liwc_data = read_csv(file_in("data-raw/liwc-results.csv")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    
    # joining data
    
    loaded_rda_data_with_liwc = join_liwc(loaded_rda_data, liwc_data),
    loaded_rda_data_with_liwc_with_state = join_state(loaded_rda_data_with_liwc, state_data),
    
    # preparing for modeling
    
    data_to_model = create_new_variables_and_filter_by_language(loaded_rda_data_with_liwc_with_state),
    
    # for RMD output
    
    descriptives = rmarkdown::render(
      knitr_in("describe-data.Rmd"),
      output_file = file_out("docs/descriptives.html"),
      params = list(d = data_to_model)),
    
    final_models = rmarkdown::render(
      knitr_in("final-models.Rmd"),
      output_file = file_out("docs/final-models.html"),
      params = list(d = data_to_model)),
  
    # for site
    
    dependencies = rmarkdown::render(
      knitr_in("dependencies.Rmd"),
      output_file = file_out("docs/dependencies.html")),
    
    rendered_site = render_site()
    
  )
