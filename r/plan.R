the_plan <-
  drake_plan(
    
    # loading data
    
    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_16.rda")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    
    # joining data
    
    loaded_rda_data_with_state = join_state(loaded_rda_data, state_data),
    
    # preparing for modeling
    
    joined_data = create_new_variables_and_filter_by_language(loaded_rda_data_with_state),
    
    data_to_model = filter_data_by_year(joined_data), # removes 20 cases before 2010
    
    # for RMD output
    
    descriptives = rmarkdown::render(
      knitr_in("describe-data.Rmd"),
      output_file = file_out("docs/descriptives.html"),
      params = list(d = data_to_model)),
    
    binary_descriptives_to_compare_directly_to_commoncore = rmarkdown::render(
      knitr_in("describe-data-with-binary-scale.Rmd"),
      output_file = file_out("docs/descriptives-binary.html"),
      params = list(d = data_to_model)),
    
    final_models = rmarkdown::render(
      knitr_in("models.Rmd"),
      output_file = file_out("docs/models.html"),
      params = list(d = data_to_model)),
  
    # for site
    
    dependencies = rmarkdown::render(
      knitr_in("dependencies.Rmd"),
      output_file = file_out("docs/dependencies.html")),
    
    rendered_site = render_site()
    
  )
