the_plan <-
  drake_plan(

    # loading data
    
    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_09.rda")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    
    # for RMD output
    
    prepared_data = rmarkdown::render(
      knitr_in("prepare-for-modeling.Rmd"),
      output_file = file_out("docs/prepare-for-modeling.html"),
      params = list(tweets_dl = loaded_rda_data,
                    state_data = state_data)),
    
    loaded_processed_data = read_rds(file_in("data/ngsschat-data-for-modeling.rds")),
    
    descriptives = rmarkdown::render(
      knitr_in("describe-data.Rmd"),
      output_file = file_out("docs/descriptives.html"),
      params = list(d = loaded_processed_data)),
    
    fitted_models = rmarkdown::render(
      knitr_in("model-sentiment.Rmd"),
      output_file = file_out("docs/models.html"),
      params = list(d = loaded_processed_data)),
    
    fitted_models_no_state = rmarkdown::render(
      knitr_in("model-sentiment-no-state.Rmd"),
      output_file = file_out("docs/models-without-states.html"),
      params = list(d = loaded_processed_data)),
    
    # for site
    
    dependencies = rmarkdown::render(
      knitr_in("dependencies.Rmd"),
      output_file = file_out("docs/dependencies.html")),
    
    rendered_site = render_site()
    
)
