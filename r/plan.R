the_plan <-
  drake_plan(

    # loading data
    
    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_04.rda")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    state_acronyms = read_csv(file_in("data-raw/state-acronyms.csv")),
    
    # for RMD output
    
    prepared_data = rmarkdown::render(
      knitr_in("prepare-for-modeling.Rmd"),
      output_file = file_out("docs/prepare-for-modeling.html"),
      params = list(tweets_dl = loaded_rda_data,
                    state_data = state_data,
                    state_acronyms = state_acronyms)),
    
    loaded_processed_data = read_rds(file_in("data/ngsschat-data-for-modeling.rds")),
    
    fitted_models = rmarkdown::render(
      knitr_in("model-sentiment.Rmd"),
      output_file = file_out("docs/models.html"),
      params = list(d = loaded_processed_data)),
    
    # for site
    
    drake_meta = rmarkdown::render(
      knitr_in("drake-graph.Rmd"),
      output_file = file_out("docs/drake-graph.html")),
    
    site_index = rmarkdown::render(
      knitr_in("index.Rmd"),
      output_file = file_out("docs/index.html"))
    
)
