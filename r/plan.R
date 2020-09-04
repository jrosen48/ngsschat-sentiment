the_plan <-
  drake_plan(

    loaded_rda_data = load_rda(file_in("data-raw/data_final_2020_09_04.rda")),
    state_data = read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    state_acronyms = read_csv(file_in("data-raw/state-acronyms.csv")),
    
    prepared_data = rmarkdown::render(
      knitr_in("prepare-for-modeling.Rmd"),
      output_file = file_out("docs/prepare-for-modeling.html"),
      params = list(tweets_dl = loaded_rda_data,
                    state_data = state_data,
                    state_acronyms = state_acronyms))
)
