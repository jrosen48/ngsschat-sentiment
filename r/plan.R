the_plan <-
  drake_plan(

    loaded_rda_data = load(file_in("data-raw/data_final_2020_09_04.rda")),
    state_data <- read_csv(file_in("data-raw/ngsschat-state-data.csv")),
    state_acronyms = read_csv(file_in("data-raw/state-acronyms.csv")),
    
    
)
