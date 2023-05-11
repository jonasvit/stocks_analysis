################################################################################
### Explore instruments
################################################################################

# Modify data daily
modify_instruments(instrument_data)

overview_plots <- list()
return_plots <- list()

# For each instrument get an overview. Strategy selected - EMA.
for (instrument in instruments_list){
  
  # Create dummy dataframe name on which the model is running
  df_name <- paste0('df_instrument_',instrument)
  
  # Create MAs
  moving_avg(instruments_daily, instrument, 151, 350, type = 'e', try_best = TRUE)

  # Get total and strategies returns
  get_returns_new(get(df_name), df_name, testing = TRUE)

  # Plot instrument overview with strategy
  plot_instrument(get(df_name))

  # Plot returns
  plot_returns(get(df_name))
  
}

