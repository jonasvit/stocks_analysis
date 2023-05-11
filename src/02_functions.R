################################################################################
### Functions
################################################################################


#' Modify instruments dataframe
#'
#' @param df 
#'
#' @return Instruments dataframe with date moficiations and a list of all instruments
#' @export
#'
#' @examples modify_instruments(instrument_data)
modify_instruments <- function(df){
  
  # Convert instrument dates to POSIXct format
  df$date <- anytime(df$date, tz = 'EEST')
  # Extract only date from the time
  df$Date_agg <- as.IDate(df$date)
  
  # Aggregate instruments by days
  instruments_daily <- df[, .(open = first(open), # summarizing by taking the first record per group
                              high = max(high), # summarizing by taking the max value per group
                              low = min(low), # summarizing by taking the min value per group
                              close = last(close), # summarizing by taking the last value per group
                              volume = sum(volume)), # summarizing by summing all values per group
                          by=list(Date_agg, instrument_id)]
  # Reorder columns for convenient
  setcolorder(instruments_daily, c("instrument_id", "Date_agg"))
  
  # Overwrite DataFrame with changes
  assign("instruments_daily", instruments_daily, envir = globalenv())
  # Write a list of all instruments in the dataframe
  assign("instruments_list", as.numeric(unique(instruments_daily$instrument_id)), envir = globalenv())
  
  return("Instruments dataframe with modifications")
}


#' Create moving average columns
#'
#' @param df Dataframe
#' @param instrument_id Instrument id
#' @param first_ma A number of days to roll moving average
#' @param second_ma Another number of days to roll moving average
#' @param type Type of moving average: 'basic' as classical one, 'e' as exponential
#' @return Instruments dataframe with moving averages as columns
#' @export
#'
#' @examples moving_avg(instruments_daily, 1001, 12, 26)
moving_avg <- function(df, instrument_ids, first_ma = 12, second_ma = 26, type = 'e', try_best = TRUE, testing = FALSE){
  
  # If optimization function was passed before and only best results are wanted, try_best == TRUE
  # Current results are already known by running function get_optimal_ma_strategy()
  if (try_best == TRUE){
    if (instrument_ids == 1001){
      first_ma = 4
      second_ma = 12
    } else if (instrument_ids == 1002){
      first_ma = 10
      second_ma = 12
    } else if (instrument_ids == 1003){
      first_ma = 37
      second_ma = 40
    } else if (instrument_ids == 1004){
      first_ma = 16
      second_ma = 11
    } else if (instrument_ids == 1005){
      first_ma = 7
      second_ma = 9
    } else if (instrument_ids == 1006){
      first_ma = 8
      second_ma = 15
    } else if (instrument_ids == 1007){
      first_ma = 12
      second_ma = 34
    }
  }
  
  # Get dataframe of one instrument_id
  df_instrument <- df[instrument_id == instrument_ids, ]
  
  # Create column names with specific MA digits
  ma_name1 <- paste0('moving_avg_',first_ma)
  ma_name2 <- paste0('moving_avg_',second_ma)
  
  # There is two averages considered - moving average and exponential and basic moving average
  if (type == 'e'){
    
    # Create column of first moving average
    df_instrument[, ma_name1] <- data.table::shift(pracma::movavg(df_instrument$close, n=first_ma, type='e'))
    df_instrument[2:first_ma, ma_name1] <- NA
    #data.table::shift(zoo::rollmean(df_instrument$close, k = first_ma, fill = NA))
    
    # Create column of second moving average
    df_instrument[, ma_name2] <- data.table::shift(pracma::movavg(df_instrument$close, n=second_ma, type='e'))
    df_instrument[2:second_ma, ma_name2] <- NA
    #data.table::shift(zoo::rollmean(df_instrument$close, k = second_ma, fill = NA))
    
  } else if (type == 'basic'){
    
    # Create column of first moving average
    df_instrument[, ma_name1] <- data.table::shift(roll_mean(df_instrument$close, n = first_ma, align = "left", fill = NA, na.rm=TRUE))
    
    # Create column of second moving average
    df_instrument[, ma_name2] <- data.table::shift(roll_mean(df_instrument$close, n = second_ma, align = "left", fill = NA, na.rm=TRUE))
    
  } else
    stop("No such type of moving average. Please try 'e' or 'basic'.")
  
  # Create signal column that indicates different MA movings                                                                          ifelse (paste0('moving_avg_',first_ma)<paste0('moving_avg_',second_ma),-1,0)))
  df_instrument <- df_instrument %>% 
    mutate("signal" = ifelse(df_instrument[[ma_name1]] > df_instrument[[ma_name2]], 1,
                             ifelse(df_instrument[[ma_name1]] < df_instrument[[ma_name2]], -1, 0)))
  
  #df_instrument <- tail(df_instrument, -max(first_ma,second_ma))
  
  # Return dataframe with new variables
  assign(paste0("df_instrument_",instrument_ids), df_instrument, envir = globalenv())
  # Return moving average columns in a list
  assign('ma_columns', list(ma_name1, ma_name2), envir = globalenv())
  
  if(try_best == FALSE & testing == TRUE){
    return(df_instrument)
  }
}


#' Plot time series for specific stock
#'
#' @param df Dataframe that is passed with specific stock information
#' @param stock_name Stock name
#'
#' @return An interactive plot
#' @export
#'
#' @examples plot_instrument(df)
plot_instrument <- function(df){
  
  # Create dummy dataframes with renamed columns for buying and selling points only
  sell_df <- df[Entry == -2,]
  colnames(sell_df)[8] = "Short_moving_avg"
  
  buy_df <- df[Entry == 2,]
  colnames(buy_df)[9] = "Long_moving_avg"
  
  # Plotting with plot_ly
  plot <- df %>%
    plot_ly(x = ~Date_agg, type = "candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low, name = 'Stocks')
  plot <- plot %>% add_lines(x = ~Date_agg, y = ~df[[ma_columns[[1]]]], line = list(color = 'blue', width = 0.75),
                             inherit = F, name = ma_columns[[1]])
  plot <- plot %>% add_lines(x = ~Date_agg, y = ~df[[ma_columns[[2]]]], line = list(color = 'red', width = 0.75),
                             inherit = F, name = ma_columns[[2]])
  plot <- plot %>% 
    layout(title = paste0('Instrument ', instrument,' price per time period'),
           legend = list(title=list(text='<b> Variables </b>'),x = 0.1, y = 1, bgcolor = 'rgba(244, 251, 250,0.3)'),
           plot_bgcolor = "#f4fbfa",
           xaxis = list(title = 'Date'), 
           yaxis = list(title = paste(instrument,'instrument price'))
    )
  plot <- plot %>%
    add_trace(data = sell_df, x = ~Date_agg, y = ~Short_moving_avg, type = 'scatter', mode = 'markers',
              marker = list(
                color = 'rgb(225, 12, 5)',
                size = 15,
                symbol = "triangle-down"
                ), name = "Sell"
              )
  
  plot <- plot %>%
    add_trace(data = buy_df,x = ~Date_agg, y = ~Long_moving_avg, type = 'scatter', mode = 'markers',
              marker = list(
                color = 'rgb(31, 202, 79)',
                size = 15,
                symbol = "triangle-up"
              ), name = "Buy"
              )
  
  #overview_plots[[paste0('overview_',instrument)]] =  plotly_build(plot)
  assign(paste0('overview_',instrument), plot, envir = globalenv())
  
  return(print(plot))
}



#' Get returns for both total and strategy
#'
#' @param df Input dataframe
#' @param return_name Name of the dataframe
#'
#' @return Dataframe with new columns: Total_return, Strategy_returns and Entry
#' @export
#'
#' @examples get_returns(instrument_data)
get_returns <- function(df, return_name = as.character(deparse(substitute(df)))){
  
  # General returns
  # df[, 'Total_return':= log(close) - data.table::shift(log(close), fill = NA)]
  df[, 'Total_return':= (close/first(close))-1]#(close/data.table::shift(close, fill = NA))-1]
  
  # Percentage change per day
  df[, 'Percentage_change':=(close/data.table::shift(close, fill = NA))-1]
  df[, 'strategyxxx':= Percentage_change * signal]
  
  # Strategies returns
  df[, 'Strategy_returns':= signal*Total_return]
  
  df[, 'Strategy_returns_new':= (1+strategyxxx)*first(close)]
  
  # Entry
  df[, 'Entry':= signal - data.table::shift(signal, fill = NA)]
  
  # Return dataset with new columns
  assign(return_name, df, envir = globalenv())
  
}

#' Get cumulative sums for both total and strategy returns. This way of calculating cumulative sums
#' was considered after spending plenty of time by trying to make functions like cumsum() work.
#'
#' @param df Input dataframe
#' @param testing Logical variable if the function is used for getting optimal MAs or not.
#'
#' @return Instruments dataframe with date moficiations and a list of all instruments
#' @export
#'
#' @examples get_returns(instrument_data)
get_cumsums <- function(df, testing = FALSE){
  
  df$Date_agg <- as.character(df$Date_agg)
  
  df <- sqldf('
      with lenta as (Select *,
             Total_return as exp_return_cumsum,
             Strategy_returns_new as exp_str_return_cumsum
     from df)
     select *,
        exp_return_cumsum as Total_return_cumsum,
        exp_str_return_cumsum as Strategy_return_cumsum
     from lenta
      ')
  
  df$Date_agg <- as.Date(df$Date_agg)
  assign('testukas', df, envir = globalenv())
  if(testing==TRUE){
    df <- tail(df$Strategy_return_cumsum,1)
  }
  
  return(df)
  
}


#' Plot returns comparing Buy/Hold and system strategies
#'
#' @param df Dataframe that is passed with specific stock information
#'
#' @return An interactive plot
#' @export
#'
#' @examples plot_returns(df)
plot_returns <- function(df){
  
  #df1 <- get_cumsums(df)
  
  # Plotting with plot_ly
  plot <- plot_ly(df, x = ~Date_agg, y = ~Total_return, name = 'Total return', type = 'scatter', mode = 'lines', color = I("#233f53")) %>% 
    layout(title = paste('Returns for',instrument,'per time period'),
           legend = list(title=list(text='<b> Variables </b>'),x = 0.1, y = 1, bgcolor = 'rgba(244, 251, 250,0.3)'),
           plot_bgcolor = "#f4fbfa",
           xaxis = list(title = 'Date'), 
           yaxis = list(title = paste(instrument,'instrument price'))
    )
  plot <- plot %>% add_trace(y = ~Strategy_returns, name = 'Strategy returns', mode = 'lines',color = I("#d33c13"))
  
  #return_plots[[paste0('returns_',instrument)]] <-  plot
  assign(paste0('returns_',instrument), plot, envir = globalenv())
  
  return(print(plot))
}


#' Function that outputs optimal MA values to perform on specific instrument. Input is taken from global env.
#'
#' @param max_iterations Max number of iterations to run different moving averages through. Starting point is 2.
#'
#' @return An interactive plot
#' @export
#'
#' @examples get_optimal_ma_stategy()
get_optimal_ma_stategy <- function(start_with = 5, max_iterations = 40, instrument = 1007){
  
  if(max_iterations < 5){
    stop('The number has to be at least 3.')
  } else {
    for (first in start_with:max_iterations){
      for (second in (start_with+1):max_iterations){
        
        if(first != second){
          df <- moving_avg(instruments_daily, instrument, first_ma = first, second_ma = second, type = 'e', try_best = FALSE, testing = TRUE)
          df <- get_returns_new(df, paste0('df_instrument_',instrument), testing = TRUE)
        }
        
        if (first == start_with & second ==start_with+1){
          dt = data.table(first_maa = first,
                          second_maa = second,
                          Strategy_returns = 0)
        } else {
          print(paste0(first," and ",second))
          dt1 = data.table(first_maa = first,
                           second_maa = second,
                           Strategy_returns = as.numeric(tail(df[!is.na(df$Strategy_returns),'Strategy_returns'],1)))
          dt = rbind(dt,dt1)
        }
        
      }
    }
  }
  
  assign('all_values', dt, envir = globalenv())
  
  return(print(dt %>% filter(Strategy_returns == max(Strategy_returns))))
  
}


#' Get returns for both total and strategy
#'
#' @param df Input dataframe
#' @param return_name Dataframe name to be returned
#' @param return_name Name of the dataframe
#' @param total_portfolio The amount of total portfolio
#' @param percent_portfolio_to_invest The percent from total portfolio to invest into instrument on "Buy" moment
#' @param testing Boolean parameter: if TRUE, return strategy investment dataframe.
#'
#' @return Dataframe with new columns of strategy.
#' @export
#'
#' @examples get_returns_new(get(df_name), df_name, testing = TRUE)
get_returns_new <- function(df, return_name = as.character(deparse(substitute(df))), total_portfolio = 1000,
                            percent_portfolio_to_invest = 0.05, testing = FALSE){
  
  # Entry
  df[, 'Entry':= signal - data.table::shift(signal, fill = NA)]
  
  na_entry <- sum(is.na(df$signal))+1
  df[1:na_entry, 'Entry'] <- 0
  
  
  first_entrance_value_row <- first(which(df$Entry == 2, arr.ind=TRUE))
  
  strategy_df <- data.table(Investment_from = as.Date(df$Date_agg[first_entrance_value_row]),
                            Total_portfel = total_portfolio,
                            Investment_percent = percent_portfolio_to_invest,
                            investment_money = total_portfolio*percent_portfolio_to_invest,
                            total_saved = NA)
  
  df[1:first_entrance_value_row-1, 'Entry'] <- NA
  strat_row <- 1
  
  for (row in first_entrance_value_row:nrow(df)){
    
    if(df$Entry[row] == 2 & df$signal[row] == 1 & row != nrow(df)){
      
      df[row,'Invest_or_take'] <- strategy_df$investment_money[strat_row]
      
    } else if (df$Entry[row] == 0){
      
      if(row == first_entrance_value_row+1){
        
        df[row, 'Invest_or_take'] <- df$Invest_or_take[row-1] * df$close[row] / df$close[row-1]
        
      } else {
        
        if (df$signal[row-1] == 1 & row != nrow(df)){
          
          df[row, 'Invest_or_take'] <- df$Invest_or_take[row-1] * df$close[row] / df$close[row-1]
          
        } else if (row != nrow(df)){
          
          df[row, 'Invest_or_take'] <- NA
          
        } else if (row == nrow(df) & df$signal[row] == 1){
          
          strategy_df[strat_row, 'total_saved'] <- df$Invest_or_take[row]
          
          df[row, 'Invest_or_take'] <- df$Invest_or_take[row-1] * df$close[row] / df$close[row-1]
          strategy_df <- transform(strategy_df, total_saved = as.numeric(total_saved))
          strategy_df[strat_row, 'total_saved'] <- df$Invest_or_take[row]
          strat_row <- strat_row + 1
          strategy_row <- data.table(Investment_from = as.Date(df$Date_agg[row]),
                                     Total_portfel = strategy_df$Total_portfel[strat_row-1] + df$Invest_or_take[row]-strategy_df$investment_money[strat_row-1],
                                     Investment_percent = percent_portfolio_to_invest,
                                     investment_money = 0,
                                     total_saved = NA)
          strategy_row <- transform(strategy_row, total_saved = as.numeric(total_saved))
          strategy_df <- rbind(strategy_df, strategy_row)
          
        }
        
      }
      
    } else if (df$Entry[row] == -2){
      
      df[row, 'Invest_or_take'] <- df$Invest_or_take[row-1] * df$close[row] / df$close[row-1]
      strategy_df <- transform(strategy_df, total_saved = as.numeric(total_saved))
      strategy_df[strat_row, 'total_saved'] <- df$Invest_or_take[row]
      strat_row <- strat_row + 1
      strategy_row <- data.table(Investment_from = as.Date(df$Date_agg[row]),
                                 Total_portfel = strategy_df$Total_portfel[strat_row-1] + df$Invest_or_take[row]-strategy_df$investment_money[strat_row-1],
                                 Investment_percent = percent_portfolio_to_invest,
                                 investment_money = percent_portfolio_to_invest* (strategy_df$Total_portfel[strat_row-1] + df$Invest_or_take[row]-strategy_df$investment_money[strat_row-1]),
                                 total_saved = NA)
      strategy_row <- transform(strategy_row, total_saved = as.numeric(total_saved))
      strategy_df <- rbind(strategy_df, strategy_row)
      
    } else stop('yo')
  }
  
  strategy_df$Strategy_returns <- strategy_df$Total_portfel-total_portfolio
  
  df[first_entrance_value_row:nrow(df), 'Total_return_growth':= (close/df$close[first_entrance_value_row])-1]
  df[first_entrance_value_row, 'Total_return'] = total_portfolio*percent_portfolio_to_invest
  df[, 'Total_return'] = total_portfolio*percent_portfolio_to_invest*(1+df$Total_return_growth)
  
  setnames(df, "Invest_or_take", "Strategy_returns")
  
  df <- df[order(rank(Date_agg))]
  
  assign(return_name, df, envir = globalenv())
  
  if (testing == TRUE){
    return(print(strategy_df))
  }
}

