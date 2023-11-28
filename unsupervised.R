library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)

DIR <- '/Users/carlosperezricardo/Downloads/'

drop_X <- function(df) {
  df$X <- NULL
  return(df)
}

emirate_offer <- drop_X(read.csv(paste0(DIR,'offer_emirate.csv')) %>% as.data.table)
community_offer <- drop_X(read.csv(paste0(DIR,'offer_by_community.csv')) %>% as.data.table)

emirate_demand <- drop_X(read.csv(paste0(DIR,'demand_emirate.csv')) %>% as.data.table)
community_demand <- drop_X(read.csv(paste0(DIR,'demand_by_community.csv')) %>% as.data.table)

cat(dim(emirate_offer),':', dim(community_offer))
cat(dim(emirate_demand),':', dim(community_demand))

# LAGS
lag_data <- function(df, lags, by_cols=NULL) {
  df_ <- copy(df)
  setorder(df_, dt_year, dt_month)
  columns_to_lag <- setdiff(names(df_), c("dt_year", "dt_month"))
  
  # Create lagged variables for each specified column
  for (col in columns_to_lag) {
    for (lag in lags) {
      lagged_col_name <- paste0(col, "_lag", lag)
      if (is.null(by_cols)) {
        df_[, (lagged_col_name) := shift(get(col), lag)]  
      } else {
        df_[, (lagged_col_name) := shift(get(col), lag), by=(get(by_cols))] 
      }
      
    }
  }
  return(df_)
}
lags <- c(1, 3, 6)

emirate_offer_ <- lag_data(emirate_offer, lags)
emirate_demand_ <- lag_data(emirate_demand, lags)

community_offer_ <- lag_data(community_offer, lags, 'community')
community_demand_ <- lag_data(community_demand, lags, 'community')

