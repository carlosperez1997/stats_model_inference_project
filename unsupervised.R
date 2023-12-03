library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)
library(factoextra)
library(BiocManager)

DIR <- '/Users/carlosperezricardo/Downloads/'

drop_X <- function(df) {
  df$X <- NULL
  return(df)
}

# Offer: Listed properties webscraped from Bayut
# Demand: Real transactions extracted from Dubai Land Deparment API
# Note that we have less year-months in offer than in demand

emirate_offer <- drop_X(read.csv(paste0(DIR,'offer_emirate.csv')) %>% as.data.table)
emirate_demand <- drop_X(read.csv(paste0(DIR,'demand_emirate.csv')) %>% as.data.table)
# Drop repeated count in demand
emirate_demand$emirate_price_area_count <- NULL

nulls <- colSums(is.na(emirate_offer))
print(nulls[nulls!=0])

# Drop offplan (many nulls)
cols <- setdiff(names(emirate_offer),  grep('offplan', names(emirate_offer), value=T))
emirate_offer <- emirate_offer[, .SD, .SDcols = cols]

nulls <- colSums(is.na(emirate_offer))
print(nulls[nulls!=0])

nulls <- colSums(is.na(emirate_demand))
print(nulls[nulls!=0])

# Perform lag percentage (house price index does sth like this)
# value_pct = (value - shifted_value) / shifted_value
# It performs this lag percentage for each column. 
# get(col) helps to work iteratively with any column
# 
# Positive values indicate, this column has increased with respect to shift reference
# Negative values indicate, this column has decreased with respect to shift reference
lag_percentage <- function(df, lags, by_cols=NULL) {
  df_ <- copy(df)
  setorder(df_, dt_year, dt_month)
  columns_to_lag <- setdiff(names(df_), c("dt_year", "dt_month", by_cols))
  for (col in columns_to_lag) {
    for (lag in lags) {
      lagged_col_name <- paste0(col, "_lag", lag)
      if (is.null(by_cols)) {
        df_[, (lagged_col_name) := (get(col) - shift(get(col), lag)) / shift(get(col), lag)]  
      } else {
        df_[, (lagged_col_name) := shift(get(col), lag), by=(get(by_cols))]
        df_[, (lagged_col_name) := (get(col) - get(lagged_col_name)) / get(lagged_col_name)] 
      }
    }
  }
  return(df_)
}

lags <- c(3, 6, 12, 18)
emirate_offer_ <- lag_percentage(emirate_offer, lags)
emirate_demand_ <- lag_percentage(emirate_demand, lags)

# Set keys and drop NULL
setindex(emirate_demand_, dt_year, dt_month)
emirate_demand_ <- emirate_demand_ %>% drop_na()
setindex(emirate_offer_, dt_year, dt_month)
emirate_offer_ <- emirate_offer_ %>% drop_na()

cat('Emirate:', dim(emirate_offer_),':', dim(emirate_demand_))

perform_pca <- function(df) {
  # Select only the desired columns
  # grep is similar to string filter 
  # (select only those columns that have "lag" in its name)
  cols <- setdiff(names(df), c('dt_year','dt_month'))
  cols <- grep("lag", cols, value = TRUE)
  
  data.pca <- prcomp(scale(df[, .SD, .SDcols = cols]), center=T, scale.=T)
  pca_transformed_data <- predict(data.pca)[,1:5]
  
  return(list(pca = data.pca, transf_data = pca_transformed_data))
}

determine_sign_pca <- function(pca_transformed_data, data, col ) {
  i <- 1
  for (sign_ in cor(pca_transformed_data, data[, get(col)])) {
    cat('PCA',i,':',sign_,'\n')
    pca_transformed_data[,i] <- sign(sign_) * pca_transformed_data[,i]
    i <- i+1
  }
  
  pca_transformed_data
}

# OFFER
  x <- perform_pca(emirate_offer_)
  data.pca <- x$pca
  pca_transformed_data_offer <- x$transf_data
  summary(data.pca)$importance[, 1:5]
    
  fviz_eig(data.pca, addlabels = TRUE)
  
  # First component
  sorted_indices <- order(-abs(data.pca$rotation[,1]))
  sorted_loadings <- data.pca$rotation[,1][sorted_indices]
  sorted_loadings[1:5]
  
  fviz_pca_var(data.pca, col.var = "black")
  
# DEMAND
  x <- perform_pca(emirate_demand_)
  data.pca <- x$pca
  pca_transformed_data_demand <- x$transf_data
  summary(data.pca)$importance[, 1:5]
  
  fviz_eig(data.pca, addlabels = TRUE)
  
# Train-test split (validation)
  scaler <- scale(emirate_demand_[dt_year < 2023] %>% drop_na() %>% select(-dt_year, -dt_month))
  train <- scaler

  test <- as.data.frame(scale(emirate_demand_[dt_year >= 2023] %>% select(-dt_year, -dt_month), 
                              center = attr(scaler, "scaled:center"), 
                              scale = attr(scaler, "scaled:scale")))

  pc <- prcomp(train, center = TRUE, scale. = TRUE)
  pc_train <- predict(pc, newdata = train)
  pc_test <- predict(pc, newdata = test)
  
  eigenvalues_train <- apply(pc_train, 2, function(col) sum(col^2))
  explained_variance_train <- eigenvalues_train / sum(eigenvalues_train)
  cat("\nExplained Variance for Test Data (Percentage):\n")
  print(explained_variance_train[1:5])
  summary(pc)$importance[, 1:5]
  
  eigenvalues_test <- apply(pc_test, 2, function(col) sum(col^2))
  explained_variance_test <- eigenvalues_test / sum(eigenvalues_test)
  cat("\nExplained Variance for Test Data (Percentage):\n")
  print(explained_variance_test[1:5])

# Neural Nets PCA
  library(pcaMethods)
  pc <- pcaMethods::pca(as.matrix(scale(emirate_demand_)), 
                        method="nlpca", nPcs=4, 
                        maxSteps=2000)
  summary(pc)
  pca_transformed_data_demand <- pcaMethods::scores(pc)

# Bayesian
  pc <- pcaMethods::pca(as.matrix(scale(emirate_demand_)), 
                        method="bpca", nPcs=4)
  summary(pc)
  
  #loadings <- pcaMethods::loadings(pc)
  #scores <- pcaMethods::scores(pc)
  
# Final result
  pca_transformed_data <- determine_sign_pca(pca_transformed_data_demand, 
                                             emirate_demand_, 
                                             "emirate_price_count_lag12")
  
  
  emirate_demand_transformed <- cbind(emirate_demand_, 
                                      data.table(PC1_demand = pca_transformed_data[,1], 
                                                 PC2_demand = pca_transformed_data[,2],
                                                 PC3_demand = pca_transformed_data[,3])) %>%
    .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]
  
  pca_transformed_data <- determine_sign_pca(pca_transformed_data_offer, 
                                             emirate_offer_, 
                                             "emirate_price_count_lag12")

  emirate_offer_transformed <- cbind(emirate_offer_, 
                                   data.table(PC1_offer = pca_transformed_data[,1], 
                                              PC2_offer = pca_transformed_data[,2],
                                              PC3_offer = pca_transformed_data[,3])
                                  ) %>%
  .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]



#### VALIDATE WITH INDEX
scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
  
index <- read.csv(paste0(DIR, 'residential_index.csv')) %>% 
  as.data.table %>% .[, date := lubridate::as_date(DateTime)]

plot_data <- merge(emirate_demand_transformed, index, by='date', all.x=T, all.y=T)
plot_data <- merge(plot_data, emirate_offer_transformed, by='date', all.x=T, all.y=T)
plot_data <- plot_data[date <= '2022-11-01'] %>% .[date >= '2014-01-01']

# Scale to 0 and 1 and Moving average to smooth
pca_cols <- grep("PC", names(plot_data), value=T)
for (col in pca_cols) {
  plot_data[, (col) := scale_to_01(get(col))]
  moving_avg_col <- paste0(col, "_ma")
  plot_data[, (col) := zoo::na.locf(get(col), na.rm = FALSE, fromLast = FALSE)]
  plot_data[, (moving_avg_col) := frollmean(get(col), 4, align = "right", fill = 1)]
}

reg <- lm(Index ~ PC1_offer+PC2_offer+PC3_offer+PC1_demand+PC2_demand+PC3_demand, plot_data)
summary(reg)

# Regression to Index is quite good!
reg <- lm(Index ~ PC1_offer_ma+PC2_offer_ma+PC3_offer_ma+PC1_demand_ma+PC2_demand_ma+PC3_demand_ma, plot_data)
summary(reg)

# Offer: blue - Demand: black
scaleFactor <-  max(plot_data$PC1_offer, na.rm=T) / max(plot_data$Index, na.rm=T)
ggplot(plot_data, aes(x=date)) +
  geom_point(aes(y=PC1_offer), col="blue", alpha=0.3) +
  geom_point(aes(y=PC1_demand), col="black", alpha=0.3) +
  geom_line(aes(y=PC1_offer_ma), col="blue") +
  geom_line(aes(y=PC1_demand_ma), col="black") +
  #geom_line(aes(y=PC2_ma), col="steelblue") +
  #geom_line(aes(y=PC3_ma), col="yellow") +
  geom_line(aes(y=Index * scaleFactor), col="red") +
  theme_minimal() +
  labs(title='Real Estate Residential Indexes', x='Dates') + 
  scale_y_continuous(name="Custom indexes", sec.axis=sec_axis(~./scaleFactor, name="Official Real Estate Index")) +
  theme(
    axis.title.y.left=element_text(color="black", size=14),
    axis.text.y.left=element_text(color="black", size=12),
    axis.title.y.right=element_text(color="red", size=14),
    axis.text.y.right=element_text(color="red", size=14),
    axis.text.x=element_text(size=12),
    axis.title.x=element_text(size=16),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) + geom_text(aes(x = as.Date("2018-01-01"), y = 0.8, label = "Offer"), color = "blue", size=5) +
  geom_text(aes(x = as.Date("2016-01-05"), y = 0.5, label = "Demand"), color = "black", size=5) +
  geom_text(aes(x = as.Date("2016-04-01"), y = 1, label = "DLD residential index"), color = "red", size=5) 


###### COMMUNITY
  #"Dubai Marina" "Jumeirah Lake Towers (JLT)" "Jumeirah Beach Residence (JBR)"
  
  community_ <- "Jumeirah Beach Residence (JBR)"
  
  community_offer <- drop_X(read.csv(paste0(DIR,'offer_by_community.csv')) %>% as.data.table)
  community_demand <- drop_X(read.csv(paste0(DIR,'demand_by_community.csv')) %>% as.data.table)
  community_offer <- community_offer[(dt_year == 2023 & dt_month >= 6) == F]
  community_demand <- community_demand[(dt_year == 2023 & dt_month >= 6) == F]
  
  lags <- c(3, 6, 12)
  community_offer <- lag_percentage(community_offer, lags, 'community')
  community_demand <- lag_percentage(community_demand, lags, 'community')
  
  ### Drop offplan in offer (many nulls)
  cols <- setdiff(names(community_offer),  grep('offplan', names(community_offer), value=T))
  community_offer <- community_offer[, .SD, .SDcols = cols]

  nulls <- colSums(is.na(community_offer))
  print(nulls[nulls!=0])

  setindex(community_offer, dt_year, dt_month, community)
  community_offer <- community_offer %>% drop_na()
  setindex(community_demand, dt_year, dt_month)
  community_demand <- community_demand %>% drop_na()

  # Subset 
  community_offer <- community_offer[community == community_]
  community_demand <- community_demand[community == community_]

  cat('Community', dim(community_offer),':', dim(community_demand))

# Offer
  x <- perform_pca(community_offer)
  data.pca <- x$pca
  pca_transformed_data <- x$transf_data
  summary(data.pca)$importance[, 1:5]

  fviz_eig(data.pca, addlabels = TRUE)
  
  pca_transformed_data <- determine_sign_pca(pca_transformed_data, 
                                             community_offer, 
                                             "community_price_count_lag3")
  
  community_offer_transformed <- cbind(community_offer, 
                                     data.table(PC1 = pca_transformed_data[,1], 
                                                PC2 = pca_transformed_data[,2],
                                                PC3 = pca_transformed_data[,3])) %>%
    .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]
  
  names(community_offer_transformed) <- paste0('offer_', names(community_offer_transformed))
  
# Demand
  x <- perform_pca(community_demand)
  data.pca <- x$pca
  pca_transformed_data <- x$transf_data
  summary(data.pca)$importance[, 1:5]
  
  fviz_eig(data.pca, addlabels = TRUE)
  
  pca_transformed_data <- determine_sign_pca(pca_transformed_data, 
                                             community_demand, 
                                             "community_price_count_lag3")
  
  community_demand_transformed <- cbind(community_demand, 
                                        data.table(PC1 = pca_transformed_data[,1], 
                                                   PC2 = pca_transformed_data[,2],
                                                   PC3 = pca_transformed_data[,3])) %>%
    .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]
  
  names(community_demand_transformed) <- paste0('demand_', names(community_demand_transformed))

# JOIN
  plot_data <- merge(community_demand_transformed, community_offer_transformed, 
                     by.x='demand_date', by.y='offer_date', all.x=T, all.y=T)
  plot_data <- plot_data[demand_dt_year >= 2019]

  # Scale to 0 and 1 and Moving average to smooth
  pca_cols <- grep("PC", names(plot_data), value=T)
  for (col in pca_cols) {
    plot_data[, (col) := scale_to_01(get(col))]
    moving_avg_col <- paste0(col, "_smooth")
    plot_data[, (moving_avg_col) := frollmean(get(col), 3, align = "right", fill = NA)]
  }

# Offer: blue - Demand: black
  big_max <- max(max(plot_data$demand_community_price_count, na.rm = TRUE), max(plot_data$offer_community_price_count, na.rm = TRUE))
  small_max <- min(max(plot_data$demand_community_price_count, na.rm = TRUE), max(plot_data$offer_community_price_count, na.rm = TRUE))
  cat(big_max, small_max)
  
  scaleFactor <- max(plot_data$demand_PC1, na.rm = TRUE) / small_max
  
  # Create the plot
  ggplot(plot_data, aes(x = demand_date)) +
    geom_point(aes(y = offer_PC1), col = "blue", alpha = 0.3) +
    geom_point(aes(y = demand_PC1), col = "black", alpha = 0.3) +
    geom_line(aes(y = offer_PC1_smooth), col = "blue") +
    geom_line(aes(y = demand_PC1_smooth), col = "black") +
    geom_line(aes(y = plot_data$demand_community_price_count * scaleFactor), col = "red") +
    geom_line(aes(y = plot_data$demand_community_price_offplan_count * scaleFactor), col = "red",  linetype = "dashed") +
    theme_minimal() +
    labs(title = paste0('Real Estate Residential Indexes - ', community_), x = 'Dates') + 
    scale_y_continuous(name = "Custom indexes", sec.axis = sec_axis(~./scaleFactor, name = "Number of transactions")) +
    theme(
      axis.title.y.left = element_text(color = "black", size = 14),
      axis.text.y.left = element_text(color = "black", size = 12),
      axis.title.y.right = element_text(color = "red", size = 14),
      axis.text.y.right = element_text(color = "red", size = 14),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    geom_text(aes(x = as.Date("2021-05-01"), y = 0.7, label = "Offer"), color = "blue", size = 4.5) +
    geom_text(aes(x = as.Date("2019-10-05"), y = 0.6, label = "Demand"), color = "black", size = 4.5) +
    geom_text(aes(x = as.Date("2022-05-01"), y = 0.95, label = "Nb transactions"), color = "red", size = 4.5) +
    geom_text(aes(x = as.Date("2022-09-01"), y = 0.05, label = "Nb transactions - \n Offplan"), color = "red", size = 4.5)


  big_max <- max(max(plot_data$demand_community_price_count, na.rm = TRUE), max(plot_data$offer_community_price_count, na.rm = TRUE))
  small_max <- min(max(plot_data$demand_community_price_count, na.rm = TRUE), max(plot_data$offer_community_price_count, na.rm = TRUE))
  cat(big_max, small_max)
  
### OFFER PLOT
  scaleFactor <- max(plot_data$demand_PC1, na.rm = TRUE) / big_max
  
  # Create the plot
  ggplot(plot_data, aes(x = demand_date)) +
    geom_point(aes(y = offer_PC1), col = "blue", alpha = 0.3) +
    geom_point(aes(y = demand_PC1), col = "black", alpha = 0.3) +
    geom_line(aes(y = offer_PC1_smooth), col = "blue") +
    geom_line(aes(y = demand_PC1_smooth), col = "black") +
    geom_line(aes(y = plot_data$offer_community_price_count * scaleFactor), col = "purple") +
    #geom_line(aes(y = plot_data$offer_community_price_offplan_count_x * scaleFactor), col = "red",  linetype = "dashed") +
    theme_minimal() +
    labs(title = paste0('Real Estate Residential Indexes - ', community_), x = 'Dates') + 
    scale_y_continuous(name = "Custom indexes", 
                       sec.axis = sec_axis(~./scaleFactor, name = "Number of listings", )) +
    theme(
      axis.title.y.left = element_text(color = "black", size = 14),
      axis.text.y.left = element_text(color = "black", size = 12),
      axis.title.y.right = element_text(color = "red", size = 14),
      axis.text.y.right = element_text(color = "red", size = 14),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    geom_text(aes(x = as.Date("2021-05-01"), y = 0.7, label = "Offer"), color = "blue", size = 4.5) +
    geom_text(aes(x = as.Date("2019-10-05"), y = 0.6, label = "Demand"), color = "black", size = 4.5) +
    geom_text(aes(x = as.Date("2022-05-01"), y = 0.95, label = "Nb listings"), color = "purple", size = 4.5)

### OFFER PRICE
  scaleFactor <- max(plot_data$demand_PC1, na.rm = TRUE) /  (max(plot_data$offer_community_price_mean, na.rm = TRUE))
  
  # Create the plot
  ggplot(plot_data, aes(x = demand_date)) +
    geom_point(aes(y = offer_PC1), col = "blue", alpha = 0.3) +
    geom_point(aes(y = demand_PC1), col = "black", alpha = 0.3) +
    geom_line(aes(y = offer_PC1_smooth), col = "blue") +
    geom_line(aes(y = demand_PC1_smooth), col = "black") +
    geom_line(aes(y = plot_data$offer_community_price_mean * scaleFactor), col = "darkgreen") +
    #geom_line(aes(y = plot_data$offer_community_price_offplan_count_x * scaleFactor), col = "red",  linetype = "dashed") +
    theme_minimal() +
    labs(title = paste0('Real Estate Residential Indexes - ', community_), x = 'Dates') + 
    scale_y_continuous(name = "Custom indexes",
                       sec.axis = sec_axis(~./scaleFactor, name = "Avg Listings price [M AED]", 
                                           labels = scales::comma_format(scale = 1/1e6, accuracy = 0.2))) +
    theme(
      axis.title.y.left = element_text(color = "black", size = 14),
      axis.text.y.left = element_text(color = "black", size = 12),
      axis.title.y.right = element_text(color = "darkgreen", size = 14),
      axis.text.y.right = element_text(color = "darkgreen", size = 14),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 16),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    ) +
    geom_text(aes(x = as.Date("2020-11-01"), y = 0.7, label = "Offer"), color = "blue", size = 4.5) +
    geom_text(aes(x = as.Date("2019-10-05"), y = 0.4, label = "Demand"), color = "black", size = 4.5) +
    geom_text(aes(x = as.Date("2019-09-01"), y = 0.8, label = "Avg listing price"), color = "darkgreen", size = 4.5)
  
  