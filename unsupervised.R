library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)
library(factoextra)

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

nulls <- colSums(is.na(emirate_offer))
print(nulls[nulls!=0])

# Drop offplan (many nulls)
cols <- setdiff(names(emirate_offer),  grep('offplan', names(emirate_offer), value=T))
emirate_offer <- emirate_offer[, .SD, .SDcols = cols]

nulls <- colSums(is.na(emirate_offer))
print(nulls[nulls!=0])

nulls <- colSums(is.na(emirate_demand))
print(nulls[nulls!=0])

emirate_demand <- drop_X(read.csv(paste0(DIR,'demand_emirate.csv')) %>% as.data.table)
community_demand <- drop_X(read.csv(paste0(DIR,'demand_by_community.csv')) %>% as.data.table)
# Drop repeated count in demand
emirate_demand$emirate_price_area_count <- NULL
community_demand$community_price_area_count <- NULL

cat('Offer:', dim(emirate_offer),':', dim(community_offer))
cat('Demand:', dim(emirate_demand),':', dim(community_demand))

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
  
  data.pca <- princomp(cov(scale(df[, .SD, .SDcols = cols])))
  data_for_pca <- df[, .SD, .SDcols = cols]
  scaled_data <- scale(data_for_pca)
  pca_result <- prcomp(scaled_data)
  pca_transformed_data <- predict(pca_result)[,1:5]
  
  return(list(pca = data.pca, transf_data = pca_transformed_data))
}

# OFFER
x <- perform_pca(emirate_offer_)
data.pca <- x$pca
pca_transformed_data <- x$transf_data
summary(data.pca)

loadings <- data.pca$loadings
print(abs(loadings[, 1]) %>% sort(decreasing=T) %>% head(5))

fviz_eig(data.pca, addlabels = TRUE)
#fviz_pca_var(data.pca, col.var = "black")

# Be careful => PCA can have negative relationship with index
# We should do sth to decide the direction
emirate_offer_transformed <- cbind(emirate_offer_, 
                                   data.table(PC1_offer = -pca_transformed_data[,1], 
                                              PC2_offer = pca_transformed_data[,2],
                                              PC3_offer = pca_transformed_data[,3])) %>%
  .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]


# DEMAND
x <- perform_pca(emirate_demand_)
data.pca <- x$pca
pca_transformed_data <- x$transf_data
summary(data.pca)

loadings <- data.pca$loadings
print(abs(loadings[, 1]) %>% sort(decreasing=T) %>% head(5))

fviz_eig(data.pca, addlabels = TRUE)
#fviz_pca_var(data.pca, col.var = "black")

# Be careful => PCA component can have negative relationship with index
emirate_demand_transformed <- cbind(emirate_demand_, 
                                   data.table(PC1_demand = pca_transformed_data[,1], 
                                              PC2_demand = pca_transformed_data[,2],
                                              PC3_demand = pca_transformed_data[,3])) %>%
  .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]

# Offer-demand combined
offer_demand <- merge(emirate_demand_, emirate_offer_, by=c('dt_year', 'dt_month'), all.x=T, all.y=T, suffixes=c('_demand', '_offer'))
offer_demand <- offer_demand %>% drop_na()
dim(offer_demand)

x <- perform_pca(offer_demand)
data.pca <- x$pca
pca_transformed_data <- x$transf_data
summary(data.pca)
fviz_eig(data.pca, addlabels = TRUE)

#### VALIDATE WITH INDEX
index <- read.csv(paste0(DIR, 'residential_index.csv')) %>% 
  as.data.table %>% .[, date := lubridate::as_date(DateTime)]

plot_data <- merge(emirate_demand_transformed, index, by='date', all.x=T, all.y=T)
plot_data <- merge(plot_data, emirate_offer_transformed, by='date', all.x=T, all.y=T)
plot_data <- plot_data[ date <= '2022-11-01']

scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Scale to 0 and 1 and Moving average to smooth
pca_cols <- grep("PC", names(plot_data), value=T)
for (col in pca_cols) {
  plot_data[, (col) := scale_to_01(get(col))]
  moving_avg_col <- paste0(col, "_ma")
  plot_data[, (moving_avg_col) := frollmean(get(col), 3, align = "right", fill = NA)]
}

reg <- lm(Index ~ PC1_offer+PC2_offer+PC3_offer+PC1_demand+PC2_demand+PC3_demand, plot_data)
summary(reg)

# Regression to Index is quite good!
reg <- lm(Index ~ PC1_offer_ma+PC2_offer_ma+PC3_offer_ma+PC1_demand_ma+PC2_demand_ma+PC3_demand_ma, plot_data)
summary(reg)

# Offer: black - Demand: Blue 
scaleFactor <-  max(plot_data$PC1_offer, na.rm=T) / max(plot_data$Index, na.rm=T)
ggplot(plot_data, aes(x=date)) +
  geom_point(aes(y=PC1_offer), col="blue", alpha=0.3) +
  geom_point(aes(y=PC1_demand), col="black", alpha=0.3) +
  geom_line(aes(y=PC1_offer_ma), col="blue") +
  geom_line(aes(y=PC1_demand_ma), col="black") +
  #geom_line(aes(y=PC2_ma), col="steelblue") +
  #geom_line(aes(y=PC3_ma), col="yellow") +
  geom_line(aes(y=Index * scaleFactor), col="red") +
  scale_y_continuous(name="Generated Indexes", sec.axis=sec_axis(~./scaleFactor, name="Official Index")) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  ) + geom_text(aes(x = as.Date("2018-01-01"), y = 0.8, label = "Offer"), color = "blue") +
  geom_text(aes(x = as.Date("2016-01-05"), y = 0.5, label = "Demand"), color = "black") +
  geom_text(aes(x = as.Date("2013-01-01"), y = 0.9, label = "Real Estate Index"), color = "red")


###### COMMUNITY
emirate_offer <- drop_X(read.csv(paste0(DIR,'offer_by_community.csv')) %>% as.data.table)
emirate_demand <- drop_X(read.csv(paste0(DIR,'demand_by_community.csv')) %>% as.data.table)

community_ <- "Jumeirah Lake Towers (JLT)" #"Dubai Marina" "Jumeirah Lake Towers (JLT)" "Jumeirah Beach Residence (JBR)"
lags <- c(3, 6, 12)

community_offer_ <- lag_percentage(community_offer, lags, 'community')
community_demand_ <- lag_percentage(community_demand, lags, 'community')

### Drop offplan in offer (many nulls)
cols <- setdiff(names(community_offer),  grep('offplan', names(community_offer), value=T))
community_offer <- community_offer[, .SD, .SDcols = cols]

nulls <- colSums(is.na(community_offer))
print(nulls[nulls!=0])
###

setindex(community_offer_, dt_year, dt_month, community)
community_offer_ <- community_offer_ %>% drop_na()
setindex(community_demand_, dt_year, dt_month)
community_demand_ <- community_demand_ %>% drop_na()

# Subset
community_offer_ <- community_offer_[community == community_]
community_demand_ <- community_demand_[community == community_]

cat('Community', dim(community_offer_),':', dim(community_demand_))

# Offer
x <- perform_pca(community_offer_)
data.pca <- x$pca
pca_transformed_data <- x$transf_data
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)

community_offer_transformed <- cbind(community_offer_, 
                                   data.table(PC1_offer = -pca_transformed_data[,1], 
                                              PC2_offer = pca_transformed_data[,2],
                                              PC3_offer = pca_transformed_data[,3])) %>%
  .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]

# Demand
x <- perform_pca(community_demand_)
data.pca <- x$pca
pca_transformed_data <- x$transf_data
summary(data.pca)

fviz_eig(data.pca, addlabels = TRUE)

community_demand_transformed <- cbind(community_demand_, 
                                      data.table(PC1_demand = -pca_transformed_data[,1], 
                                                 PC2_demand = pca_transformed_data[,2],
                                                 PC3_demand = pca_transformed_data[,3])) %>%
  .[, date := as.Date(paste(dt_year, dt_month, "01", sep = "-"))]

# JOIN
plot_data <- merge(community_demand_transformed, community_offer_transformed, by='date', all.x=T, all.y=T)
plot_data <- merge(plot_data, index, by='date', all.x=T, all.y=T)
plot_data <- plot_data[ date <= '2022-11-01']

scale_to_01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Scale to 0 and 1 and Moving average to smooth
pca_cols <- grep("PC", names(plot_data), value=T)
for (col in pca_cols) {
  plot_data[, (col) := scale_to_01(get(col))]
  moving_avg_col <- paste0(col, "_ma")
  plot_data[, (moving_avg_col) := frollmean(get(col), 3, align = "right", fill = NA)]
}

# Offer: black - Demand: Blue 
scaleFactor <-  max(plot_data$PC1_offer, na.rm=T) / max(plot_data$Index, na.rm=T)
ggplot(plot_data, aes(x=date)) +
  geom_point(aes(y=PC1_offer), col="blue", alpha=0.3) +
  geom_point(aes(y=PC1_demand), col="black", alpha=0.3) +
  geom_line(aes(y=PC1_offer_ma), col="blue") +
  geom_line(aes(y=PC1_demand_ma), col="black") +
  #geom_line(aes(y=PC2_ma), col="steelblue") +
  #geom_line(aes(y=PC3_ma), col="yellow") +
  geom_line(aes(y=Index * scaleFactor), col="red") +
  scale_y_continuous(name="Generated Indexes", sec.axis=sec_axis(~./scaleFactor, name="Official Index")) +
  labs(title=community_) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  ) + geom_text(aes(x = as.Date("2018-01-01"), y = 0.8, label = "Offer"), color = "blue") +
  geom_text(aes(x = as.Date("2016-01-05"), y = 0.5, label = "Demand"), color = "black")

