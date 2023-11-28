library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)

DIR <- '/Users/carlosperezricardo/Downloads/'

train <- read.csv(paste0(DIR,'realestate_train.csv')) %>% as.data.table
test <- read.csv(paste0(DIR,'realestate_test.csv')) %>% as.data.table
train$X <- NULL
test$X <- NULL

train %>% select(date_added_start, price)

# community:
# community_Jumeirah.Beach.Residence..JBR. 
# community_Jumeirah.Lake.Towers..JLT. 
# community_Jumeirah.Village.Circle..JVC.
train[, community := 'Dubai Marina']
train <- train %>%
  .[community_Jumeirah.Beach.Residence..JBR.==T, community := 'Jumeirah Beach \n Residence (JBR)'] %>%
  .[community_Jumeirah.Lake.Towers..JLT.==T, community := 'Jumeirah Lake \n Towers (JLT)'] %>%
  .[community_Jumeirah.Village.Circle..JVC.==T, community := 'Jumeirah Village \n Circle (JVC)']

p <- ggplot(train, aes(y=community, x=price, color=community)) + geom_boxplot() +
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(size = 16),  # Adjust the font size here
        axis.title = element_text(size = 16),  # Adjust the title font size
        axis.text = element_text(size = 10),  # Adjust the axis text font size
        axis.ticks = element_line(size = 0.7)) +  # Adjust the axis ticks size
  scale_x_continuous(labels = scales::comma) +
  labs(title="Prices of listed properties in each community", y='', x="Price [AED]") #+
  #theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
p

# property type
# property_type_Townhouse property_type_Villa
train[, property_type := 'Apartment']
train <- train %>%
  .[property_type_Townhouse==T, property_type := 'Townhouse'] %>%
  .[property_type_Villa==T, property_type := 'Villa']

p <- ggplot(train, aes(y=property_type, x=price, color=property_type)) + geom_boxplot() +
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(size = 16),  # Adjust the font size here
        axis.title = element_text(size = 16),  # Adjust the title font size
        axis.text = element_text(size = 10),  # Adjust the axis text font size
        axis.ticks = element_line(size = 0.7)) +  # Adjust the axis ticks size
  scale_x_continuous(labels = scales::comma) +
  labs(title="Prices of listed properties for each property type", y='', x="Price [AED]") #+
#theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
p

numeric_train <- train[, .SD, .SDcols = sapply(train, is.numeric)]
numeric_train <- numeric_train[, .SD, .SDcols = sapply(numeric_train, sd) > 0]

# Binary columns
columns_to_drop <- names(numeric_train)[sapply(numeric_train, function(col) all(col %in% c(0, 1)))] 
length(columns_to_drop)
columns_to_drop
numeric_train[, (columns_to_drop) := NULL]

# Other columns
columns_to_drop <- grep("lt", names(numeric_train), value = TRUE)
columns_to_drop <- c(columns_to_drop, grep("3m", names(numeric_train), value = TRUE))
columns_to_drop <- c(columns_to_drop, grep("6m", names(numeric_train), value = TRUE))
numeric_train[, (columns_to_drop) := NULL]
dim(numeric_train)

heatmap(cor(numeric_train),Rowv = NA, Colv = NA)
