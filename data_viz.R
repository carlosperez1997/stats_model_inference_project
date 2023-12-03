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

ggplot(train, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins=20, fill = "lightblue", color = "black") +
  geom_density(alpha = 0.2, fill = "blue") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, max(train$price), by = 1000000)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y.left=element_text(color="black", size=14),
    axis.text.y.left=element_text(color="black", size=12),
    axis.title.y.right=element_text(color="black", size=14),
    axis.text.y.right=element_text(color="black", size=14),
    axis.text.x=element_text(size=12),
    axis.title.x=element_text(size=16)
  ) +
  labs(title="KDE plot and histogram of price", y='Density', x="Price [AED]") + 
  theme_minimal()

ggplot() +
  geom_density(data = train, aes(x = price, fill = "Train"), alpha = 0.2) +
  geom_density(data = test, aes(x = price, fill = "Test"), alpha = 0.2) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, max(train$price), by = 1000000)) +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "green")) +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14),
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14)
  ) +
  labs(title = "KDE plot of prices of listings", y = 'Density', x = "Price [AED]")
  

# community:
# community_Jumeirah.Beach.Residence..JBR. 
# community_Jumeirah.Lake.Towers..JLT. 
# community_Jumeirah.Village.Circle..JVC.
train[, community := 'Dubai Marina']
train <- train %>%
  .[community_Jumeirah.Beach.Residence..JBR.==T, community := 'Jumeirah Beach \n Residence (JBR)'] %>%
  .[community_Jumeirah.Lake.Towers..JLT.==T, community := 'Jumeirah Lake \n Towers (JLT)'] %>%
  .[community_Jumeirah.Village.Circle..JVC.==T, community := 'Jumeirah Village \n Circle (JVC)']

train %>% count(community)

p <- ggplot(train, aes(y=community, x=price, color=community)) + 
  geom_violin(aes(fill = community,
                  fill = after_scale(colorspace::lighten(fill, .2))),
              draw_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
              size = 0.2, scale='width', colour = "black") +
  #geom_jitter(aes(fill = community), height = 0.5, width = 0.1, alpha=0.05) + 
  theme_minimal() + 
  theme(legend.position="none",
        text = element_text(size = 16),  # Adjust the font size here
        axis.title = element_text(size = 16),  # Adjust the title font size
        axis.text = element_text(size = 10),  # Adjust the axis text font size
        axis.ticks = element_line(size = 0.8),
        axis.title.y.left=element_text(color="black", size=14),
        axis.text.y.left=element_text(color="black", size=12),
        axis.title.y.right=element_text(color="black", size=14),
        axis.text.y.right=element_text(color="black", size=14),
        axis.text.x=element_text(size=12),
        axis.title.x=element_text(size=16),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) +  # Adjust the axis ticks size
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
