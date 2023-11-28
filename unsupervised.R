library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)
library(readr)
library(tidyverse)

DIR <- '/Users/carlosperezricardo/Downloads/'

emirate_offer <- read.csv(paste0(DIR,'offer_emirate.csv')) %>% as.data.table
community_offer <- read.csv(paste0(DIR,'offer_by_community.csv')) %>% as.data.table

emirate_demand <- read.csv(paste0(DIR,'demand_emirate.csv')) %>% as.data.table
community_demand <- read.csv(paste0(DIR,'demand_by_community.csv')) %>% as.data.table



