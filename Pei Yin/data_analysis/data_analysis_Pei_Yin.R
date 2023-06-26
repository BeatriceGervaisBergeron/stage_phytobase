# Analysis script

library(vegan)
library(HH)


#### call your data  ####
data_std <- read.csv('./Pei yin/data_cleaning_final/data_std_cleaned.csv', sep=',',header = T, dec = '.')

## join the traits to your data 
traits <- readRDS('./complete_data.rds')
data_std <- left_join(data_std , traits, by=c('AccSpeciesName_cor'='sp'))


##### PCA of willows vs all species ####







