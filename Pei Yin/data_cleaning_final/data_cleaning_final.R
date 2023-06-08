# DATA CLEANING

## LIBRARY

library(dplyr)
library(stringr)
library(taxize)



#### call your data  ####
data <- read.csv('./Pei yin/soil_sp_database_Pei_Yin.csv', sep=',',header = T, dec = '.')



#### decimals ####
# make sure there is not comma instead of points
# already with points

# make sure all column are in the correct forms (character or numerical)
str(data)






