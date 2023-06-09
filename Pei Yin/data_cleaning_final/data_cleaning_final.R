# DATA CLEANING

## LIBRARY

library(dplyr)
library(stringr)
library(taxize)



#### call your data  ####

data_2 <- read.csv('./Pei Yin/data_cleaning_final/soil_sp_database_Pei_Yin_Copy.csv', sep=',', header = T, dec = '.')


#### decimals ####
# make sure there is not comma instead of points
# already with points

# make sure all column are in the correct forms (character or numerical)
str(data_2)


# transform variable that needed
# Transform data
data <- data %>%
  mutate(
    expe_t = as.numeric(expe_t)
    , ph = as.numeric(ph)
    , clay = as.numeric(clay)
    , pb_s = as.numeric(pb_s)
    , co_s = as.numeric(co_s)
    , mn_s = as.numeric(mn_s)
    , hg_s = as.numeric(hg_s)
    , co_ba = as.numeric(co_ba)
    , hg_ba = as.numeric(hg_ba)
    , co_br = as.numeric(co_br)
    , mn_br = as.numeric(mn_br)
    , hg_br = as.numeric(hg_br)
    , as_ba.1 = as.numeric(as_ba.1)
    , zn_ba.1 = as.numeric(zn_ba.1)
    , se_ba.1 = as.numeric(se_ba.1)
    , co_ba.1 = as.numeric(co_ba.1)
    , mn_ba.1 = as.numeric(mn_ba.1)
    , hg_ba.1 = as.numeric(hg_ba.1))
# here all value for ph and clay are not only numerical, so Na were introduced. Those column need to be adjusted

# Verify the mutation
str(data) # good


#### all white space to NA ####
data[data == ''] <- NA




