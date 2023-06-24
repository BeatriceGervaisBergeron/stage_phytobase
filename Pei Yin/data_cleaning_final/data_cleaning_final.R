# DATA CLEANING

## LIBRARY

library(dplyr)
library(stringr)
library(taxize)



#### call your data  ####

data <- read.csv('./Pei Yin/data_cleaning_final/soil_sp_database_Pei_Yin_Copy.csv', sep=',', header = T, dec = '.')


#### decimals ####
# make sure there is not comma instead of points
# already with points

# make sure all column are in the correct forms (character or numerical)
str(data)


#### transform variable that needed ####
# Transform data
data <- data %>%
  mutate(
    Experiment_T = as.numeric(Experiment_T)
    , pb_s = as.numeric(pb_s)
    , mn_s = as.numeric(mn_s)
    , hg_s = as.numeric(hg_s)
    , as_ba = as.numeric(as_ba)
    , pb_ba = as.numeric(pb_ba)
    , ni_ba = as.numeric(ni_ba)
    , co_ba = as.numeric(co_ba)
    , hg_ba = as.numeric(hg_ba)
    , co_br = as.numeric(co_br)
    , hg_br = as.numeric(hg_br)
    , cu_br.1 = as.numeric(cu_br.1)
    , zn_br.1 = as.numeric(zn_br.1)
    , se_br.1 = as.numeric(se_br.1)
    , ni_br.1 = as.numeric(ni_br.1)
    , co_br.1 = as.numeric(co_br.1)
    , mn_br.1 = as.numeric(mn_br.1)
    , cr_br.1 = as.numeric(cr_br.1)
    , hg_br.1 = as.numeric(hg_br.1)
    , as_br.2 = as.numeric(as_br.2)
    , cd_br.2 = as.numeric(cd_br.2)
    , cu_br.2 = as.numeric(cu_br.2)
    , pb_br.2 = as.numeric(pb_br.2)
    , zn_br.2 = as.numeric(zn_br.2)
    , se_br.2 = as.numeric(se_br.2)
    , ni_br.2 = as.numeric(ni_br.2)
    , co_br.2 = as.numeric(co_br.2)
    , mn_br.2 = as.numeric(mn_br.2)
    , cr_br.2 = as.numeric(cr_br.2)
    , hg_br.2 = as.numeric(hg_br.2)
    , as_br.3 = as.numeric(as_br.3)
    , cd_br.3 = as.numeric(cd_br.3)
    , cu_br.3 = as.numeric(cu_br.3)
    , pb_br.3 = as.numeric(pb_br.3)
    , zn_br.3 = as.numeric(zn_br.3)
    , se_br.3 = as.numeric(se_br.3)
    , ni_br.3 = as.numeric(ni_br.3)
    , co_br.3 = as.numeric(co_br.3)
    , mn_br.3 = as.numeric(mn_br.3)
    , cr_br.3 = as.numeric(cr_br.3)
    , hg_br.3 = as.numeric(hg_br.3)
    , as_ba = as.numeric(as_ba)
    , cd_ba = as.numeric(cd_ba)
    , cu_ba = as.numeric(cu_ba)
    , pb_ba = as.numeric(pb_ba)
    , zn_ba = as.numeric(zn_ba)
    , se_ba = as.numeric(se_ba)
    , ni_ba = as.numeric(ni_ba)
    , co_ba = as.numeric(co_ba)
    , mn_ba = as.numeric(mn_ba)
    , cr_ba = as.numeric(cr_ba)
    , hg_ba = as.numeric(hg_ba)
    , as_ba.1 = as.numeric(as_ba.1)
    , cd_ba.1 = as.numeric(cd_ba.1)
    , cu_ba.1 = as.numeric(cu_ba.1)
    , pb_ba.1 = as.numeric(pb_ba.1)
    , zn_ba.1 = as.numeric(zn_ba.1)
    , se_ba.1 = as.numeric(se_ba.1)
    , ni_ba.1 = as.numeric(ni_ba.1)
    , co_ba.1 = as.numeric(co_ba.1)
    , mn_ba.1 = as.numeric(mn_ba.1)
    , cr_ba.1 = as.numeric(cr_ba.1)
    , hg_ba.1 = as.numeric(hg_ba.1))
# here all value for ph and clay are not only numerical, so Na were introduced. Those column need to be adjusted

# Verify the mutation
str(data) # good


#### all white space to NA ####
data[data == ''] <- NA



#### species names cleaning  ####

# check unique sp list in your database

uni_sp<-as.data.frame(unique(data$name)) # 42 unique species
colnames(uni_sp) <- c('sp')

# Correct the name according to the species name corrected from TRY

# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')

# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 20 species not on the corrected list, so need to be corrected

# Resolve the unmatched name with the 4 databases selected:
# "The International Plant Names Index",'USDA NRCS PLANTS Database',"Tropicos - Missouri Botanical Garden", 'Catalogue of Life'
match.name <- to.be.cor$sp %>%
  gnr_resolve(data_source_ids = c(1,150,165,167), 
              with_canonical_ranks=T)

# provide a short summary table
matches <- match.name %>%
  select(user_supplied_name,submitted_name, matched_name2, score)%>% # 19 matches
  distinct()

# Are all species considered in the correction
uni_sp_2<- as.data.frame(unique(match.name$user_supplied_name)) # 19 sp, so 1 sp was not taken into account
colnames(uni_sp_2) <- c('sp')

# which are not included
unmatch <- to.be.cor %>% filter(!sp %in% uni_sp_2$sp)
colnames(unmatch) <- c('user_supplied_name')
# add them to the end of the match list
matches.all<-bind_rows(matches, unmatch)


# Insert three new columns (change names as you like) and insert text:
# ‘implement’ - should the name suggested by GNR be used? (TRUE/FALSE)?
# ‘alternative’ - write an alternative name here
# ‘dupl’ - Is this entry a duplicate with other name in this list (TRUE/FALSE)?

matches.all$implement <- ''  # add the column "implement"
matches.all$alternative <- '' # add the column "alternative"
matches.all$dupl <-'' # add the column "dupl"


# write it back as a table for manual correction in Excel
write.table(matches.all,
            "./Pei Yin/data_cleaning_final/uni_sp_match_names_2.txt", 
            sep="\t", row.names = F, quote = F)

# open the txt file in excel to make manual corrections
# All in () content should be remove
# For all hybrids (with x) both name should be keep

# Save the corrected names in a txt file name, adding _cor to the name of the document

# import back the data 
uni_sp_cor <- read.table("./Pei Yin/data_cleaning_final/uni_sp_match_names_cor.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# eliminate duplicates
uni_sp_cor$dupl <- duplicated(uni_sp_cor$user_supplied_name)
uni_sp_cor <- uni_sp_cor %>%  filter(!dupl ==T) # no duplicates in the first column

# Save the final corrected list in an rds object
saveRDS(uni_sp_cor, file='Pei Yin/data_cleaning_final/salix_sp_cor.rds')


#### Join list of corrected names ####

# call the newly corrected list
salix_sp_cor <- readRDS('Pei Yin/data_cleaning_final/salix_sp_cor.rds')

# Add the new corrected list to the complete on
list_sp_cor_salix <- bind_rows(salix_sp_cor, list_sp_cor)

# add the corrections to the data
data <- data %>% 
  left_join(list_sp_cor_salix, by=c('name'= 'user_supplied_name'))

# make a new column of the correct names
data <- data %>% 
  mutate(AccSpeciesName_cor = ifelse(implement == T, alternative, submitted_name)) 

# Keep the corrected column
data <- data[,-c(162:168)] # should have 162 columns

# Check number of sp now
uni_salix_cor <- unique(data$AccSpeciesName_cor) # 42 sp
# same number of sp, if you return to the uni_sp with 42 sp, so no lines with redundant names


#### standardized units ####

# Select all the units column
units <- data %>%
  select(contains('unit'))
# check unit conversion for every unit column
colnames(units)

#"om_units"
unique(units$om_units) # "%" "" "g kg-1" 
# need to convert g kg-1 to % (/10)
data_std <- data %>%
  mutate(om = ifelse(om_units == 'g kg-1', om/10, om)) %>% # divide per 10 all the om data that have g kg-1 units
  mutate(om_units = ifelse(om_units == 'g kg-1', '%', om_units)) # replace all g kg-1 units per %
# verify
unique(data_std$om_units) # only "%" and ""


#"oc_units"
unique(units$oc_units) # ""     "g/kg"    "%"
# need to convert g/kg to % (/10)
data_std <- data %>%
  mutate(oc = ifelse(oc_units == 'g/kg', oc/10, oc)) %>% # divide per 10 all the oc data that have g/kg units
  mutate(oc_units = ifelse(oc_units == 'g/kg', '%', oc_units)) # replace all g/kg units per %
# verify
unique(data_std$oc_units) # only "%"

#"clay_units" 
unique(units$clay_units) # "%" "" 
# no need for conversion if only %

#"sand_units" 
unique(units$sand_units) #  "%" "" 
# no need for conversion if only %

#"ec_units"
unique(units$ec_units) # ""  "mS cm−1"
# no need for conversion if only mS cm−1 

#"cec_units"
unique(units$cec_units) # "meq 100 g-1" "cmolc/kg"  "NA"  "mmol kg-1"  "cmol/kg"  "mmol(+)/kg"  "cmol kg-1"
## need to convert all units to "cmolc kg-1"
# replace "meq 100 g-1" by 'cmolc kg-1'
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units == 'meq 100 g-1', 'cmolc kg-1', cec_units)) # replace all 'meq 100 g-1' units by 'cmolc kg-1'
# replace 'cmolc/kg' by 'cmolc kg-1'
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units == 'cmolc/kg', 'cmolc kg-1', cec_units)) # replace all 'cmolc/kg' units by 'cmolc kg-1'
# need to convert mmol kg-1 to cmolc kg-1 (/10)
data_std <- data_std %>%
  mutate(cec = ifelse(cec_units == 'mmol kg-1', cec/10, cec)) %>% # divide per 10 all the cec data that have mmol/kg units
  mutate(cec_units = ifelse(cec_units == 'mmol kg-1', 'cmolc kg-1', cec_units)) # replace all 'mmol kg-1' units by 'cmolc kg-1'
# replace 'cmol/kg' by 'cmolc kg-1'
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units == 'cmol/kg', 'cmolc kg-1', cec_units)) # replace all 'cmol/kg' units by 'cmolc kg-1'
# need to convert 'mmol(+)/kg' to 'cmolc kg-1'
data_std <- data_std %>%
  mutate(cec = ifelse(cec_units == 'mmol(+)/kg', cec/10, cec)) %>% # divide per 10 all the cec data that have 'mmol(+)/kg' units
  mutate(cec_units = ifelse(cec_units == 'mmol(+)/kg', 'cmolc kg-1', cec_units)) # replace all 'mmol(+)/kg' units by 'cmolc kg-1'
# replace 'cmol kg-1' by 'cmolc kg-1'
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units == 'cmol kg-1', 'cmolc kg-1', cec_units)) # replace all 'cmol kg-1' units by 'cmolc kg-1'
#verify
unique(data_std$cec_units)# only 'cmolc kg-1'

#"N_units"
unique(units$N_units) # "%"   ""    "mg kg-1"
# need to convert % to mg kg-1 (*10000)
data_std <- data_std %>%
  mutate(N = ifelse(N_units == '%', N*10000, N)) %>% # multiply per 10000 all the n data that have % units
  mutate(N_units = ifelse(N_units == '%', 'mg kg-1', N_units)) # replace all % per g/kg units 
#verify
unique(data_std$N_units) # only mg kg-1

#"P_units"   
unique(units$P_units) # "mg kg-1"    ""
# no need for conversion if only 'mg kg-1'

#"units_s"   
unique(units$units_s) # "mg kg-1"
# no need for conversion if only 'mg kg-1'

#"units_b" 
unique(units$units_b) # "g m-2"   "g"   ""    "g/plant"  " t ha-1 yr-1" "kg/ha"   "g/pot" 
# since biomass is measure really differently, for the moment we'll let it like that
# 'on hold', to be unify if needed further on

#"units_te_ba"
unique(units$units_te_ba) # "mg kg-1" "?g g-1"
# replace '?g g-1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(units_te_ba = ifelse(units_te_ba == '?g g-1' , 'mg kg-1', units_te_ba))
# verify
unique(data_std$units_te_ba) # only "mg kg-1"

# "units_te_br"   
unique(units$units_te_br) # "mg kg-1"    NA    "?g g-1"
# replace '?g g-1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(units_te_br = ifelse(units_te_br == '?g g-1' , 'mg kg-1', units_te_br))
# verify
unique(data_std$units_te_br) # only "mg kg-1" "" 

# "units_te_br.1"
unique(units$units_te_br.1) # "mg kg-1"    NA
# no need for conversion if only mg kg-1

# "units_te_br.2"
unique(units$units_te_br.2) # "mg kg-1"    NA
# no need for conversion if only mg kg-1

# "units_te_br.3"
unique(units$units_te_br.3) # "mg kg-1"    NA
# no need for conversion if only mg kg-1

#"units_te_ba.2"
unique(units$units_te_ba.2) # "mg kg-1" "?g g-1"
# replace '?g g-1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(units_te_ba.2 = ifelse(units_te_ba.2 == '?g g-1' , 'mg kg-1', units_te_ba.2))
# verify
unique(data_std$units_te_ba.2) # only "mg kg-1"

#"units_te_ba.1"
unique(units$units_te_ba.1) # "mg kg-1" "?g g-1"
# replace '?g g-1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(units_te_ba.1 = ifelse(units_te_ba.1 == '?g g-1' , 'mg kg-1', units_te_ba.1))
# verify
unique(data_std$units_te_ba.1) # only "mg kg-1"


#### standardize categories terms ####

# Climate
unique(data$climate) # "temperate"  "transfer-featured climate between continental climate and temperate climate"  "humid subtropical"
# 'on hold', to be unify if needed further on

# Texture
unique(data$texture) # ""   "fine sandy loam"   "Clay sand silt"    "Loamy"   "Coarse-textured, low content of clay"   "Clay"
#  "Coarse-textured" is for two classes, need to be checked and if not more precise, make a category in between for eventual %

# p_density
unique(data$p_density..ind..m2.or.pots.)
# 'on hold', to be unify if needed further on

# organs_ba, organs_ba.1 and organs_ba.2
unique(data_std$organs_ba) # "Shoots"  "leaf"   "leaves"   "Stems"
unique(data_std$organs_ba.1) # "Wood"
unique(data_std$organs_ba.2) # "Stalks" "Twigs"  "Stems"
# conversion for only shoots or leaves or stems    
syn_shoots <- c("Shoots")
syn_stems <- c("Stalks", "Twigs" , "Stems")
syn_leaves <- c("leaf", "leaves")

data_std <- data_std %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_shoots , 'shoots', organs_ba)) %>% # replace all by shoots 
  mutate(organs_ba = ifelse(organs_ba %in% syn_stems , 'stems', organs_ba)) %>% # replace all by stems
  mutate(organs_ba = ifelse(organs_ba %in% syn_leaves , 'leaves', organs_ba)) %>% # replace all by leaves
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_shoots , 'shoots', organs_ba.1)) %>% # replace all by shoots 
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_stems , 'stems', organs_ba.1)) %>% # replace all by stems
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_leaves , 'leaves', organs_ba.1)) %>% # replace all by leaves
  mutate(organs_ba.2 = ifelse(organs_ba.2 %in% syn_shoots , 'shoots', organs_ba.2)) %>% # replace all by shoots 
  mutate(organs_ba.2 = ifelse(organs_ba.2 %in% syn_stems , 'stems', organs_ba.2)) %>% # replace all by stems
  mutate(organs_ba.2 = ifelse(organs_ba.2 %in% syn_leaves , 'leaves', organs_ba.2)) # replace all by leaves
# verify
unique(data_std$organs_ba) # only shoots or leaves or stems
unique(data_std$organs_ba.1) # wood             ***TO CONTINUE***
unique(data_std$organs_ba.2) # only stems


# organs_br, organs_br.1, organs_br.2 and organs_br.3

unique(data_std$organs_br) # "Roots"
unique(data_std$organs_br.1) # "Coarse roots"          *** TO CONTINUE ***
unique(data_std$organs_br.2) # "Fine roots"           *** TO CONTINUE ***
unique(data_std$organs_br.3) # "Stump"               *** TO CONTINUE ***
# conversion for only roots
syn_roots <- c("Roots")

data_std <- data_std %>%
  mutate(organs_br = ifelse(organs_br %in% syn_roots , 'roots', organs_br)) # replace all by roots
# verify
unique(data_std$organs_br) # "roots"


#### outliers and errors in numerical data ####

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables means 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # keep only numerical data, so 112 variables

# import the data normal range
num_range <- read.table("./numerical_range_variables.txt", 
                        sep="\t", header=T, stringsAsFactors = F)


# decimals

# make sure there is no comma instead of points
# open "num_range" and look at the decimals

# the values of "2,4" and "6,0" of the 8th line "oc" are with commas
num_range[8,2] # "2,4"
num_range[8,3] # "6,0"

# need to change the commas to points
num_range[8,2] <- "2.4"
num_range[8,3] <- "6.0"
# verify
num_range[8,2] # "2.4"
num_range[8,3] # "6.0"


# data types of "num_range"

str(num_range) 
# variables: chr
# min_value: chr
# max_value: chr
# sources  : chr

# so the data type of columns "min_value" and "max_value" are chr
str(num_range$min_value) # chr
str(num_range$max_value) # chr

# transform "min_value" and "max_value" as numeric

num_range <- num_range %>%
  mutate(
    min_value = as.numeric(min_value)
    , max_value = as.numeric(max_value))
# verify
str(num_range$min_value) # num
str(num_range$max_value) # num


# data types of "data_num"

str(data_num)

# Transform the data in "data_num" as numeric
data_num <- data_num %>%
  mutate(
    covidence = as.numeric(covidence)
    , year = as.numeric(year)
    , n_s = as.numeric(n_s)
    , n_te_ba = as.numeric(n_te_ba)
    , n_te_br = as.numeric(n_te_br)
    , n_te_br.1 = as.numeric(n_te_br.1)
    , n_te_br.2 = as.numeric(n_te_br.2)
    , n_te_br.3 = as.numeric(n_te_br.3)
    , n_te_ba.2 = as.numeric(n_te_ba.2)
    , n_te_ba.1 = as.numeric(n_te_ba.1))

# verify
str(data_num)



# data_num has 112 variables for now

# remove the columns "season_exposure" and "day_exposure", 
# since no data of "min_value" or "max_value" to compare to, in "num_range" (i.e. no outlier)
data_num <- data_num[,-c(6:7)] # should have 110 columns



# check outliers

# for each of the 112 variables, isolate data that are outside the range
# here are the 112 variables
list <-colnames(data_num)

# isolate the outliers lines for the variable 'covidence'
outliers <- data_num %>% 
  filter(covidence < num_range$min_value[num_range$variables == 'covidence'] | covidence > num_range$max_value[num_range$variables == 'covidence'] )
# if the outliers has 0 lines, it indicate not apparent outliers

# you can also write it with number from the list to save time, as follow with Covidence as number 1 in the list
outliers <- data_num %>% 
  filter(data_num[,1] < num_range$min_value[1] | data_num[,1] > num_range$max_value[1] )
# 0 line/0 obs, so no outliers

# outliers for list[2] = year 
outliers <- data_num %>% 
  filter(data_num[,2] < num_range$min_value[2] | data_num[,2] > num_range$max_value[2] )
# 0 line/0 obs, so no outliers

# outliers for list[3] = Experiment_T 
outliers <- data_num %>% 
  filter(data_num[,3] < num_range$min_value[3] | data_num[,3] > num_range$max_value[3] )
# 0 line/0 obs, so no outliers

# outliers for list[4] = mat..C.
outliers <- data_num %>% 
  filter(data_num[,4] < num_range$min_value[4] | data_num[,4] > num_range$max_value[4] )
# 0 line/0 obs, so no outliers

# outliers for list[5] = map..mm.
outliers <- data_num %>% 
  filter(data_num[,5] < num_range$min_value[5] | data_num[,5] > num_range$max_value[5] )
# 0 line/0 obs, so no outliers

# outliers for list[6] = ph
outliers <- data_num %>% 
  filter(data_num[,6] < num_range$min_value[6] | data_num[,6] > num_range$max_value[6] )
# 0 line/0 obs, so no outliers

# outliers for list[7] = om
outliers <- data_num %>% 
  filter(data_num[,7] < num_range$min_value[7] | data_num[,7] > num_range$max_value[7] )
# 0 line/0 obs, so no outliers

# outliers for list[8] = oc
outliers <- data_num %>% 
  filter(data_num[,8] < num_range$min_value[8] | data_num[,8] > num_range$max_value[8] )
# 47 lines/47 obs, so 47 outliers to verify

# outliers for list[9] = clay
outliers <- data_num %>% 
  filter(data_num[,9] < num_range$min_value[9] | data_num[,9] > num_range$max_value[9] )
# 0 line/0 obs, so no outliers

# outliers for list[10] = sand
outliers <- data_num %>% 
  filter(data_num[,10] < num_range$min_value[10] | data_num[,10] > num_range$max_value[10] )
# 0 line/0 obs, so no outliers

# outliers for list[11] = ec..dsm.1.
outliers <- data_num %>% 
  filter(data_num[,11] < num_range$min_value[11] | data_num[,11] > num_range$max_value[11] )
# 0 line/0 obs, so no outliers

# outliers for list[12] = cec
outliers <- data_num %>% 
  filter(data_num[,12] < num_range$min_value[12] | data_num[,12] > num_range$max_value[12] )
# 3 line/3 obs, so 3 outliers

# outliers for list[13] = N
outliers <- data_num %>% 
  filter(data_num[,13] < num_range$min_value[13] | data_num[,13] > num_range$max_value[13] )
# 11 line/11 obs, so 11 outliers

# outliers for list[14] = P
outliers <- data_num %>% 
  filter(data_num[,14] < num_range$min_value[14] | data_num[,14] > num_range$max_value[14] )
# 31 line/31 obs, so 31 outliers

# outliers for list[15] = as_s
outliers <- data_num %>% 
  filter(data_num[,15] < num_range$min_value[15] | data_num[,15] > num_range$max_value[15] )
# 12 line/12 obs, so 12 outliers

# outliers for list[16] = cd_s
outliers <- data_num %>% 
  filter(data_num[,16] < num_range$min_value[16] | data_num[,16] > num_range$max_value[16] )
# 12 line/12 obs, so 12 outliers
# after verification, the outliers are all NA values of cd_s

# outliers for list[17] = cu_s
outliers <- data_num %>% 
  filter(data_num[,17] < num_range$min_value[17] | data_num[,17] > num_range$max_value[17] )
# 6 line/6 obs, so 6 outliers

# outliers for list[18] = pb_s
outliers <- data_num %>% 
  filter(data_num[,18] < num_range$min_value[18] | data_num[,18] > num_range$max_value[18] )
# 0 line/0 obs, so 0 outliers

# outliers for list[19] = zn_s
outliers <- data_num %>% 
  filter(data_num[,19] < num_range$min_value[19] | data_num[,19] > num_range$max_value[19] )
# 0 line/0 obs, so 0 outliers

# outliers for list[20] = se_s
outliers <- data_num %>% 
  filter(data_num[,20] < num_range$min_value[20] | data_num[,20] > num_range$max_value[20] )
# 0 line/0 obs, so 0 outliers

# outliers for list[21] = ni_s
outliers <- data_num %>% 
  filter(data_num[,21] < num_range$min_value[21] | data_num[,21] > num_range$max_value[21] )
# 0 line/0 obs, so 0 outliers

# outliers for list[22] = co_s
outliers <- data_num %>% 
  filter(data_num[,22] < num_range$min_value[22] | data_num[,22] > num_range$max_value[22] )
# 0 line/0 obs, so 0 outliers

# outliers for list[23] = mn_s
outliers <- data_num %>% 
  filter(data_num[,23] < num_range$min_value[23] | data_num[,23] > num_range$max_value[23] )
# 0 line/0 obs, so 0 outliers

# outliers for list[24] = cr_s
outliers <- data_num %>% 
  filter(data_num[,24] < num_range$min_value[24] | data_num[,24] > num_range$max_value[24] )
# 0 line/0 obs, so 0 outliers

# outliers for list[25] = hg_s
outliers <- data_num %>% 
  filter(data_num[,25] < num_range$min_value[25] | data_num[,25] > num_range$max_value[25] )
# 0 line/0 obs, so 0 outliers

# outliers for list[26] = n_s
outliers <- data_num %>% 
  filter(data_num[,26] < num_range$min_value[26] | data_num[,26] > num_range$max_value[26] )
# 23 line/23 obs, so 23 outliers

# outliers for list[27] = ba_total - on hold for now
# outliers for list[28] = ba_stem - on hold for now
# outliers for list[29] = ba_leaf - on hold for now
# outliers for list[30] = br - on hold for now

# no comparison for outliers for list[31] = br_coarse
# no comparison for outliers for list[32] = br_fine
# no comparison for outliers for list[33] = br_stump

# outliers for list[34] = as_ba
outliers <- data_num %>% 
  filter(data_num[,34] < num_range$min_value[31] | data_num[,34] > num_range$max_value[31] )
# 0 line/0 obs, so 0 outliers

# outliers for list[35] = cd_ba
outliers <- data_num %>% 
  filter(data_num[,35] < num_range$min_value[32] | data_num[,35] > num_range$max_value[32] )
# 3 line/3 obs, so 3 outliers

# outliers for list[36] = cu_ba
outliers <- data_num %>% 
  filter(data_num[,36] < num_range$min_value[33] | data_num[,35] > num_range$max_value[33] )
# 0 line/0 obs, so 0 outliers

# outliers for list[37] = pb_ba
outliers <- data_num %>% 
  filter(data_num[,37] < num_range$min_value[34] | data_num[,37] > num_range$max_value[34] )
# 0 line/0 obs, so 0 outliers

# outliers for list[38] = zn_ba
outliers <- data_num %>% 
  filter(data_num[,38] < num_range$min_value[35] | data_num[,38] > num_range$max_value[35] )
# 2 line/2 obs, so 2 outliers

# outliers for list[39] = se_ba
outliers <- data_num %>% 
  filter(data_num[,39] < num_range$min_value[36] | data_num[,39] > num_range$max_value[36] )
# 0 line/0 obs, so 0 outliers

# outliers for list[40] = ni_ba
outliers <- data_num %>% 
  filter(data_num[,40] < num_range$min_value[37] | data_num[,40] > num_range$max_value[37] )
# 0 line/0 obs, so 0 outliers

# outliers for list[41] = co_ba
outliers <- data_num %>% 
  filter(data_num[,41] < num_range$min_value[38] | data_num[,41] > num_range$max_value[38] )
# 0 line/0 obs, so 0 outliers

# outliers for list[42] = mn_ba
outliers <- data_num %>% 
  filter(data_num[,42] < num_range$min_value[39] | data_num[,42] > num_range$max_value[39] )
# 0 line/0 obs, so 0 outliers

# outliers for list[43] = cr_ba
outliers <- data_num %>% 
  filter(data_num[,43] < num_range$min_value[40] | data_num[,43] > num_range$max_value[40] )
# 0 line/0 obs, so 0 outliers

# outliers for list[44] = hg_ba
outliers <- data_num %>% 
  filter(data_num[,44] < num_range$min_value[41] | data_num[,44] > num_range$max_value[41] )
# 0 line/0 obs, so 0 outliers

# outliers for list[45] = n_te_ba
outliers <- data_num %>% 
  filter(data_num[,45] < num_range$min_value[42] | data_num[,45] > num_range$max_value[42] )
# 0 line/0 obs, so 0 outliers

# outliers for list[46] = as_br
outliers <- data_num %>% 
  filter(data_num[,46] < num_range$min_value[43] | data_num[,46] > num_range$max_value[43] )
# 13 line/13 obs, so 13 outliers

# outliers for list[47] = cd_br
outliers <- data_num %>% 
  filter(data_num[,47] < num_range$min_value[44] | data_num[,47] > num_range$max_value[44] )
# 6 line/6 obs, so 6 outliers

# outliers for list[48] = cu_br
outliers <- data_num %>% 
  filter(data_num[,48] < num_range$min_value[45] | data_num[,48] > num_range$max_value[45] )
# 8 line/8 obs, so 8 outliers

# outliers for list[49] = pb_br
outliers <- data_num %>% 
  filter(data_num[,49] < num_range$min_value[46] | data_num[,49] > num_range$max_value[46] )
# 0 line/0 obs, so 0 outliers

# outliers for list[50] = zn_br
outliers <- data_num %>% 
  filter(data_num[,50] < num_range$min_value[47] | data_num[,50] > num_range$max_value[47] )
# 1 line/1 obs, so 1 outliers

# outliers for list[51] = se_br
outliers <- data_num %>% 
  filter(data_num[,51] < num_range$min_value[48] | data_num[,51] > num_range$max_value[48] )
# 0 line/0 obs, so 0 outliers

# outliers for list[52] = ni_br
outliers <- data_num %>% 
  filter(data_num[,52] < num_range$min_value[49] | data_num[,52] > num_range$max_value[49] )
# 0 line/0 obs, so 0 outliers

# outliers for list[53] = co_br
outliers <- data_num %>% 
  filter(data_num[,53] < num_range$min_value[50] | data_num[,53] > num_range$max_value[50] )
# 0 line/0 obs, so 0 outliers

# outliers for list[54] = mn_br
outliers <- data_num %>% 
  filter(data_num[,54] < num_range$min_value[51] | data_num[,54] > num_range$max_value[51] )
# 0 line/0 obs, so 0 outliers

# outliers for list[55] = cr_br
outliers <- data_num %>% 
  filter(data_num[,55] < num_range$min_value[52] | data_num[,55] > num_range$max_value[52] )
# 0 line/0 obs, so 0 outliers

# outliers for list[56] = hg_br
outliers <- data_num %>% 
  filter(data_num[,56] < num_range$min_value[53] | data_num[,56] > num_range$max_value[53] )
# 0 line/0 obs, so 0 outliers

# outliers for list[57] = n_te_br
outliers <- data_num %>% 
  filter(data_num[,57] < num_range$min_value[54] | data_num[,57] > num_range$max_value[54] )
# 0 line/0 obs, so 0 outliers

# outliers for list[58] = as_br.1
outliers <- data_num %>% 
  filter(data_num[,58] < num_range$min_value[43] | data_num[,58] > num_range$max_value[43] )
# 0 line/0 obs, so 0 outliers

# outliers for list[59] = cd_br.1
outliers <- data_num %>% 
  filter(data_num[,59] < num_range$min_value[44] | data_num[,59] > num_range$max_value[44] )
# 0 line/0 obs, so 0 outliers

# outliers for list[60] = cu_br.1
outliers <- data_num %>% 
  filter(data_num[,60] < num_range$min_value[45] | data_num[,60] > num_range$max_value[45] )
# 0 line/0 obs, so 0 outliers

# outliers for list[61] = pb_br.1
outliers <- data_num %>% 
  filter(data_num[,61] < num_range$min_value[46] | data_num[,61] > num_range$max_value[46] )
# 0 line/0 obs, so 0 outliers

# outliers for list[62] = zn_br.1
outliers <- data_num %>% 
  filter(data_num[,62] < num_range$min_value[47] | data_num[,62] > num_range$max_value[47] )
# 0 line/0 obs, so 0 outliers

# outliers for list[63] = se_br.1
outliers <- data_num %>% 
  filter(data_num[,63] < num_range$min_value[48] | data_num[,63] > num_range$max_value[48] )
# 0 line/0 obs, so 0 outliers

# outliers for list[64] = ni_br.1
outliers <- data_num %>% 
  filter(data_num[,64] < num_range$min_value[49] | data_num[,64] > num_range$max_value[49] )
# 0 line/0 obs, so 0 outliers

# outliers for list[65] = co_br.1
outliers <- data_num %>% 
  filter(data_num[,65] < num_range$min_value[50] | data_num[,65] > num_range$max_value[50] )
# 0 line/0 obs, so 0 outliers

# outliers for list[66] = mn_br.1
outliers <- data_num %>% 
  filter(data_num[,66] < num_range$min_value[51] | data_num[,66] > num_range$max_value[51] )
# 0 line/0 obs, so 0 outliers

# outliers for list[67] = cr_br.1
outliers <- data_num %>% 
  filter(data_num[,67] < num_range$min_value[52] | data_num[,67] > num_range$max_value[52] )
# 0 line/0 obs, so 0 outliers

# outliers for list[68] = hg_br.1
outliers <- data_num %>% 
  filter(data_num[,68] < num_range$min_value[53] | data_num[,68] > num_range$max_value[53] )
# 0 line/0 obs, so 0 outliers

# outliers for list[69] = n_te_br.1
outliers <- data_num %>% 
  filter(data_num[,69] < num_range$min_value[54] | data_num[,69] > num_range$max_value[54] )
# 0 line/0 obs, so 0 outliers

# outliers for list[70] = as_br.2
outliers <- data_num %>% 
  filter(data_num[,70] < num_range$min_value[43] | data_num[,70] > num_range$max_value[43] )
# 0 line/0 obs, so 0 outliers

# outliers for list[71] = cd_br.2
outliers <- data_num %>% 
  filter(data_num[,71] < num_range$min_value[44] | data_num[,71] > num_range$max_value[44] )
# 1 line/1 obs, so 1 outliers
outliers[,71]

# outliers for list[72] = cu_br.2
outliers <- data_num %>% 
  filter(data_num[,72] < num_range$min_value[45] | data_num[,72] > num_range$max_value[45] )
# 0 line/0 obs, so 0 outliers

# outliers for list[73] = pb_br.2
outliers <- data_num %>% 
  filter(data_num[,73] < num_range$min_value[46] | data_num[,73] > num_range$max_value[46] )
# 1 line/1 obs, so 1 outliers
outliers[,73]

# outliers for list[74] = zn_br.2
outliers <- data_num %>% 
  filter(data_num[,74] < num_range$min_value[47] | data_num[,74] > num_range$max_value[47] )
# 1 line/1 obs, so 1 outliers
outliers[,74]

# outliers for list[75] = se_br.2
outliers <- data_num %>% 
  filter(data_num[,75] < num_range$min_value[48] | data_num[,75] > num_range$max_value[48] )
# 0 line/0 obs, so 0 outliers

# outliers for list[76] = ni_br.2
outliers <- data_num %>% 
  filter(data_num[,76] < num_range$min_value[49] | data_num[,76] > num_range$max_value[49] )
# 0 line/0 obs, so 0 outliers

# outliers for list[77] = co_br.2
outliers <- data_num %>% 
  filter(data_num[,77] < num_range$min_value[50] | data_num[,77] > num_range$max_value[50] )
# 0 line/0 obs, so 0 outliers

# outliers for list[78] = mn_br.2
outliers <- data_num %>% 
  filter(data_num[,78] < num_range$min_value[51] | data_num[,78] > num_range$max_value[51] )
# 0 line/0 obs, so 0 outliers

# outliers for list[79] = cr_br.2
outliers <- data_num %>% 
  filter(data_num[,79] < num_range$min_value[52] | data_num[,79] > num_range$max_value[52] )
# 0 line/0 obs, so 0 outliers

# outliers for list[80] = hg_br.2
outliers <- data_num %>% 
  filter(data_num[,80] < num_range$min_value[53] | data_num[,80] > num_range$max_value[53] )
# 0 line/0 obs, so 0 outliers

# outliers for list[81] = n_te_br.2
outliers <- data_num %>% 
  filter(data_num[,81] < num_range$min_value[54] | data_num[,81] > num_range$max_value[54] )
# 0 line/0 obs, so 0 outliers

# outliers for list[82] = as_br.3
outliers <- data_num %>% 
  filter(data_num[,82] < num_range$min_value[43] | data_num[,82] > num_range$max_value[43] )
# 0 line/0 obs, so 0 outliers

# outliers for list[83] = cd_br.3
outliers <- data_num %>% 
  filter(data_num[,83] < num_range$min_value[44] | data_num[,83] > num_range$max_value[44] )
# 0 line/0 obs, so 0 outliers

# outliers for list[84] = cu_br.3
outliers <- data_num %>% 
  filter(data_num[,84] < num_range$min_value[45] | data_num[,84] > num_range$max_value[45] )
# 0 line/0 obs, so 0 outliers

# outliers for list[85] = pb_br.3
outliers <- data_num %>% 
  filter(data_num[,85] < num_range$min_value[46] | data_num[,85] > num_range$max_value[46] )
# 0 line/0 obs, so 0 outliers

# outliers for list[86] = zn_br.3
outliers <- data_num %>% 
  filter(data_num[,86] < num_range$min_value[47] | data_num[,86] > num_range$max_value[47] )
# 0 line/0 obs, so 0 outliers

# outliers for list[87] = se_br.3
outliers <- data_num %>% 
  filter(data_num[,87] < num_range$min_value[48] | data_num[,87] > num_range$max_value[48] )
# 0 line/0 obs, so 0 outliers

# outliers for list[88] = ni_br.3
outliers <- data_num %>% 
  filter(data_num[,88] < num_range$min_value[49] | data_num[,88] > num_range$max_value[49] )
# 0 line/0 obs, so 0 outliers

# outliers for list[89] = co_br.3
outliers <- data_num %>% 
  filter(data_num[,89] < num_range$min_value[50] | data_num[,89] > num_range$max_value[50] )
# 0 line/0 obs, so 0 outliers

# outliers for list[90] = mn_br.3
outliers <- data_num %>% 
  filter(data_num[,90] < num_range$min_value[51] | data_num[,90] > num_range$max_value[51] )
# 0 line/0 obs, so 0 outliers

# outliers for list[91] = cr_br.3
outliers <- data_num %>% 
  filter(data_num[,91] < num_range$min_value[52] | data_num[,91] > num_range$max_value[52] )
# 0 line/0 obs, so 0 outliers

# outliers for list[92] = hg_br.3
outliers <- data_num %>% 
  filter(data_num[,92] < num_range$min_value[53] | data_num[,92] > num_range$max_value[53] )
# 0 line/0 obs, so 0 outliers

# outliers for list[93] = n_te_br.3
outliers <- data_num %>% 
  filter(data_num[,93] < num_range$min_value[54] | data_num[,93] > num_range$max_value[54] )
# 0 line/0 obs, so 0 outliers

# outliers for list[94] = cd_ba.2
outliers <- data_num %>% 
  filter(data_num[,94] < num_range$min_value[55] | data_num[,94] > num_range$max_value[55] )
# 0 line/0 obs, so 0 outliers





