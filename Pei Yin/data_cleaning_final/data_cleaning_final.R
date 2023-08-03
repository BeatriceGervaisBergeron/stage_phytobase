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
    , hg_ba.1 = as.numeric(hg_ba.1)
    , covidence = as.numeric(covidence)
    , year = as.numeric(year)
    , n_s = as.numeric(n_s)
    , n_te_ba = as.numeric(n_te_ba)
    , n_te_br = as.numeric(n_te_br)
    , n_te_ba.2 = as.numeric(n_te_ba.2)
    , n_te_ba.1 = as.numeric(n_te_ba.1))
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
data <- data[,-c(117:123)] # it had 124 columns, now it should have 117 columns

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
# Change in further chapter 'Add clay and sand %'

# p_density
unique(data$p_density..ind..m2.or.pots.)
# 'on hold', to be unify if needed further on

# organs_ba, organs_ba.1 and organs_ba.2
unique(data_std$organs_ba) # "leaf"   "leaves"
unique(data_std$organs_ba.1) # "Stalks" "Twigs"  "Stems"
unique(data_std$organs_ba.2) # "Shoots" 
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
unique(data_std$organs_ba) # only leaves
unique(data_std$organs_ba.1) #  only stems 
unique(data_std$organs_ba.2) # only shoots


# organs_br
unique(data_std$organs_br) # "Roots"

# conversion for only roots
syn_roots <- c("Roots")

data_std <- data_std %>%
  mutate(organs_br = ifelse(organs_br %in% syn_roots , 'roots', organs_br)) # replace all by roots
# verify
unique(data_std$organs_br) # "roots"


#### outliers and errors in numerical data ####

# see if duplicates data entries
unique_obs <- data_std[duplicated(data_std)] # 0 variables means 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # keep only numerical data, so 77 variables

# import the data normal range
num_range <- read.table("./numerical_range_variables.txt", 
                        sep="\t", header=T, stringsAsFactors = F)


### to delete lines 366-391: ###

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
    , n_te_ba.2 = as.numeric(n_te_ba.2)
    , n_te_ba.1 = as.numeric(n_te_ba.1))

# verify
str(data_num)

### BEA: why did you did that? Do you need that for the analysis? the 'int' means integer (nombre entier)
# so I do not think you have to change it. If you think so, you should have done it at the very beginning with all the other transformation

### OK I added these as.numeric changes to the beginning (lines 61-70)
## Good, so you can delete this part



# data_num has 64 variables for now
###BEA I have 77 now, change the number to be sure we have the same results

# remove the columns "season_exposure" and "day_exposure", 
# since no data of "min_value" or "max_value" to compare to, in "num_range" (i.e. no outlier)
data_num <- data_num[,-c(6:7)] # should have 75 columns



# check outliers

# for each of the 62 variables, isolate data that are outside the range
# here are the 62 variables
list <-colnames(data_num)
list

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

### BEA: did you went to verified the data?
### Yes, the articles were: 
### no 2560 (oc = 63 g/kg in the article = 6.30%, which is slightly above the max. of 6.0)
### no 671 (oc = 0.8% in the article, which is a lot less than the min. of 2.4%, so I added a note in the journal de bord)
### no 2140 (oc = 1.93% in the article, which is under the min. of 2.4%)
### no 1008 (oc = 2.1% and 1.8% in the article, which are under the min. of 2.4%)

###BEA: OK, you can just let the verified info and delete the comments

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
### it's the article no 63 (cec = 47 meq 100 g−1 in the article = 47 cmolc kg-1)
### it's higher than the range of 2-35 cmolc kg-1

### BEA OK

# outliers for list[13] = N
outliers <- data_num %>% 
  filter(data_num[,13] < num_range$min_value[13] | data_num[,13] > num_range$max_value[13] )
# 11 line/11 obs, so 11 outliers
### there are 2 articles:
### article no 63 (N = 0.18% in the article = 1800 mg/kg, which is a bit higher than the range of 10-1500 mg/kg)
### article no 1645 (N = 0.57% in the article = 5700 mg/kg, which is a lot higher than the range, so I added a note in the journal de bord)

### BEA OK

# outliers for list[14] = P
outliers <- data_num %>% 
  filter(data_num[,14] < num_range$min_value[14] | data_num[,14] > num_range$max_value[14] )
# 31 line/31 obs, so 31 outliers
### there are 3 articles:
### article 2392 & 255 (soil infos are from article 255) (P = 133, 118 and 1042 mg/kg in article 255) 
### (1042 mg/kg is lot higher than the range of 5-100 mg/kg, so I added a note in the journal de bord)
### article 1645 (P = 2829 mg/kg, which is a lot higher than the max range of 100 mg/kg)

### BEA OK

# outliers for list[15] = as_s
outliers <- data_num %>% 
  filter(data_num[,15] < num_range$min_value[15] | data_num[,15] > num_range$max_value[15] )
# 12 line/12 obs, so 12 outliers
### there is one article: no 253 ([As_s] = 1593; 440.9; 561; 949.3; 1119.7 and 1436.1 mg/kg in the article)
### the values are a lot higher than the range of 0-250 mg/kg, so I added a note in the journal de bord)

### BEA OK

# outliers for list[16] = cd_s
outliers <- data_num %>% 
  filter(data_num[,16] < num_range$min_value[16] | data_num[,16] > num_range$max_value[16] )
# 0 line/0 obs, so 0 outliers

# outliers for list[17] = cu_s
outliers <- data_num %>% 
  filter(data_num[,17] < num_range$min_value[17] | data_num[,17] > num_range$max_value[17] )
# 6 line/6 obs, so 6 outliers
### article no 2514 ([Cu_s] = 5162.3 mg/kg in the article,
### which is double the range of 0-2500 mg/kg, so I added a note in the journal de bord)

### BEA OK

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
### there are 4 articles:
### article no 63 (n_s = 32, which is accurate after verification in the article)
### article no 83 (n_s = 50, which is accurate after verification in the article)
### article no 2140 (n_s = 32, which is accurate after verification in the article)
### article no 21 (n_s = 27, which is accurate after verification in the article)

###BEA: are you sure it is the number of replicates and not the total sample size? we wnt replicates here, not total samples

# outliers for list[27] = ba_total - on hold for now
# outliers for list[28] = ba_stem - on hold for now
# outliers for list[29] = ba_leaf - on hold for now
# outliers for list[30] = br - on hold for now

# outliers for list[31] = as_ba
outliers <- data_num %>% 
  filter(data_num[,31] < num_range$min_value[31] | data_num[,31] > num_range$max_value[31] )
# 0 line/0 obs, so 0 outliers

# outliers for list[32] = cd_ba
outliers <- data_num %>% 
  filter(data_num[,32] < num_range$min_value[32] | data_num[,32] > num_range$max_value[32] )
# 3 line/3 obs, so 3 outliers
### there are 2 articles:
### article no 1008 ([Cd_ba] = 163 mg/kg in the article, which is higher than the range of 0-100 mg/kg)
### article no 2514 ([Cd_ba] = 127.4809 and 171.1 mg/kg, which are also higher than the range)
### I added a note in the journal de bord for both articles

### BEA OK

# outliers for list[33] = cu_ba
outliers <- data_num %>% 
  filter(data_num[,33] < num_range$min_value[33] | data_num[,33] > num_range$max_value[33] )
# 0 line/0 obs, so 0 outliers

# outliers for list[34] = pb_ba
outliers <- data_num %>% 
  filter(data_num[,34] < num_range$min_value[34] | data_num[,34] > num_range$max_value[34] )
# 0 line/0 obs, so 0 outliers

# outliers for list[35] = zn_ba
outliers <- data_num %>% 
  filter(data_num[,35] < num_range$min_value[35] | data_num[,35] > num_range$max_value[35] )
# 2 line/2 obs, so 2 outliers
### there are 2 articles:
### article no 1008 ([Zn_ba] = 3991 mg/kg in the article, which is higher than the range of 0-3000 mg/kg)
### article no 607 ([Zn_ba] = 3092 mg/kg in the article, which is slightly higher than the range of 0-3000 mg/kg)
### I added a note in the journal de bord for the value of 3991 mg/kg (article no 1008)

### BEA OK

# outliers for list[36] = se_ba
outliers <- data_num %>% 
  filter(data_num[,36] < num_range$min_value[36] | data_num[,36] > num_range$max_value[36] )
# 0 line/0 obs, so 0 outliers

# outliers for list[37] = ni_ba
outliers <- data_num %>% 
  filter(data_num[,37] < num_range$min_value[37] | data_num[,37] > num_range$max_value[37] )
# 0 line/0 obs, so 0 outliers

# outliers for list[38] = co_ba
outliers <- data_num %>% 
  filter(data_num[,38] < num_range$min_value[38] | data_num[,38] > num_range$max_value[38] )
# 0 line/0 obs, so 0 outliers

# outliers for list[39] = mn_ba
outliers <- data_num %>% 
  filter(data_num[,39] < num_range$min_value[39] | data_num[,39] > num_range$max_value[39] )
# 0 line/0 obs, so 0 outliers

# outliers for list[40] = cr_ba
outliers <- data_num %>% 
  filter(data_num[,40] < num_range$min_value[40] | data_num[,40] > num_range$max_value[40] )
# 0 line/0 obs, so 0 outliers

# outliers for list[41] = hg_ba
outliers <- data_num %>% 
  filter(data_num[,41] < num_range$min_value[41] | data_num[,41] > num_range$max_value[41] )
# 0 line/0 obs, so 0 outliers

# outliers for list[42] = n_te_ba
outliers <- data_num %>% 
  filter(data_num[,42] < num_range$min_value[42] | data_num[,42] > num_range$max_value[42] )
# 0 line/0 obs, so 0 outliers

# outliers for list[43] = as_br
outliers <- data_num %>% 
  filter(data_num[,43] < num_range$min_value[43] | data_num[,43] > num_range$max_value[43] )
# 13 line/13 obs, so 13 outliers
### there is one article: no 253
### [as_br] = from 1150.602 to 4012.195 mg/kg in the article, which are a lot higher than the range of 0-1000 mg/kg
### I added a note in the journal de bord

### BEA OK

# outliers for list[44] = cd_br
outliers <- data_num %>% 
  filter(data_num[,44] < num_range$min_value[44] | data_num[,44] > num_range$max_value[44] )
# 6 line/6 obs, so 6 outliers
### there are 2 articles:
### article no 1008 ([cd_br] = 111 and 128 mg/kg in the article, which are a little higher than the range of 0-100 mg/kg)
### article no 2514 (4 values, [cd_br] = from 101.9653 to 136.3584 mg/kg, which are also a little higher than the range)

### BEA: moi j'ai 7 observations ici. Encore une fois, as-tu vérifié les outliers?
### Pour ma part, j'ai eu 6 observations, mais j'ai bel et bien vérifié les outliers 
### BEA OK j'en ai 6

# outliers for list[45] = cu_br
outliers <- data_num %>% 
  filter(data_num[,45] < num_range$min_value[45] | data_num[,45] > num_range$max_value[45] )
# 8 line/8 obs, so 8 outliers
### there are 2 articles:
### article no 63 ([cu_br] = 1354.32 and 687.35 mg/kg in the article, which are a lot higher than the range of 0-300 mg/kg)
### article no 2514 ([cu_br] = from 1026.316 to 1736.842 mg/kg in the article, which are higher than the range of 0-300 mg/kg)
### I added a note in the journal de bord for both articles

### BEA OK

# outliers for list[46] = pb_br
outliers <- data_num %>% 
  filter(data_num[,46] < num_range$min_value[46] | data_num[,46] > num_range$max_value[46] )
# 0 line/0 obs, so 0 outliers

### BEA: ici j'ai 1 outilers
### Pour ma part, j'ai toujours 0 outlier ici
### BEA OK

# outliers for list[47] = zn_br
outliers <- data_num %>% 
  filter(data_num[,47] < num_range$min_value[47] | data_num[,47] > num_range$max_value[47] )
# 1 line/1 obs, so 1 outliers
### it's article no 1008 ([zn_br] = 3216 mg/kg in the article, which is slightly above the range of 0-3000 mg/kg)

### BEA: ici j'ai 2 outliers
### Pour ma part, j'ai toujours 1 outlier ici
### BEA OK

# outliers for list[48] = se_br
outliers <- data_num %>% 
  filter(data_num[,48] < num_range$min_value[48] | data_num[,48] > num_range$max_value[48] )
# 0 line/0 obs, so 0 outliers

# outliers for list[49] = ni_br
outliers <- data_num %>% 
  filter(data_num[,49] < num_range$min_value[49] | data_num[,49] > num_range$max_value[49] )
# 0 line/0 obs, so 0 outliers

# outliers for list[50] = co_br
outliers <- data_num %>% 
  filter(data_num[,50] < num_range$min_value[50] | data_num[,50] > num_range$max_value[50] )
# 0 line/0 obs, so 0 outliers

# outliers for list[51] = mn_br
outliers <- data_num %>% 
  filter(data_num[,51] < num_range$min_value[51] | data_num[,51] > num_range$max_value[51] )
# 0 line/0 obs, so 0 outliers

# outliers for list[52] = cr_br
outliers <- data_num %>% 
  filter(data_num[,52] < num_range$min_value[52] | data_num[,52] > num_range$max_value[52] )
# 0 line/0 obs, so 0 outliers

# outliers for list[53] = hg_br
outliers <- data_num %>% 
  filter(data_num[,53] < num_range$min_value[53] | data_num[,53] > num_range$max_value[53] )
# 0 line/0 obs, so 0 outliers

# outliers for list[54] = n_te_br
outliers <- data_num %>% 
  filter(data_num[,54] < num_range$min_value[54] | data_num[,54] > num_range$max_value[54] )
# 0 line/0 obs, so 0 outliers

# outliers for list[55] = as_ba.1
outliers <- data_num %>% 
  filter(data_num[,55] < num_range$min_value[31] | data_num[,55] > num_range$max_value[31] )
# 0 line/0 obs, so 0 outliers

# outliers for list[56] = cd_ba.1
outliers <- data_num %>% 
  filter(data_num[,56] < num_range$min_value[32] | data_num[,56] > num_range$max_value[32] )
# 0 line/0 obs, so 0 outliers

# outliers for list[57] = cu_ba.1
outliers <- data_num %>% 
  filter(data_num[,57] < num_range$min_value[33] | data_num[,57] > num_range$max_value[33] )
# 0 line/0 obs, so 0 outliers

# outliers for list[58] = pb_ba.1
outliers <- data_num %>% 
  filter(data_num[,58] < num_range$min_value[34] | data_num[,58] > num_range$max_value[34] )
# 0 line/0 obs, so 0 outliers

# outliers for list[59] = zn_ba.1
outliers <- data_num %>% 
  filter(data_num[,59] < num_range$min_value[35] | data_num[,59] > num_range$max_value[35] )
# 0 line/0 obs, so 0 outliers

# outliers for list[60] = se_ba.1
outliers <- data_num %>% 
  filter(data_num[,60] < num_range$min_value[36] | data_num[,60] > num_range$max_value[36] )
# 0 line/0 obs, so 0 outliers

# outliers for list[61] = ni_ba.1
outliers <- data_num %>% 
  filter(data_num[,61] < num_range$min_value[37] | data_num[,61] > num_range$max_value[37] )
# 0 line/0 obs, so 0 outliers

# outliers for list[62] = co_ba.1
outliers <- data_num %>% 
  filter(data_num[,62] < num_range$min_value[38] | data_num[,62] > num_range$max_value[38] )
# 0 line/0 obs, so 0 outliers

# outliers for list[63] = mn_ba.1
outliers <- data_num %>% 
  filter(data_num[,63] < num_range$min_value[39] | data_num[,63] > num_range$max_value[39] )
# 0 line/0 obs, so 0 outliers

# outliers for list[64] = cr_ba.1
outliers <- data_num %>% 
  filter(data_num[,64] < num_range$min_value[40] | data_num[,64] > num_range$max_value[40] )
# 0 line/0 obs, so 0 outliers

# outliers for list[65] = hg_ba.1
outliers <- data_num %>% 
  filter(data_num[,65] < num_range$min_value[41] | data_num[,65] > num_range$max_value[41] )
# 0 line/0 obs, so 0 outliers

# outliers for list[66] = n_te_ba.1
outliers <- data_num %>% 
  filter(data_num[,66] < num_range$min_value[42] | data_num[,66] > num_range$max_value[42] )
# 0 line/0 obs, so 0 outliers

# outliers for list[67] = as_ba.2
outliers <- data_num %>% 
  filter(data_num[,67] < num_range$min_value[31] | data_num[,67] > num_range$max_value[31] )
# 0 line/0 obs, so 0 outliers

# outliers for list[68] = cd_ba.2
outliers <- data_num %>% 
  filter(data_num[,68] < num_range$min_value[32] | data_num[,68] > num_range$max_value[32] )
# 0 line/0 obs, so 0 outliers

# outliers for list[69] = pb_ba.2
outliers <- data_num %>% 
  filter(data_num[,69] < num_range$min_value[34] | data_num[,69] > num_range$max_value[34] )
# 0 line/0 obs, so 0 outliers

# outliers for list[70] = zn_ba.2
outliers <- data_num %>% 
  filter(data_num[,70] < num_range$min_value[35] | data_num[,70] > num_range$max_value[35] )
# 0 line/0 obs, so 0 outliers

# outliers for list[71] = n_te_ba.2
outliers <- data_num %>% 
  filter(data_num[,71] < num_range$min_value[42] | data_num[,71] > num_range$max_value[42] )
# 0 line/0 obs, so 0 outliers


# if some lines appear, go see the data and verify in the literature if it is a typo, or if it is the exact number from the literature
# if the data still appear high, Write a note in the 'journal de bord'

## Repeat with all the 71 variables


#### Add clay and sand % values according to textural class of soils ####

# check for unique terms in 'texture'
unique(data_std$texture) 
# NA  "fine sandy loam"   "Clay sand silt"   "Loamy"  "Coarse-textured, low content of clay"   "Clay"

# standardize textural terms
data_std <- data_std %>%
  mutate(texture = ifelse(texture == 'fine sandy loam' , 'Sandy loam', texture)) %>% # replace 'fine sandy loam' by 'Sandy loam'
  mutate(texture = ifelse(texture == 'Clay sand silt' , 'Clay', texture)) %>% # replace 'Clay sand silt' by 'Clay'
  mutate(texture = ifelse(texture == 'Loamy' , 'Loam', texture)) %>% # replace 'Loamy' by 'Loam'
  mutate(texture = ifelse(texture == 'Coarse-textured, low content of clay' , 'Coarse texture', texture)) # replace 'Coarse-textured, low content of clay' by 'Coarse texture'

### BEA: why did you replace clay and silt by clay? did you when back to the article? If so, I would replace it by 'silty clay'

### Je suis retourner dans l'article, il mentionne un sol "heavy clay". Donc il contiendrait >40%-50% de clay.
### Je pense que "Clay sand silt" désigne l'ordre decroissant de la quantite de chaque composante. 
### Dans l'article, la texture "Clay sand silt" est pour le sol en surface, et aussi pour le sol a 0-30 cm de profondeur. 
### Pour les profondeurs de 30-60 cm et 60-90 cm du meme sol, la texture est "Clay sand". 
### Probablement qu'en surface, le silt est en faible quantite, et qu'il diminuerait en quantite a mesure 
### que l'on descend en profondeur dans le sol ?
### Si c'est le cas, il y aurait >40-50% de clay, et plus de sand que de silt.
### C'est pour cela que j'ai remplacer "Clay sand silt" par "Clay"

### BEA OK, je voulais simplemt savoir pourquoi. Toutefois, on ne se concentre que sur la couche superieure si l'article fait une distinction de profondeur.
#As-tu extrait les concentrations et info du sol 0-30cm pour cette article?

# verify the conversion worked
unique(data_std$texture)
# NA  "Sandy loam"  "Clay"  "Loam"  "Coarse texture"

# create a backup file in case the conversion of textural classes doesn't work at first
data_std_backup <- data_std

# call conversion table
txt_table <- read.table("./textural_class_average.txt", 
                        sep="\t", header=T, stringsAsFactors = F)

# textural class list
txt_list <- txt_table$texture

# Add the values of 'clay' & 'sand' if needed
data_std_textures <- data_std %>%
  mutate(clay = replace(clay, texture == txt_table$texture[3], txt_table$clay[3])) %>% # replace sandy loam
  mutate(sand = replace(sand, texture == txt_table$texture[3], txt_table$sand[3])) %>% # replace sandy loam
  mutate(clay = replace(clay, texture == txt_table$texture[4], txt_table$clay[4])) %>% # replace loam
  mutate(sand = replace(sand, texture == txt_table$texture[4], txt_table$sand[4])) %>% # replace loam
  # the 'Clay' (12th) line in txt_table should not be used for all the 'data_std_textures' table
  # since article no 2514 (author - Yang) already has 'clay' & 'sand' values, 
  # and also has 'Clay' in texture, we need to keep the article's original 'clay' and 'sand' values
  mutate(clay = replace(clay, texture == txt_table$texture[13], txt_table$clay[13])) %>% # replace Coarse texture
  mutate(sand = replace(sand, texture == txt_table$texture[13], txt_table$sand[13])) %>% # replace Coarse texture
  # only replace the clay & sand values of 'Clay' (12th line) for the article of 'Cicek' (author)
  mutate(clay = replace(clay, author == 'Cicek', txt_table$clay[12])) %>%
  mutate(sand = replace(sand, author == 'Cicek', txt_table$sand[12]))

# now all the textural class should be added in the clay and sand column


#### Add the % in clay_units and sand_units for the clay & sand values that had been added ####

# verify in 'data_std_textures' which lines of clay_units & sand_units need to have "%" unit added
# after verifying, it's the lines 4-18 and 20-87

### BEA: can you find a way to avoid writing all those line to replace a units? Try with the function Filter
### OK I used the replace function for that
##BEA, great

# create a backup file in case the replacement doesn't work at first
data_std_textures_b <- data_std_textures

# Add the % unit if needed
data_std_textures <- data_std_textures %>%
  mutate(clay_units = replace(clay_units, texture == "Sandy loam", "%")) %>% # add "%" unit to clay_units where texture is "Sandy loam"
  mutate(sand_units = replace(sand_units, texture == "Sandy loam", "%")) %>% # add "%" unit to sand_units where texture is "Sandy loam"
  mutate(clay_units = replace(clay_units, texture == "Clay", "%")) %>% # add "%" unit to clay_units where texture is "Clay"
  mutate(sand_units = replace(sand_units, texture == "Clay", "%")) %>% # add "%" unit to sand_units where texture is "Clay"
  mutate(clay_units = replace(clay_units, texture == "Loam", "%")) %>% # add "%" unit to clay_units where texture is "Loam"
  mutate(sand_units = replace(sand_units, texture == "Loam", "%")) %>% # add "%" unit to sand_units where texture is "Loam"
  mutate(clay_units = replace(clay_units, texture == "Coarse texture", "%")) %>% # add "%" unit to clay_units where texture is "Coarse texture"
  mutate(sand_units = replace(sand_units, texture == "Coarse texture", "%")) # add "%" unit to sand_units where texture is "Coarse texture"

# verify for clay_units
unique(data_std_textures$clay_units[4:18]) # "%"
unique(data_std_textures$clay_units[20:87]) # "%"

# verify for sand_units
unique(data_std_textures$sand_units[4:18]) # "%"
unique(data_std_textures$clay_units[20:87]) # "%"


#### Save the final corrected file ####

# Save the final corrected file with textural classes in an rds object
saveRDS(data_std_textures, file = 'Pei Yin/data_cleaning_final/data_std_cleaned.rds')

# save the final corrected file as txt file
write.table(data_std_textures,
            "./Pei Yin/data_cleaning_final/data_std_cleaned.txt", 
            sep="\t", row.names = F, quote = F)

