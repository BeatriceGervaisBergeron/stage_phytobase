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
unique(units$P_units) # "mg kg−1"    "" 
# replace 'mg kg−1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(P_units = ifelse(P_units == 'mg kg−1' , 'mg kg-1', P_units))
# verify
unique(data_std$P_units) # only "mg kg-1" "" 

#"units_s"   
unique(units$units_s) # "mg kg−1"
# replace 'mg kg−1' by 'mg kg-1'
data_std <- data_std %>%
  mutate(units_s = ifelse(units_s == 'mg kg−1' , 'mg kg-1', units_s))
# verify
unique(data_std$units_s) # only "mg kg-1" "" 

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

# organs_ba and organs_ba.1
unique(data_std$organs_ba) # 








