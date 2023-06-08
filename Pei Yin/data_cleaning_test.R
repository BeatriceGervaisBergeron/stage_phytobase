# DATA CLEANING

## LIBRARY

library(dplyr)
library(stringr)
library(taxize)


## remove and reinstall the packages because of error messages:

# Error: package or namespace load failed for ‘dplyr’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), 
# versionCheck = vI[[j]]): namespace ‘vctrs’ 0.4.1 is being loaded, but >= 0.6.0 is required
# In addition: Warning message:
#  package ‘dplyr’ was built under R version 4.2.3 


remove.packages("dplyr")
install.packages("dplyr")
library(dplyr)


#### call your data  ####
data <- read.csv('C:/Users/Pei Yin/PEI_YIN_LI/5-Université/UdeM - sciences bio/Session 2.5 (E-2023)/BIO 2050 (Stage bio computat IRBV)/Analyses_stats_Phytobase/stage_phytobase/Pei Yin/soil_sp_database_Pei_Yin.csv', sep=',',header = T, dec = '.')



#### decimals ####
# make sure there is not comma instead of points
# already with points

# make sure all column are in the correct forms (character or numerical)
str(data)

# transform variable that needed
# Transform data
data <- data %>%
  mutate(
    expe_t = as.numeric(expe_t)
    , ph = as.numeric(ph)
    , clay = as.numeric(clay)
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



#### species names cleaning  ####

# check unique sp list in your database
uni_sp<-as.data.frame(unique(data$name)) # 36 unique species
colnames(uni_sp) <- c('sp')

# Correct the name according to the species name corrected from TRY

# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')

# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 22 species not on the corrected list, so need to be corrected

# Resolve the unmatched name with the 4 databases selected:
# "The International Plant Names Index",'USDA NRCS PLANTS Database',"Tropicos - Missouri Botanical Garden", 'Catalogue of Life'
match.name <- to.be.cor$sp %>%
  gnr_resolve(data_source_ids = c(1,150,165,167), 
              with_canonical_ranks=T)

# provide a short summary table
matches <- match.name %>%
  select(user_supplied_name,submitted_name, matched_name2, score)%>% #add original value
  distinct()

# Are all species considered in the correction
uni_sp_2<- as.data.frame(unique(match.name$user_supplied_name)) # 20 sp, so 2 sp were not taken into account
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

matches.all$implement <- 'TRUE'

matches.all$implement[3] <- 'FALSE'
matches.all$implement[9] <- 'FALSE'
matches.all$implement[10] <- 'FALSE'
matches.all$implement[15] <- 'FALSE'
matches.all$implement[17] <- 'FALSE'
matches.all$implement[21] <- 'FALSE'
matches.all$implement[22] <- 'FALSE'

matches.all$implement[3 & 9 & 10 & 15 & 17 & 21 & 22] <- 'FALSE' # this didn't work and had an error:
# Error in matches.all$implement[3, 9, 10, 15, 17, 21, 22] <- "FALSE" : 
#     incorrect number of subscripts


matches.all$alternative<-''
matches.all$dupl<-''

# write it back as a table for manual correction in Excel
write.table(matches.all,
            "./Pei yin/uni_sp_match_names.txt", 
            sep="\t", row.names = F, quote = F)

# open the txt file in excel to make manual corrections
  # All in () content should be remove
  # For all hybrids (with x) both name should be keep

# Save the corrected names in a txt file name, adding _cor to the name of the document

# import back the data 
uni_sp_cor <- read.table("./Pei yin/uni_sp_match_names_cor.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# eliminate duplicates
uni_sp_cor$dupl2 <- duplicated(uni_sp_cor$user_supplied_name)
uni_sp_cor <-uni_sp_cor %>%  filter(!dupl2 ==T) # no duplicates in the first column

# Save the final corrected list in an rds object
saveRDS(uni_sp_cor, file='Pei yin/salix_sp_cor.rds')

#### Join list of corrected names ####

# call the newly corrected list
salix_sp_cor <- readRDS('Pei yin/salix_sp_cor.rds')

# Add the new corrected list to the complete on
list_sp_cor_salix<- bind_rows( salix_sp_cor, list_sp_cor)

# add the corrections to the data
data <- data %>% 
  left_join(list_sp_cor_salix, by=c('name'= 'user_supplied_name'))
# make a new column of the correct names
data <- data %>% 
  mutate(AccSpeciesName_cor = ifelse(implement == T, alternative, submitted_name)) 
# Keep the corrected column
data <- data[,-c(100:106)] # shoudl have 100 columns

# Check number of sp now
uni_salix_cor <-unique(data$AccSpeciesName_cor)# 33 sp
# 3 species less, if you return to the uni_sp with 36 sp, you see indeed 3 lines with redundant names



#### standardized units ####

# Select all the units column
units <- data %>%
  select(contains('unit'))
# check unit conversion for every unit column
colnames(units)

#"om_units"
unique(units$om_units) # "%" "" 
# no need for conversion if only %

#"oc_units" 
unique(units$oc_units) # ""     "g/kg" "%"  
# need to convert g/Kg to % (/10)
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
unique(units$cec_units) # "meq 100 g−1" "meq 100 g−2" "meq 100 g−3" "cmolc/kg"    "cmolc kg-1"  ""  "mmol kg−1"  
# no need for conversion for all of these:"meq 100 g−1" "meq 100 g−2" "meq 100 g−3" "cmolc/kg", they all mean "cmolc kg-1"
convert <- c("meq 100 g−1","meq 100 g−2","meq 100 g−3","cmolc/kg")
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units %in% convert , 'cmolc kg-1', cec_units)) # replace all g/kg units per %
# need to convert mmol kg−1 to cmolc kg-1 (/10)
data_std <- data_std %>%
  mutate(cec = ifelse(cec_units == 'mmol kg−1', cec/10, cec)) %>% # divide per 10 all the cec data that have g/kg units
  mutate(cec_units = ifelse(cec_units == 'mmol kg−1', 'cmolc kg-1', cec_units)) # replace all g/kg units per %
#verify
unique(data_std$cec_units)# only cmolc kg-1

#"n_units" 
unique(units$n_units) # "%" "" 
# need to convert % to mg kg-1 (*10000)
data_std <- data_std %>%
  mutate(n = ifelse(n_units == '%', n*10000, n)) %>% # multiply per 10000 all the n data that have % units
  mutate(n_units = ifelse(n_units == '%', 'mg kg−1', n_units)) # replace all % per g/kg units 
#verify
unique(data_std$n_units)# only mg kg-1

#"p_units"   
unique(units$p_units) # "%"  ""  "mg kg−1"
# need to convert % to mg kg-1 (*10000)
data_std <- data_std %>%
  mutate(p = ifelse(p_units == '%', p*10000, p)) %>% # multiply per 10000 all the p data that have % units
  mutate(p_units = ifelse(p_units == '%', 'mg kg−1', p_units)) # replace all % per g/kg units 
#verify
unique(data_std$p_units)# only mg kg-1

#"units_s"   
unique(units$units_s) # "mg kg−1"
# no need for conversion if only mg kg−1 

#"units_b" 
unique(units$units_b) # "g m−2"       "g"           ""            "mg kg−1"     "g/plant"     "t ha−1 yr−1"
# the mg mg-1 unit do not make sense, it should be verified for potential mistake
# since biomass is measure really differently, for the moment we'll let it like that
# 'on hold', to be unify if needed further on

#"units_te_ba" 
unique(units$units_te_ba) # "mg kg−1" "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_ba = ifelse(units_te_ba == 'mg Kg-1' , 'mg kg−1', units_te_ba)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_ba) # only "mg kg−1" "" 

#"units_te_br"   
unique(units$units_te_br) # "mg kg−1" "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_br = ifelse(units_te_br == 'mg Kg-1' , 'mg kg−1', units_te_br)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_br) # only "mg kg−1" "" 

#"units_te_ba.1"
unique(units$units_te_ba.1) # "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_ba.1 = ifelse(units_te_ba.1 == 'mg Kg-1' , 'mg kg−1', units_te_ba.1)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_ba.1) # only "mg kg−1" "" 


#### standardize categories terms ####

# Climate
unique(data$climate) 
# 'on hold', to be unify if needed further on

# Texture
unique(data$texture) # NA                "fine sandy loam" "Clay sand silt"  "Loamy"           "Coarse-textured"
# some categories need to be check and adjust as they are not texture
# Clay sand silt, seems like the definition of texture, need to be check in the article
#  "Coarse-textured" is for two classes, need to be check and if not more precise, make a categoty in between for eventual %

# p_density
unique(data$p_density)
# 'on hold', to be unify if needed further on

# organs_ba and organs_ba.1
unique(data_std$organs_ba) # "Shoots" "leaf"   "leaves" "Stems"  "Leaves" NA
unique(data_std$organs_ba.1)# NA       "Stalks" "Twigs"  "Stems" 
# conversion for only shoots or leaves or stems
syn_shoots <- c("Shoots")
syn_stems <- c("Stalks", "Twigs" , "Stems")
syn_leaves <- c("leaf", "Leaves")

data_std <- data_std %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_shoots , 'shoots', organs_ba)) %>% # replace all by shoots 
  mutate(organs_ba = ifelse(organs_ba %in% syn_stems , 'stems', organs_ba)) %>% # replace all by stems
  mutate(organs_ba = ifelse(organs_ba %in% syn_leaves , 'leaves', organs_ba)) %>% # replace all by leaves
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_shoots , 'shoots', organs_ba.1)) %>% # replace all by shoots 
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_stems , 'stems', organs_ba.1)) %>% # replace all by stems
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_leaves , 'leaves', organs_ba.1)) # replace all by leaves

# verify
unique(data_std$organs_ba) # only shoots or leaves or stems
unique(data_std$organs_ba.1) # only stems

# organs_br
# make same thing as for organ_ba



#### outliers and errors in numerical data #### EN CONSTRUCTION

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables means 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # keep only numerical data, so 66 variables

# make a tables of the normal range for all those variables
num_range <- as.data.frame(colnames(data_num))
# add column to enter the ranges
num_range$min_value <- '' 
num_range$max_value <- ''
num_range$sources <- ''

# write it back as a table for manual correction in Excel
write.table(num_range,
            "./Pei yin/numerical_range.txt", 
            sep="\t", row.names = F, quote = F)

# import back the data 
num_range_full <- read.table("./Pei yin/numerical_range_full.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# isolate data that are outside the range # EN CONSTRUCTION

outliers <- data_std %>% 
  select(ph) %>% 
  filter(ph < num_range_full$min_value[num_range_full$variable == 'ph'] | ph > num_range_full$max_value[num_range_full$variable == 'ph'] )

for(i in 1:ncol(data_num)) 
{
  outliers <- data_num %>% 
    select(i) %>% 
    filter(i < num_range_full$min_value[num_range_full$variable == 'i'] | i > num_range_full$max_value[num_range_full$variable == 'i'] )
}  




#### add clay and sand % ###

# call conversion table
# ajuster les % de clay and sand en fonction des % moyen dans ton tableau



